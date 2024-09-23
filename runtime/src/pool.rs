use std::collections::HashMap;

use crate::{Error, FromBytesOwned, Plugin, PluginBuilder, ToBytes};

/// `PoolPlugin` is used by the pool to track the number of live instances of a particular plugin
#[derive(Clone, Debug)]
pub struct PoolPlugin(std::rc::Rc<std::cell::RefCell<Plugin>>);

impl PoolPlugin {
    fn new(plugin: Plugin) -> Self {
        Self(std::rc::Rc::new(std::cell::RefCell::new(plugin)))
    }

    /// Access the underlying plugin
    pub fn plugin(&self) -> std::cell::RefMut<Plugin> {
        self.0.borrow_mut()
    }

    /// Helper to call a plugin function on the underlying plugin
    pub fn call<'a, Input: ToBytes<'a>, Output: FromBytesOwned>(
        &self,
        name: impl AsRef<str>,
        input: Input,
    ) -> Result<Output, Error> {
        self.plugin().call(name.as_ref(), input)
    }

    /// Helper to get the underlying plugin's ID
    pub fn id(&self) -> uuid::Uuid {
        self.plugin().id
    }
}

type PluginSource = dyn Fn() -> Result<Plugin, Error>;

struct PoolInner<Key: std::fmt::Debug + Clone + std::hash::Hash + Eq = String> {
    plugins: HashMap<Key, Box<PluginSource>>,
    instances: HashMap<Key, Vec<PoolPlugin>>,
}

/// `Pool` manages threadsafe access to a limited number of instances of multiple plugins
#[derive(Clone)]
pub struct Pool<Key: std::fmt::Debug + Clone + std::hash::Hash + Eq = String> {
    max_instances: usize,
    inner: std::sync::Arc<std::sync::Mutex<PoolInner<Key>>>,
}

unsafe impl<T: std::fmt::Debug + Clone + std::hash::Hash + Eq> Send for Pool<T> {}
unsafe impl<T: std::fmt::Debug + Clone + std::hash::Hash + Eq> Sync for Pool<T> {}

impl<Key: std::fmt::Debug + Clone + std::hash::Hash + Eq> Pool<Key> {
    /// Create a new pool
    pub fn new(max_instances: usize) -> Self {
        Pool {
            max_instances,
            inner: std::sync::Arc::new(std::sync::Mutex::new(PoolInner {
                plugins: Default::default(),
                instances: Default::default(),
            })),
        }
    }

    /// Add a plugin using a callback function
    pub fn add<F: 'static + Fn() -> Result<Plugin, Error>>(&self, key: Key, source: F) {
        let mut pool = self.inner.lock().unwrap();
        if !pool.instances.contains_key(&key) {
            pool.instances.insert(key.clone(), vec![]);
        }

        pool.plugins.insert(key, Box::new(source));
    }

    /// Add a plugin using a `PluginBuilder`
    pub fn add_builder(&self, key: Key, source: PluginBuilder<'static>) {
        let mut pool = self.inner.lock().unwrap();
        if !pool.instances.contains_key(&key) {
            pool.instances.insert(key.clone(), vec![]);
        }

        pool.plugins
            .insert(key, Box::new(move || source.clone().build()));
    }

    fn find_available(&self, key: &Key) -> Result<Option<PoolPlugin>, Error> {
        let mut pool = self.inner.lock().unwrap();
        if let Some(entry) = pool.instances.get_mut(key) {
            for instance in entry.iter() {
                if std::rc::Rc::strong_count(&instance.0) == 1 {
                    return Ok(Some(instance.clone()));
                }
            }
        }
        Ok(None)
    }

    /// Get the number of live instances for a plugin
    pub fn count(&self, key: &Key) -> usize {
        self.inner
            .lock()
            .unwrap()
            .instances
            .get(key)
            .map(|x| x.len())
            .unwrap_or_default()
    }

    /// Get access to a plugin, this will create a new instance if needed (and allowed by the specified
    /// max_instances). `Ok(None)` is returned if the timeout is reached before an available plugin could be
    /// acquired
    pub fn get(
        &self,
        key: &Key,
        timeout: std::time::Duration,
    ) -> Result<Option<PoolPlugin>, Error> {
        let start = std::time::Instant::now();
        let max = self.max_instances;
        if let Some(avail) = self.find_available(key)? {
            return Ok(Some(avail));
        }

        {
            let mut pool = self.inner.lock().unwrap();
            if pool.instances.get(key).map(|x| x.len()).unwrap_or_default() < max {
                if let Some(source) = pool.plugins.get(key) {
                    let plugin = source()?;
                    let instance = PoolPlugin::new(plugin);
                    let v = pool.instances.get_mut(key).unwrap();
                    v.push(instance);
                    return Ok(Some(v.last().unwrap().clone()));
                }
            }
        }

        loop {
            if let Ok(Some(x)) = self.find_available(key) {
                return Ok(Some(x));
            }
            if std::time::Instant::now() - start > timeout {
                return Ok(None);
            }
        }
    }

    /// Access a plugin in a callback function. This calls `Pool::get` then the provided
    /// callback. `Ok(None)` is returned if the timeout is reached before an available
    /// plugin could be acquired
    pub fn with_plugin<T>(
        &self,
        key: &Key,
        timeout: std::time::Duration,
        f: impl FnOnce(&mut Plugin) -> Result<T, Error>,
    ) -> Result<Option<T>, Error> {
        if let Some(plugin) = self.get(key, timeout)? {
            return f(&mut plugin.plugin()).map(Some);
        }
        Ok(None)
    }
}
