use crate::{Error, FromBytesOwned, Plugin, ToBytes};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::RwLock;

// `PoolBuilder` is used to configure and create `Pool`s
#[derive(Debug, Clone)]
pub struct PoolBuilder {
    /// Max number of concurrent instances for a plugin - by default this is set to
    /// the output of `std::thread::available_parallelism`
    pub max_instances: usize,
}

impl PoolBuilder {
    /// Create a `PoolBuilder` with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the max number of parallel instances
    pub fn with_max_instances(mut self, n: usize) -> Self {
        self.max_instances = n;
        self
    }

    /// Create a new `Pool` with the given configuration
    pub fn build<F: 'static + Fn() -> Result<Plugin, Error>>(self, source: F) -> Pool {
        Pool::new_from_builder(source, self)
    }
}

impl Default for PoolBuilder {
    fn default() -> Self {
        PoolBuilder {
            max_instances: std::thread::available_parallelism()
                .expect("available parallelism")
                .into(),
        }
    }
}

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

struct PoolInner {
    plugin_source: Box<PluginSource>,
    instances: Vec<PoolPlugin>,
}

unsafe impl Send for PoolInner {}
unsafe impl Sync for PoolInner {}

/// `Pool` manages threadsafe access to a limited number of instances of multiple plugins
#[derive(Clone)]
pub struct Pool {
    config: PoolBuilder,
    inner: Arc<std::sync::Mutex<PoolInner>>,
    existing_functions: Arc<RwLock<HashMap<String, bool>>>,
}

unsafe impl Send for Pool {}
unsafe impl Sync for Pool {}

impl Pool {
    /// Create a new pool with the default configuration
    pub fn new<F: 'static + Fn() -> Result<Plugin, Error>>(source: F) -> Self {
        Self::new_from_builder(Box::new(source), PoolBuilder::default())
    }

    /// Create a new pool configured using a `PoolBuilder`
    pub fn new_from_builder<F: 'static + Fn() -> Result<Plugin, Error>>(
        source: F,
        builder: PoolBuilder,
    ) -> Self {
        Pool {
            config: builder,
            inner: Arc::new(std::sync::Mutex::new(PoolInner {
                plugin_source: Box::new(source),
                instances: Default::default(),
            })),
            existing_functions: RwLock::new(HashMap::default()).into(),
        }
    }

    fn find_available(&self) -> Result<Option<PoolPlugin>, Error> {
        let pool = self.inner.lock().unwrap();
        for instance in pool.instances.iter() {
            if std::rc::Rc::strong_count(&instance.0) == 1 {
                return Ok(Some(instance.clone()));
            }
        }
        Ok(None)
    }

    /// Get the number of live instances for a plugin
    pub fn count(&self) -> usize {
        self.inner.lock().unwrap().instances.len()
    }

    /// Get access to a plugin, this will create a new instance if needed (and allowed by the specified
    /// max_instances). `Ok(None)` is returned if the timeout is reached before an available plugin could be
    /// acquired
    pub fn get(&self, timeout: std::time::Duration) -> Result<Option<PoolPlugin>, Error> {
        let start = std::time::Instant::now();
        let max = self.config.max_instances;
        if let Some(avail) = self.find_available()? {
            return Ok(Some(avail));
        }

        {
            let mut pool = self.inner.lock().unwrap();
            if pool.instances.len() < max {
                let plugin = (*pool.plugin_source)()?;
                let instance = PoolPlugin::new(plugin);
                pool.instances.push(instance);
                return Ok(Some(pool.instances.last().unwrap().clone()));
            }
        }

        loop {
            if let Ok(Some(x)) = self.find_available() {
                return Ok(Some(x));
            }
            if std::time::Instant::now() - start > timeout {
                return Ok(None);
            }

            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }

    /// Access a plugin in a callback function. This calls `Pool::get` then the provided
    /// callback. `Ok(None)` is returned if the timeout is reached before an available
    /// plugin could be acquired
    pub fn with_plugin<T>(
        &self,
        timeout: std::time::Duration,
        f: impl FnOnce(&mut Plugin) -> Result<T, Error>,
    ) -> Result<Option<T>, Error> {
        if let Some(plugin) = self.get(timeout)? {
            return f(&mut plugin.plugin()).map(Some);
        }
        Ok(None)
    }

    /// Returns `true` if the given function exists, otherwise `false`. Results are cached
    /// after the first call.
    pub fn function_exists(&self, name: &str, timeout: std::time::Duration) -> Result<bool, Error> {
        // read current value if any
        let read = self.existing_functions.read().unwrap();
        let exists_opt = read.get(name).cloned();
        drop(read);
        if let Some(exists) = exists_opt {
            Ok(exists)
        } else {
            // load plugin and call function_exists
            let plugin = self.get(timeout)?;
            let exists = plugin.unwrap().0.borrow().function_exists(name);

            // write result to hashmap
            let mut write = self.existing_functions.write().unwrap();
            write.insert(name.to_string(), exists);

            Ok(exists)
        }
    }
}
