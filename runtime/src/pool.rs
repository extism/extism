use dashmap::DashMap;
use extism::{FromBytesOwned, ToBytes};

#[derive(Clone)]
pub struct PoolPlugin {
    inner: std::rc::Rc<std::cell::RefCell<extism::Plugin>>,
}

impl PoolPlugin {
    fn new(plugin: extism::Plugin) -> Self {
        Self {
            inner: std::rc::Rc::new(std::cell::RefCell::new(plugin)),
        }
    }

    pub fn plugin(&self) -> std::cell::RefMut<extism::Plugin> {
        self.inner.borrow_mut()
    }

    pub fn call<'a, Input: ToBytes<'a>, Output: FromBytesOwned>(
        &self,
        name: impl AsRef<str>,
        input: Input,
    ) -> Result<Output, extism::Error> {
        self.plugin().call(name.as_ref(), input)
    }
}

type PluginSource = dyn Fn() -> Result<extism::Plugin, extism::Error>;

struct PoolInner<Key: std::fmt::Debug + Clone + std::hash::Hash + Eq = String> {
    plugins: DashMap<Key, Box<PluginSource>>,
    instances: DashMap<Key, Vec<PoolPlugin>>,
}

#[derive(Clone)]
pub struct Pool<Key: std::fmt::Debug + Clone + std::hash::Hash + Eq = String> {
    max_instances: usize,
    inner: std::sync::Arc<PoolInner<Key>>,
}

unsafe impl<T: std::fmt::Debug + Clone + std::hash::Hash + Eq> Send for Pool<T> {}
unsafe impl<T: std::fmt::Debug + Clone + std::hash::Hash + Eq> Sync for Pool<T> {}

impl<Key: std::fmt::Debug + Clone + std::hash::Hash + Eq> Pool<Key> {
    pub fn new(max_instances: usize) -> Self {
        Pool {
            max_instances,
            inner: std::sync::Arc::new(PoolInner {
                plugins: Default::default(),
                instances: Default::default(),
            }),
        }
    }

    pub fn add<F: Fn() -> Result<extism::Plugin, extism::Error>>(&self, key: Key, source: F)
    where
        F: 'static,
    {
        let pool = &self.inner;
        if !pool.instances.contains_key(&key) {
            pool.instances.insert(key.clone(), vec![]);
        }

        pool.plugins.insert(key, Box::new(source));
    }

    pub fn find_available(&self, key: &Key) -> Result<Option<PoolPlugin>, extism::Error> {
        if let Some(entry) = self.inner.instances.get_mut(key) {
            for instance in entry.iter() {
                if std::rc::Rc::strong_count(&instance.inner) == 1 {
                    return Ok(Some(instance.clone()));
                }
            }
        }
        Ok(None)
    }

    pub fn count(&self, key: &Key) -> usize {
        self.inner
            .instances
            .get(key)
            .map(|x| x.len())
            .unwrap_or_default()
    }

    pub fn get(
        &self,
        key: &Key,
        timeout: std::time::Duration,
    ) -> Result<Option<PoolPlugin>, extism::Error> {
        let start = std::time::Instant::now();
        let max = self.max_instances;
        if let Some(avail) = self.find_available(key)? {
            return Ok(Some(avail));
        }

        if self.inner.instances.get(key).map(|x| x.len()).unwrap_or(0) < max {
            if let Some(source) = self.inner.plugins.get(key) {
                let plugin = source()?;
                let instance = PoolPlugin::new(plugin);
                let mut v = self.inner.instances.get_mut(key).unwrap();
                v.push(instance);
                return Ok(Some(v.last().unwrap().clone()));
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
}
