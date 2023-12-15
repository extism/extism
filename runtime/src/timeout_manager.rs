use crate::*;

enum DropBehavior {
    Add,
    Sub,
}

/// `TimeoutManager` is used to control `Plugin` timeouts from within host functions
///
/// It can be used to add or subtract time from a plug-in's timeout. If a plugin is not
/// configured to have a timeout then this will have no effect.
pub struct TimeoutManager {
    start_time: std::time::Instant,
    id: uuid::Uuid,
    tx: std::sync::mpsc::Sender<TimerAction>,
    drop_behavior: Option<DropBehavior>,
}

impl TimeoutManager {
    /// Create a new `TimeoutManager` from the `CurrentPlugin`, this will return `None` if no timeout
    /// is configured
    pub fn new(plugin: &CurrentPlugin) -> Option<TimeoutManager> {
        plugin.manifest.timeout_ms.map(|_| TimeoutManager {
            start_time: std::time::Instant::now(),
            id: plugin.id.clone(),
            tx: Timer::tx(),
            drop_behavior: None,
        })
    }

    /// Add to the configured timeout
    pub fn add(&self, duration: std::time::Duration) -> Result<(), Error> {
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: duration.into(),
        })?;
        Ok(())
    }

    /// Add the amount of time this value has existed to the configured timeout
    pub fn add_elapsed(&mut self) -> Result<(), Error> {
        let d = self.start_time.elapsed();
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: d.into(),
        })?;
        self.start_time = std::time::Instant::now();
        Ok(())
    }

    /// Subtract from the configured timeout
    pub fn sub(&self, duration: std::time::Duration) -> Result<(), Error> {
        let d: timer::ExtendTimeout = duration.into();
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: -d,
        })?;
        Ok(())
    }

    /// Subtract the amount of time this value has existed to the configured timeout
    pub fn sub_elapsed(&mut self) -> Result<(), Error> {
        let d: timer::ExtendTimeout = self.start_time.elapsed().into();
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: -d,
        })?;
        self.start_time = std::time::Instant::now();
        Ok(())
    }

    /// When the `TimeoutManager` is dropped the elapsed duration since it was created will be
    /// added to the plug-in's timeout
    pub fn add_on_drop(mut self) -> Self {
        self.drop_behavior = Some(DropBehavior::Add);
        self
    }

    /// When the `TimeoutManager` is dropped the elapsed duration since it was created will be
    /// added to the plug-in's timeout
    pub fn sub_on_drop(mut self) -> Self {
        self.drop_behavior = Some(DropBehavior::Sub);
        self
    }
}

impl Drop for TimeoutManager {
    fn drop(&mut self) {
        if let Some(b) = &self.drop_behavior {
            let x = match b {
                DropBehavior::Add => self.add_elapsed(),
                DropBehavior::Sub => self.sub_elapsed(),
            };
            if let Err(e) = x {
                error!(
                    plugin = self.id.to_string(),
                    "unable to extend timeout: {}",
                    e.to_string()
                );
            }
        }
    }
}
