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
    cost: f64,
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
            cost: 1.0,
            drop_behavior: None,
        })
    }

    /// Add to the configured timeout
    pub fn add(&self, duration: std::time::Duration) -> Result<(), Error> {
        let d = duration.mul_f64(self.cost);
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: d.into(),
        })?;
        Ok(())
    }

    /// Subtract from the configured timeout
    pub fn sub(&self, duration: std::time::Duration) -> Result<(), Error> {
        let d: timer::ExtendTimeout = duration.mul_f64(self.cost).into();
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: -d,
        })?;
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

    /// Adjust the cost of added/subtracted values, this will scale all durations
    /// submitted to this manager by the provided factor.
    pub fn cost(mut self, cost: f64) -> Self {
        self.cost = cost;
        self
    }
}

impl Drop for TimeoutManager {
    fn drop(&mut self) {
        if let Some(b) = &self.drop_behavior {
            let duration = self.start_time.elapsed();
            let x = match b {
                DropBehavior::Add => self.add(duration),
                DropBehavior::Sub => self.sub(duration),
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
