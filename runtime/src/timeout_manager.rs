use crate::*;

/// `TimeoutManager` is used to control `Plugin` timeouts from within host functions
///
/// It can be used to add or subtract time from a plug-in's timeout. If a plugin is not
/// configured to have a timeout then this will have no effect.
pub(crate) struct TimeoutManager {
    start_time: std::time::Instant,
    id: uuid::Uuid,
    tx: std::sync::mpsc::Sender<TimerAction>,
    cost: f64,
}

impl TimeoutManager {
    /// Create a new `TimeoutManager` from the `CurrentPlugin`, this will return `None` if no timeout
    /// is configured
    pub fn new(plugin: &CurrentPlugin) -> Option<TimeoutManager> {
        plugin.manifest.timeout_ms.map(|_| TimeoutManager {
            start_time: std::time::Instant::now(),
            id: plugin.id.clone(),
            tx: Timer::tx(),
            cost: 0.0,
        })
    }

    /// Add the amount of time this value has existed to the configured timeout
    pub fn add_elapsed(&mut self) -> Result<(), Error> {
        let cost = self.cost.abs();
        let d = self.start_time.elapsed().mul_f64(cost);
        let mut d: timer::ExtendTimeout = d.into();
        if self.cost.is_sign_negative() {
            d = -d;
        }
        self.tx.send(TimerAction::Extend {
            id: self.id.clone(),
            duration: d,
        })?;
        self.start_time = std::time::Instant::now();
        Ok(())
    }

    pub fn with_cost(mut self, cost: f64) -> Self {
        self.cost = cost;
        self
    }
}

impl Drop for TimeoutManager {
    fn drop(&mut self) {
        let x = self.add_elapsed();
        if let Err(e) = x {
            error!(
                plugin = self.id.to_string(),
                "unable to extend timeout: {}",
                e.to_string()
            );
        }
    }
}
