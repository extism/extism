use crate::*;

#[derive(Debug)]
pub(crate) struct ExtendTimeout {
    duration: std::time::Duration,
    negative: bool,
}

impl From<std::time::Duration> for ExtendTimeout {
    fn from(value: std::time::Duration) -> Self {
        ExtendTimeout {
            duration: value,
            negative: false,
        }
    }
}

impl std::ops::Neg for ExtendTimeout {
    type Output = ExtendTimeout;

    fn neg(mut self) -> Self::Output {
        self.negative = !self.negative;
        self
    }
}

pub(crate) enum TimerAction {
    Start {
        id: uuid::Uuid,
        engine: Engine,
        duration: Option<std::time::Duration>,
    },
    Extend {
        id: uuid::Uuid,
        duration: ExtendTimeout,
    },
    Stop {
        id: uuid::Uuid,
    },
    Cancel {
        id: uuid::Uuid,
    },
    EnterHost {
        id: uuid::Uuid,
    },
    ExitHost {
        id: uuid::Uuid,
    },
    Shutdown,
}

pub(crate) struct Timer {
    pub tx: std::sync::mpsc::Sender<TimerAction>,
    pub thread: Option<std::thread::JoinHandle<()>>,
}

#[cfg(not(target_family = "windows"))]
extern "C" fn cleanup_timer() {
    let mut timer = match unsafe { TIMER.lock() } {
        Ok(x) => x,
        Err(e) => e.into_inner(),
    };
    drop(timer.take());
}

static mut TIMER: std::sync::Mutex<Option<Timer>> = std::sync::Mutex::new(None);

type TimerMap = std::collections::BTreeMap<uuid::Uuid, (Engine, Option<std::time::Instant>)>;

impl Timer {
    pub(crate) fn tx() -> std::sync::mpsc::Sender<TimerAction> {
        let mut timer = match unsafe { TIMER.lock() } {
            Ok(x) => x,
            Err(e) => e.into_inner(),
        };

        let timer = &mut *timer;

        match timer {
            None => Timer::init(timer),
            Some(t) => t.tx.clone(),
        }
    }

    pub fn init(timer: &mut Option<Timer>) -> std::sync::mpsc::Sender<TimerAction> {
        let (tx, rx) = std::sync::mpsc::channel();
        let thread = std::thread::spawn(move || {
            let mut plugins = TimerMap::new();
            let mut in_host = TimerMap::new();

            macro_rules! handle {
                ($x:expr) => {
                    match $x {
                        TimerAction::Start {
                            id,
                            engine,
                            duration,
                        } => {
                            let timeout = duration.map(|x| std::time::Instant::now() + x);
                            trace!(
                                plugin = id.to_string(),
                                "start event with timeout: {:?}",
                                duration
                            );
                            plugins.insert(id, (engine, timeout));
                        }
                        TimerAction::Stop { id } => {
                            trace!(plugin = id.to_string(), "handling stop event");
                            plugins.remove(&id);
                            in_host.remove(&id);
                        }
                        TimerAction::Cancel { id } => {
                            trace!(plugin = id.to_string(), "handling cancel event");
                            if let Some((engine, _)) = plugins.remove(&id) {
                                engine.increment_epoch();
                            }
                            if let Some((engine, _)) = in_host.remove(&id) {
                                engine.increment_epoch();
                            }
                        }
                        TimerAction::Shutdown => {
                            trace!("Shutting down timer");
                            for (id, (engine, _)) in plugins.iter() {
                                trace!(plugin = id.to_string(), "handling shutdown event");
                                engine.increment_epoch();
                            }

                            for (id, (engine, _)) in in_host.iter() {
                                trace!(plugin = id.to_string(), "handling shutdown event");
                                engine.increment_epoch();
                            }
                            return;
                        }
                        TimerAction::Extend { id, duration } => {
                            if let Some((_engine, Some(timeout))) = plugins.get_mut(&id) {
                                let x = if duration.negative {
                                    timeout.checked_sub(duration.duration)
                                } else {
                                    timeout.checked_add(duration.duration)
                                };
                                if let Some(t) = x {
                                    *timeout = t;
                                } else {
                                    error!(
                                        plugin = id.to_string(),
                                        "unable to extend timeout by {:?}", duration.duration
                                    );
                                }
                            }
                        }
                        TimerAction::EnterHost { id } => {
                            trace!(plugin = id.to_string(), "enter host function");
                            if let Some(x) = plugins.remove(&id) {
                                in_host.insert(id, x);
                            }
                        }
                        TimerAction::ExitHost { id } => {
                            trace!(plugin = id.to_string(), "exit host function");
                            if let Some(x) = in_host.remove(&id) {
                                plugins.insert(id, x);
                            }
                        }
                    }
                };
            }

            loop {
                if plugins.is_empty() {
                    if let Ok(x) = rx.recv() {
                        handle!(x)
                    }
                }

                for x in rx.try_iter() {
                    handle!(x)
                }

                plugins = plugins
                    .into_iter()
                    .filter(|(_k, (engine, end))| {
                        if let Some(end) = end {
                            let now = std::time::Instant::now();
                            if end <= &now {
                                engine.increment_epoch();
                                return false;
                            }
                        }
                        true
                    })
                    .collect();
            }
        });
        *timer = Some(Timer {
            thread: Some(thread),
            tx: tx.clone(),
        });
        trace!("Extism timer created");

        #[cfg(not(target_family = "windows"))]
        unsafe {
            libc::atexit(cleanup_timer);
        }

        tx
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        let _ = self.tx.send(TimerAction::Shutdown);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}
