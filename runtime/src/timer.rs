use crate::*;

pub(crate) enum TimerAction {
    Start {
        id: uuid::Uuid,
        engine: Engine,
        duration: Option<std::time::Duration>,
    },
    Stop {
        id: uuid::Uuid,
    },
    Cancel {
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
    let mut timer = match TIMER.lock() {
        Ok(x) => x,
        Err(e) => e.into_inner(),
    };
    drop(timer.take());
}

static TIMER: std::sync::Mutex<Option<Timer>> = std::sync::Mutex::new(None);

impl Timer {
    pub(crate) fn tx() -> std::sync::mpsc::Sender<TimerAction> {
        let mut timer = match TIMER.lock() {
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
            let mut plugins = std::collections::BTreeMap::new();

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
                        }
                        TimerAction::Cancel { id } => {
                            trace!(plugin = id.to_string(), "handling cancel event");
                            if let Some((engine, _)) = plugins.remove(&id) {
                                engine.increment_epoch();
                            }
                        }
                        TimerAction::Shutdown => {
                            trace!("Shutting down timer");
                            for (id, (engine, _)) in plugins.iter() {
                                trace!(plugin = id.to_string(), "handling shutdown event");
                                engine.increment_epoch();
                            }
                            return;
                        }
                    }
                };
            }

            loop {
                if plugins.is_empty() {
                    if let Ok(x) = rx.recv() {
                        handle!(x);
                    }
                }

                let mut timeout: Option<std::time::Duration> = None;

                plugins.retain(|_k, (engine, end)| {
                    if let Some(end) = end {
                        let now = std::time::Instant::now();
                        if *end <= now {
                            engine.increment_epoch();
                            return false;
                        } else {
                            let time_left =
                                (*end - now).saturating_sub(std::time::Duration::from_millis(1));
                            if let Some(t) = &timeout {
                                if time_left < *t {
                                    timeout = Some(time_left);
                                }
                            } else {
                                timeout = Some(time_left);
                            }
                        }
                    }

                    true
                });

                if let Some(timeout) = timeout {
                    if let Ok(x) = rx.recv_timeout(timeout) {
                        handle!(x)
                    }
                } else if let Ok(x) = rx.recv() {
                    handle!(x)
                }
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
