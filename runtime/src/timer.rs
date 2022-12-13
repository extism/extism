use crate::*;

pub(crate) enum TimerAction {
    Start {
        id: uuid::Uuid,
        engine: Engine,
        duration: std::time::Duration,
    },
    Stop {
        id: uuid::Uuid,
    },
    Shutdown,
}

pub(crate) struct Timer {
    pub tx: std::sync::mpsc::SyncSender<TimerAction>,
    pub thread: Option<std::thread::JoinHandle<()>>,
}

impl Timer {
    pub fn init(timer: &mut Option<Timer>) -> std::sync::mpsc::SyncSender<TimerAction> {
        let (tx, rx) = std::sync::mpsc::sync_channel(128);
        let thread = std::thread::spawn(move || {
            let mut plugins = std::collections::BTreeMap::new();

            loop {
                for x in rx.try_iter() {
                    match x {
                        TimerAction::Start {
                            id,
                            engine,
                            duration,
                        } => {
                            plugins.insert(id, (engine, std::time::Instant::now() + duration));
                        }
                        TimerAction::Stop { id } => {
                            plugins.remove(&id);
                        }
                        TimerAction::Shutdown => return,
                    }
                }

                if plugins.is_empty() {
                    continue;
                }

                plugins = plugins
                    .into_iter()
                    .filter(|(_k, (engine, end))| {
                        let now = std::time::Instant::now();
                        if end <= &now {
                            engine.increment_epoch();
                            return false;
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
