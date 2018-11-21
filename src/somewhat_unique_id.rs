use std::{thread, thread::ThreadId, cell::Cell};

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub(crate) struct UID {
    tid: ThreadId,
    num: u64
}
impl UID {
    pub(crate) fn new() -> UID {
        TID.with(|&tid| {
            COUNTER.with(|ctr| {
                let num = ctr.get();
                ctr.set(num + 1);
                UID {
                    tid, num
                }
            })
        })
    }
}
thread_local! {
    pub static TID: ThreadId = thread::current().id();
    pub static COUNTER: Cell<u64> = Cell::new(0);
}
