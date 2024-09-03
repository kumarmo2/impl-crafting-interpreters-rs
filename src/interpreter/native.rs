use std::time::{SystemTime, UNIX_EPOCH};

use super::Object;
pub(crate) fn clock(_args: Option<Box<dyn Iterator<Item = Object>>>) -> Object {
    // args will be
    // eventually used to have "native" functions that take parameters.
    let inner_fn = || {
        Object::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Clock may have gone backwards")
                .as_secs_f64(),
        )
    };

    inner_fn()
}
