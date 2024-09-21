mod common;

use ez_jni::{call, utils::get_string, FromException};
use jni::{objects::{JObject, JString}, JNIEnv};

#[derive(FromException)]
#[class(java.lang.Exception)]
struct MyErr1 {
    // Implicitly calls getMessage()
    message: String,
}

#[derive(FromException)]
#[class(java.lang.Exception)]
struct MyErr2 {
    #[field(call = getMessage)]
    msg: String,
}

#[derive(FromException)]
#[class(java.lang.Exception)]
struct Exception<'local>(#[field(call = getMessage, class = java.lang.String)] JObject<'local>);
impl<'local> Exception<'local> {
    fn message(&self, env: &mut JNIEnv) -> String {
        get_string(JString::from(env.new_local_ref(&self.0).unwrap()), env)
    }
}

#[test]
fn from_exception() {
    setup_env!(env);
    assert_eq!(
        call!(static me.test.Test::throwObj() -> Result<java.lang.Object, MyErr1>)
            .unwrap_err()
            .message,
        "exception"
    );
    assert_eq!(
        call!(static me.test.Test::throwObj() -> Result<java.lang.Object, MyErr2>)
            .unwrap_err()
            .msg,
        "exception"
    );
    assert_eq!(
        call!(static me.test.Test::throwObj() -> Result<java.lang.Object, Exception>)
            .unwrap_err()
            .message(&mut env),
        "exception"
    );
}
