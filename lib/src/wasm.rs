use std::alloc::{alloc, Layout};
use std::io::Cursor;

pub const DEFAULT: &str = include_str!("../include/default.midilisp");

type WasmResult = Result<Vec<u8>, Vec<u8>>;

#[export_name = "malloc"]
pub unsafe extern "C" fn malloc(n: usize) -> *mut u8 {
    assert_ne!(n, 0);
    alloc(Layout::array::<u8>(n).expect("overflow"))
}

#[export_name = "getVecPtr"]
pub unsafe extern "C" fn get_vec_ptr(ptr: *const Vec<u8>) -> *const u8 {
    ptr.as_ref().expect("null ptr").as_ptr()
}

#[export_name = "getVecLen"]
pub unsafe extern "C" fn get_vec_len(ptr: *const Vec<u8>) -> usize {
    ptr.as_ref().expect("null ptr").len()
}

#[export_name = "isOk"]
pub unsafe extern "C" fn is_ok(ptr: *const WasmResult) -> i32 {
    ptr.as_ref().expect("null ptr").is_ok() as i32
}

#[export_name = "unwrap"]
pub unsafe extern "C" fn unwrap(ptr: *const WasmResult) -> *const Vec<u8> {
    match ptr.as_ref().expect("null ptr") {
        Ok(v) => v,
        Err(v) => v,
    }
}

#[export_name = "run"]
pub extern "C" fn run(ptr: *const u8, len: usize) -> *const WasmResult {
    assert!(!ptr.is_null());
    assert_ne!(len, 0);
    assert!(len <= isize::MAX as usize);

    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };

    let result = match std::str::from_utf8(slice) {
        Ok(src) => call_midilisp(src),
        Err(e) => Err(e.to_string().into_bytes()),
    };

    Box::into_raw(Box::new(result))
}

fn call_midilisp(src: &str) -> WasmResult {
    let mut w = Cursor::new(Vec::new());

    match crate::run(&mut w, src) {
        Ok(retval) => {
            let vec = w.into_inner();

            if !vec.is_empty() {
                Ok(vec)
            } else {
                Err(format!("Ok: {}", retval).into_bytes())
            }
        }
        Err(e) => Err(format!("Err: {}", e).into_bytes()),
    }
}
