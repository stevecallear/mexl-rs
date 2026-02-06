use std::{
    alloc::{self, Layout},
    slice, str,
};

use crate::module::{CompileResponse, RunResponse};

mod module;

/// Compile the expression at the specified memory pointer.
#[allow(clippy::missing_safety_doc)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn compile(ptr: *mut u8, len: usize) -> u64 {
    unsafe {
        let expr_bytes = slice::from_raw_parts(ptr, len);
        let expr_str = match str::from_utf8(expr_bytes) {
            Ok(s) => s,
            Err(e) => return pack_string(CompileResponse::error(e.to_string()).to_json()),
        };

        let response = module::compile(expr_str);
        pack_string(response.to_json())
    }
}

/// Run the compiled program, using the environment JSON at the specified memory pointer.
#[allow(clippy::missing_safety_doc)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn run(program_id: u64, env_ptr: *mut u8, env_len: usize) -> u64 {
    unsafe {
        let env_bytes = slice::from_raw_parts(env_ptr, env_len);
        let env_str = match str::from_utf8(env_bytes) {
            Ok(s) => s,
            Err(e) => return pack_string(RunResponse::error(e.to_string()).to_json()),
        };

        let response = module::run(program_id, env_str);
        pack_string(response.to_json())
    }
}

/// Allocate memory.
#[allow(clippy::missing_safety_doc)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn alloc(size: usize) -> *mut u8 {
    let layout = get_u8_layout(size);
    unsafe {
        let ptr = alloc::alloc(layout);
        if ptr.is_null() {
            alloc::handle_alloc_error(layout);
        }
        ptr
    }
}

/// Deallocate the specified memory.
#[allow(clippy::missing_safety_doc)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn dealloc(ptr: *mut u8, size: usize) {
    let layout = get_u8_layout(size);
    unsafe {
        alloc::dealloc(ptr, layout);
    }
}

// Return the allocation layout for the size.
fn get_u8_layout(size: usize) -> std::alloc::Layout {
    Layout::array::<u8>(size).unwrap()
}

// Pack a pointer and length u32 to the supplied string.
fn pack_string(mut s: String) -> u64 {
    s.shrink_to_fit();

    let mut output = s.into_bytes();
    output.shrink_to_fit();

    let ptr = output.as_mut_ptr();
    let len = output.len();
    std::mem::forget(output);

    ((len as u64) << 32) | (ptr as u64)
}
