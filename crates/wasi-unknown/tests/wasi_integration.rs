use serde::Deserialize;
use serde_json::json;
use std::path::PathBuf;
use wasmtime::{Engine, Error, Instance, Module, Store};

#[derive(Debug, Deserialize)]
struct CompileResponse {
    success: bool,
    program_id: Option<u64>,
}

#[derive(Debug, Deserialize)]
struct RunResponse {
    success: bool,
    result: Option<serde_json::Value>,
}

#[test]
fn test_wasi_integration() -> Result<(), Error> {
    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let module = Module::from_file(&engine, get_wasm_path())?;
    let instance = Instance::new(&mut store, &module, &[])?;

    let alloc = instance.get_typed_func::<i32, i32>(&mut store, "alloc")?;
    let dealloc = instance.get_typed_func::<(i32, i32), ()>(&mut store, "dealloc")?;
    let compile = instance.get_typed_func::<(i32, i32), i64>(&mut store, "compile")?;
    let run = instance.get_typed_func::<(i64, i32, i32), i64>(&mut store, "run")?;
    let memory = instance
        .get_memory(&mut store, "memory")
        .expect("failed to get memory");

    // test compile
    let expr = "(1 + 2) * x";
    let bytes = expr.as_bytes();
    let len = bytes.len() as i32;

    let ptr = alloc.call(&mut store, len)?;
    memory.write(&mut store, ptr as usize, bytes)?;
    let res = compile.call(&mut store, (ptr, len))?;
    dealloc.call(&mut store, (ptr, len))?;

    let ptr = (res as u32) as usize;
    let len = (res >> 32) as usize;
    let mut buffer = vec![0u8; len];
    memory.read(&mut store, ptr, &mut buffer)?;
    dealloc.call(&mut store, (ptr as i32, len as i32))?;

    let output = String::from_utf8(buffer)?;
    let compile_response: CompileResponse = serde_json::from_str(&output).unwrap();
    assert!(compile_response.success);
    assert!(compile_response.program_id.is_some());

    // test run
    let env = r#"{"x": 5}"#;
    let bytes = env.as_bytes();
    let len = bytes.len() as i32;

    let ptr = alloc.call(&mut store, len)?;
    memory.write(&mut store, ptr as usize, bytes)?;
    let res = run.call(
        &mut store,
        (compile_response.program_id.unwrap() as i64, ptr, len),
    )?;
    dealloc.call(&mut store, (ptr, len))?;

    let ptr = (res as u32) as usize;
    let len = (res >> 32) as usize;
    let mut buffer = vec![0u8; len];
    memory.read(&mut store, ptr, &mut buffer)?;
    dealloc.call(&mut store, (ptr as i32, len as i32))?;

    let output = String::from_utf8(buffer)?;
    let run_response: RunResponse = serde_json::from_str(&output)?;
    assert!(run_response.success);
    assert_eq!(run_response.result.unwrap(), json!(15));

    Ok(())
}

fn get_wasm_path() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("../../target/wasm32-unknown-unknown/release/mexl_wasi.wasm");
    if !path.exists() {
        panic!(
            "WASM module not found at {:?}. Please build it with `cargo build --release --target wasm32-unknown-unknown`.",
            path
        );
    }
    path
}
