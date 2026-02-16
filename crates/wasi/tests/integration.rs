use std::path::PathBuf;

use wasmtime::{
    Config, Engine, Store,
    component::{Component, Linker, bindgen},
};
use wasmtime_wasi::{ResourceTable, WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

bindgen!({
    path: "wit/mexl-wasi.wit",
    additional_derives: [PartialEq],
});

struct State {
    ctx: WasiCtx,
    table: ResourceTable,
}

impl WasiView for State {
    fn ctx(&mut self) -> wasmtime_wasi::WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.ctx,
            table: &mut self.table,
        }
    }
}

#[test]
fn test_integration() {
    let mut config = Config::new();
    config.wasm_component_model(true);

    let engine = Engine::new(&config).unwrap();

    let mut linker: Linker<State> = Linker::new(&engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker).unwrap();

    let wasi = WasiCtxBuilder::new().inherit_stdio().build();

    let state = State {
        ctx: wasi,
        table: ResourceTable::new(),
    };

    let mut store = Store::new(&engine, state);
    let component = Component::from_file(&engine, get_wasm_path()).unwrap();

    let instance = MexlWasi::instantiate(&mut store, &component, &linker).unwrap();

    let tests = vec![
        ("null", "{}", Value::Null),
        ("1 + 2", "{}", Value::Integer(3)),
        ("3 as float / 2", "{}", Value::Float(1.5)),
        ("x * 2", r#"{"x": 5}"#, Value::Integer(10)),
        (
            r#""ab" + x"#,
            r#"{"x": "c"}"#,
            Value::String("abc".to_string()),
        ),
        ("true && false", "{}", Value::Boolean(false)),
        (
            "x",
            r#"{"x": {"y": true }}"#,
            Value::Json(r#"{"y":true}"#.to_string()),
        ),
    ];

    for (expr, env, expected) in tests {
        let program_id = instance.call_compile(&mut store, expr).unwrap().unwrap();
        let result = instance
            .call_run(&mut store, program_id, env)
            .unwrap()
            .unwrap();

        assert_eq!(result, expected);
    }
}

fn get_wasm_path() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("../../target/wasm32-wasip2/release/mexl_wasi.wasm");
    if !path.exists() {
        panic!(
            "WASM module not found at {:?}. Please build it with `cargo component build --release --target wasm32-wasip2`.",
            path
        );
    }
    path
}
