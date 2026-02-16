#[allow(warnings)]
#[rustfmt::skip]
mod bindings;

use std::{
    collections::{HashMap, hash_map::Entry},
    hash::{DefaultHasher, Hash, Hasher},
    sync::Mutex,
};

use bindings::{Guest, Value};
use lazy_static::lazy_static;
use mexl::{Environment, Object, Program};

lazy_static! {
    static ref PROGRAMS: Mutex<HashMap<u64, Program>> = Mutex::new(HashMap::new());
}

struct Component;

impl Guest for Component {
    fn compile(expr: String) -> Result<u64, String> {
        let program_id = compute_hash(&expr);

        let mut cache = PROGRAMS.lock().unwrap();
        if let Entry::Vacant(e) = cache.entry(program_id) {
            let program = mexl::compile(&expr).map_err(|e| e.to_string())?;
            e.insert(program);
        };

        Ok(program_id)
    }

    fn run(program_id: u64, env_json: String) -> Result<bindings::Value, String> {
        let cache = PROGRAMS.lock().unwrap();
        let program = cache
            .get(&program_id)
            .ok_or_else(|| format!("program not found: {}", program_id))?;

        let env: Environment = serde_json::from_str(&env_json).map_err(|e| e.to_string())?;
        let result = mexl::run(program, &env).map_err(|e| e.to_string())?;

        match result {
            Object::Null => Ok(Value::Null),
            Object::Integer(i) => Ok(Value::Integer(i)),
            Object::Float(f) => Ok(Value::Float(f)),
            Object::String(s) => Ok(Value::String(s)),
            Object::Boolean(b) => Ok(Value::Boolean(b)),
            _ => {
                let s = serde_json::to_string(&result).map_err(|e| e.to_string())?;
                Ok(Value::Json(s))
            }
        }
    }
}

fn compute_hash(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

bindings::export!(Component with_types_in bindings);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_and_run() {
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
            let result = compile_and_run(expr, env);
            assert_eq(result, expected);
        }
    }

    #[test]
    fn test_program_cache() {
        let expr = "1 + 2";
        let env = "{}";

        for _ in 0..10 {
            let result = compile_and_run(expr, env);
            assert_eq(result, Value::Integer(3));
        }
    }

    fn compile_and_run(expr: &str, env: &str) -> Value {
        let program_id = Component::compile(expr.to_string()).unwrap();
        Component::run(program_id, env.to_string()).unwrap()
    }

    fn assert_eq(result: Value, expected: Value) {
        match (result, expected) {
            (Value::Null, Value::Null) => {}
            (Value::Integer(a), Value::Integer(b)) => assert_eq!(a, b),
            (Value::Float(a), Value::Float(b)) => assert!((a - b).abs() < 1e-6),
            (Value::String(a), Value::String(b)) => assert_eq!(a, b),
            (Value::Boolean(a), Value::Boolean(b)) => assert_eq!(a, b),
            (Value::Json(a), Value::Json(b)) => assert_eq!(a, b),
            _ => panic!("type mismatch"),
        }
    }
}
