use std::{
    collections::{HashMap, hash_map::Entry},
    hash::{DefaultHasher, Hash, Hasher},
    sync::Mutex,
};

use lazy_static::lazy_static;
use mexl::{Environment, Object, Program};
use serde::Serialize;

const RESPONSE_SERIALIZE_ERROR: &'static str =
    r#"{"success": false, "error": "failed to serialize response"}"#;

lazy_static! {
    /// A global cache for compiled programs, mapping program IDs to their corresponding instances.
    static ref PROGRAMS: Mutex<HashMap<u64, Program>> = Mutex::new(HashMap::new());
}

/// Represents the response from a compile operation, including success status, caching information, program ID, and error messages if applicable.
#[derive(Debug, Serialize)]
pub struct CompileResponse {
    success: bool,
    cached: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    program_id: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

impl CompileResponse {
    /// Creates a successful compile response.
    pub fn success(program_id: u64, cached: bool) -> Self {
        Self {
            success: true,
            cached,
            program_id: Some(program_id),
            error: None,
        }
    }

    /// Creates an error compile response.
    pub fn error(message: String) -> Self {
        Self {
            success: false,
            cached: false,
            program_id: None,
            error: Some(message),
        }
    }

    /// Converts the compile response to a JSON string.
    pub fn to_json(&self) -> String {
        match serde_json::to_string(self) {
            Ok(json) => json,
            Err(_) => RESPONSE_SERIALIZE_ERROR.to_string(),
        }
    }
}

/// Represents the response from a run operation, including success status, result of the execution, and error messages if applicable.
#[derive(Debug, Serialize)]
pub struct RunResponse {
    success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Object>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

impl RunResponse {
    /// Creates a successful run response.
    pub fn success(result: Object) -> Self {
        Self {
            success: true,
            result: Some(result),
            error: None,
        }
    }

    /// Creates an error run response.
    pub fn error(message: String) -> Self {
        Self {
            success: false,
            result: None,
            error: Some(message),
        }
    }

    /// Converts the run response to a JSON string.
    pub fn to_json(&self) -> String {
        match serde_json::to_string(self) {
            Ok(json) => json,
            Err(_) => RESPONSE_SERIALIZE_ERROR.to_string(),
        }
    }
}

/// Compiles a Mexl expression into a program, returning a response with the program ID and caching information or an error message if compilation fails.
pub fn compile(expr: &str) -> CompileResponse {
    let program_id = compute_hash(expr);

    let mut cache = PROGRAMS.lock().unwrap();
    let mut cached = true;

    if let Entry::Vacant(e) = cache.entry(program_id) {
        match mexl::compile(expr) {
            Ok(program) => {
                e.insert(program);
                cached = false;
            }
            Err(e) => return CompileResponse::error(e.to_string()),
        }
    }

    CompileResponse::success(program_id, cached)
}

/// Executes a compiled program with the given environment, returning the result or an error message.
pub fn run(program_id: u64, env_json: &str) -> RunResponse {
    let cache = PROGRAMS.lock().unwrap();

    let program = match cache.get(&program_id) {
        Some(p) => p,
        None => return RunResponse::error("program not found".to_string()),
    };

    let env = match Environment::from_json_str(env_json) {
        Ok(e) => e,
        Err(e) => return RunResponse::error(format!("failed to parse environment: {}", e)),
    };

    match mexl::run(program, &env) {
        Ok(result) => RunResponse::success(result),
        Err(e) => RunResponse::error(e.to_string()),
    }
}

fn compute_hash(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_response_to_json() {
        let tests = vec![
            (
                CompileResponse::success(1, true),
                r#"{"success":true,"cached":true,"program_id":1}"#,
            ),
            (
                CompileResponse::success(2, false),
                r#"{"success":true,"cached":false,"program_id":2}"#,
            ),
            (
                CompileResponse::error("compilation failed".to_string()),
                r#"{"success":false,"cached":false,"error":"compilation failed"}"#,
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(input.to_json(), expected);
        }
    }

    #[test]
    fn test_run_response_to_json() {
        let tests = vec![
            (
                RunResponse::success(42.into()),
                r#"{"success":true,"result":42}"#,
            ),
            (
                RunResponse::error("execution failed".to_string()),
                r#"{"success":false,"error":"execution failed"}"#,
            ),
        ];

        for (input, expected) in tests {
            assert_eq!(input.to_json(), expected);
        }
    }

    #[test]
    fn test_compile_and_run() {
        let expr = "email ew \"@example.com\"";
        let compile_response = compile(expr);
        assert!(compile_response.success);
        assert!(!compile_response.cached);
        let program_id = compile_response.program_id.unwrap();

        let env_json = r#"{"email": "test@example.com"}"#;
        let run_response = run(program_id, env_json);
        assert!(run_response.success);
        assert!(run_response.result.unwrap() == true.into());
    }

    #[test]
    fn test_compile_caching() {
        let expr = "1 + 1";
        let first_response = compile(expr);
        assert!(first_response.success);
        assert!(!first_response.cached);

        let second_response = compile(expr);
        assert!(second_response.success);
        assert!(second_response.cached);
        assert_eq!(first_response.program_id, second_response.program_id);
    }

    #[test]
    fn test_compile_errors() {
        let expr = "invalid expression";
        let response = compile(expr);
        assert!(!response.success);
        assert!(response.error.is_some());
    }

    #[test]
    fn test_run_invalid_program_id() {
        let invalid_program_id = 9999;
        let env_json = r#"{"key": "value"}"#;
        let response = run(invalid_program_id, env_json);
        assert!(!response.success);
        assert!(response.error.is_some());
    }

    #[test]
    fn test_run_invalid_env() {
        let expr = "1 + 2";
        let compile_response = compile(expr);
        assert!(compile_response.success);
        let program_id = compile_response.program_id.unwrap();
        let invalid_env_json = r#"{"key": "value""#; // malformed JSON
        let response = run(program_id, invalid_env_json);
        assert!(!response.success);
        assert!(response.error.is_some());
    }

    #[test]
    fn test_run_execution_error() {
        let expr = "\"abc\" * 2"; // runtime error
        let compile_response = compile(expr);
        assert!(compile_response.success);
        let program_id = compile_response.program_id.unwrap();
        let env_json = r#"{}"#;
        let response = run(program_id, env_json);
        assert!(!response.success);
        assert!(response.error.is_some());
    }
}
