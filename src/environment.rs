use std::collections::HashMap;

use crate::object::{Function, NativeFn, Object};

/// Represents the environment that holds variable and function bindings.
#[derive(Debug, Clone, Default)]
pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    /// Defines a native function in the environment.
    pub fn define_function(&mut self, name: &str, handler: NativeFn) {
        self.values.insert(
            name.to_owned(),
            Object::Function(Function {
                name: name.to_owned(),
                handler,
            }),
        );
    }

    /// Sets a variable in the environment.
    pub fn set(&mut self, key: &str, value: Object) {
        self.values.insert(key.to_owned(), value);
    }

    /// Gets a variable or function from the environment.
    pub fn get(&self, key: &str) -> Option<Object> {
        self.values.get(key).cloned()
    }
}

#[cfg(feature = "serde")]
impl Environment {
    pub fn from_json(json_str: &str) -> Result<Environment, String> {
        let mut env = Environment::default();
        let v: serde_json::Value = serde_json::from_str(json_str).map_err(|e| e.to_string())?;

        if let serde_json::Value::Object(map) = v {
            for (key, value) in map {
                env.set(&key, Object::from(value));
            }
            Ok(env)
        } else {
            Err("JSON root must be an object".to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_environment_define_function() {
        let mut env = Environment::default();
        env.define_function("test", |args| Ok(args[0].clone()));

        let expected: Object = 1.into();
        let actual = match env.get("test").unwrap() {
            Object::Function(f) => (f.handler)(vec![expected.clone()]),
            _ => unimplemented!(),
        };

        assert_eq!(actual.unwrap(), expected);
    }

    #[test]
    fn test_environment_set() {
        let mut env = Environment::default();
        let expected = Object::Integer(1);
        env.set("key", expected.clone());
        let actual = env.get("key").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_environment_get() {
        let env = Environment::default();
        let actual = env.get("key");
        assert_eq!(actual.is_none(), true);
    }

    #[test]
    fn test_environment_from_json() {
        let json_str = r#"
    {
        "name": "test_user",
        "age": 30,
        "is_active": true,
        "roles": ["user", "admin"]
    }"#;

        let env = Environment::from_json(json_str).unwrap();

        assert_eq!(env.get("name").unwrap(), "test_user".into());
        assert_eq!(env.get("age").unwrap(), 30.into());
        assert_eq!(env.get("is_active").unwrap(), true.into());
        assert_eq!(
            env.get("roles").unwrap(),
            vec!["user".into(), "admin".into()].into()
        );
    }

    #[test]
    fn test_environment_from_json_error_cases() {
        for json_str in &["null", "[]", "{"] {
            let result = Environment::from_json(json_str);
            assert!(result.is_err());
        }
    }
}
