use std::collections::HashMap;

use crate::{object::{Function, NativeFn, Object}};

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Object>
}

impl Environment {
    pub fn define_function(&mut self, name: &str, handler: NativeFn) {
        let obj = Object::Function(Function {
            name: name.to_owned(),
            handler,
        });
        self.set(name, obj);
    }

    pub fn set(&mut self, key: &str, value: Object) {
        self.values.insert(key.to_owned(), value);
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        let obj = self.values.get(key)?;
        Some(obj.clone())
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self { values: HashMap::new() }
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
}