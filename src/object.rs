use std::{collections::HashMap, fmt};

pub type NativeFn = fn(Vec<Object>) -> Result<Object, String>;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub handler: NativeFn,
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool { 
        self.name == other.name
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Array(Vec<Object>),
    Map(HashMap<String, Object>),
    Function(Function),
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}

impl Object {
    pub fn default_integer() -> Object {
        Object::Integer(0)
    }

    pub fn default_float() -> Object {
        Object::Float(0_f64)
    }

    pub fn default_string() -> Object {
        Object::String("".into())
    }

    pub fn default_boolean() -> Object {
        Object::Boolean(false)
    }
}

pub fn cast_to_integer(obj: Object) -> Result<Object, String> {
    match obj {
        Object::Null => Ok(Object::default_integer()),
        Object::Integer(_) => Ok(obj),
        Object::Float(f) => Ok((f as i64).into()),
        Object::String(s) => s.parse::<i64>().
            map(Object::Integer).
            map_err(|_| format!("cannot cast string '{}' to integer", s)),
        _ => Err(format!("cannot cast {} to integer", obj))
    }
}

pub fn cast_to_float(obj: Object) -> Result<Object, String> {
    match obj {
        Object::Null => Ok(Object::default_float()),
        Object::Integer(i) => Ok((i as f64).into()),
        Object::Float(_) => Ok(obj),
        Object::String(s) => s.parse::<f64>().
            map(Object::Float).
            map_err(|_| format!("cannot cast string '{}' to float", s)),
        _ => Err(format!("cannot cast {} to float", obj))
    }
}

pub fn cast_to_string(obj: Object) -> Result<Object, String> {
    match obj {
        Object::Null => Ok(Object::default_string()),
        Object::Integer(i) => Ok(i.to_string().into()),
        Object::Float(f) => Ok(f.to_string().into()),
        Object::String(_) => Ok(obj),
        Object::Boolean(b) => Ok(b.to_string().into()),
        _ => Err(format!("cannot cast {} to string", obj))
    }
}

pub fn cast_to_boolean(obj: Object) -> Result<Object, String> {
    if matches!(obj, Object::Function(_)) {
        return Err(format!("cannot cast {} to boolean", obj));
    }
    Ok(is_truthy(&obj).into())
}

pub fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Integer(i) => if *i == 0 { false } else { true },
        Object::Float(f) => if *f == 0.0 { false } else { true },
        Object::String(s) => if s.as_str() == "" { false } else { true },
        Object::Boolean(b) => *b,
        Object::Array(a) => if a.len() == 0 { false } else { true },
        Object::Map(m) => if m.len() == 0 { false } else { true },
        Object::Function(_) => { true },
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Object::Integer(v) => write!(f, "{}", v),
            Object::Float(v) => write!(f, "{}", v),
            Object::String(v) => write!(f, "\"{}\"", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Array(v) => {
                write!(f, "[")?;
                for (i, obj) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;                    
                    }
                    write!(f, "{}", obj)?;
                }
                write!(f, "]")
            },
            Object::Map(v) => {
                write!(f, "{{")?;
                for (i, (k, v)) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;                    
                    }
                    write!(f, "\"{}\": {}", k, v)?;
                }
                write!(f, "}}")
            }
            Object::Null => write!(f, "null"),
            Object::Function(v) => write!(f, "fn:{}", v.name),
        }
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object::Integer(value)
    }
}

impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Object::Float(value)
    }
}

impl From<&str> for Object {
    fn from(value: &str) -> Self {
        Object::String(value.to_owned())
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Object::String(value)
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object::Boolean(value)
    }
}

impl From<Vec<Object>> for Object {
    fn from(value: Vec<Object>) -> Self {
        Object::Array(value)
    }
}

impl From<HashMap<String, Object>> for Object {
    fn from(value: HashMap<String, Object>) -> Self {
        Object::Map(value)
    }
}

pub fn unify_operands(left: Object, right: Object) -> (Object, Object) {
    match (&left, &right) {
        (Object::Integer(_), Object::Null) => (left, Object::default_integer()),
        (Object::Null, Object::Integer(_)) => (Object::default_integer(), right),
        (Object::Float(_), Object::Null) => (left, Object::default_float()),
        (Object::Null, Object::Float(_)) => (Object::default_float(), right),
        (Object::String(_), Object::Null) => (left, Object::default_string()),
        (Object::Null, Object::String(_)) => (Object::default_string(), right),
        (Object::Boolean(_), Object::Null) => (left, Object::default_boolean()),
        (Object::Null, Object::Boolean(_)) => (Object::default_boolean(), right),        
        (Object::Integer(l), Object::Float(_)) => (Object::Float(*l as f64), right),
        (Object::Float(_), Object::Integer(r)) => (left, Object::Float(*r as f64)),
        _ => (left, right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_object_defaults() {
        assert_eq!(Object::default(), Object::Null);
        assert_eq!(Object::default_integer(), Object::Integer(0));
        assert_eq!(Object::default_float(), Object::Float(0.0));
        assert_eq!(Object::default_string(), Object::String("".into()));
        assert_eq!(Object::default_boolean(), Object::Boolean(false));
    }

    #[test]
    fn test_cast_to_integer() {
        let tests: Vec<(Object, Result<Object, String>)> = vec![
            (Object::Null, Ok(0.into())),
            (1.into(), Ok(1.into())),
            (1.5.into(), Ok(1.into())),
            ("1".into(), Ok(1.into())),
            ("a".into(), Err("".into())),
            (true.into(), Err("".into())),
        ];

        for (input, expected) in tests {
            match cast_to_integer(input) {
                Ok(actual) => { assert_eq!(actual, expected.unwrap()); }
                Err(_) => { assert!(expected.is_err())},
            }
        }
    }

    #[test]
    fn test_cast_to_float() {
        let tests: Vec<(Object, Result<Object, String>)> = vec![
            (Object::Null, Ok(0.0.into())),
            (1.into(), Ok(1.0.into())),
            (1.5.into(), Ok(1.5.into())),
            ("1.5".into(), Ok(1.5.into())),
            ("a".into(), Err("".into())),
            (true.into(), Err("".into())),
        ];

        for (input, expected) in tests {
            match cast_to_float(input) {
                Ok(actual) => { assert_eq!(actual, expected.unwrap()); }
                Err(_) => { assert!(expected.is_err())},
            }
        }
    }

    #[test]
    fn test_cast_to_string() {
        let tests: Vec<(Object, Result<Object, String>)> = vec![
            (Object::Null, Ok("".into())),
            (1.into(), Ok("1".into())),
            (1.5.into(), Ok("1.5".into())),
            ("1.5".into(), Ok("1.5".into())),
            (true.into(), Ok("true".into())),
            (vec![].into(), Err("".into())),
        ];

        for (input, expected) in tests {
            match cast_to_string(input) {
                Ok(actual) => { assert_eq!(actual, expected.unwrap()); }
                Err(_) => { assert!(expected.is_err())},
            }
        }
    }

    #[test]
    fn test_cast_to_boolean() {
        let tests: Vec<(Object, Result<Object, String>)> = vec![
            (1.into(), Ok(true.into())),
            (0.0.into(), Ok(false.into())),
            (Object::Function(Function { 
                name: "fn".into(), 
                handler: |_| unreachable!(),
             }), Err("".into()))
        ];

        for (input, expected) in tests {
            match cast_to_boolean(input) {
                Ok(actual) => { assert_eq!(actual, expected.unwrap()); }
                Err(_) => { assert!(expected.is_err())},
            }
        }
    }

    #[test]
    fn test_object_display() {
        let tests: Vec<(Object, &str)> = vec![
            (Object::Integer(1), "1"),
            (Object::Float(1.5), "1.5"),
            (Object::String("abc".into()), r#""abc""#),
            (Object::Boolean(true), "true"),
            (Object::Boolean(false), "false"),
            (vec![
                1.into(), 1.5.into(), "abc".into(), true.into(), Object::Null
            ].into(), r#"[1, 1.5, "abc", true, null]"#),
            (HashMap::from([
                ("x".into(), HashMap::from([
                    ("y".into(), true.into())
                ]).into())
            ]).into(), r#"{"x": {"y": true}}"#),
            (Object::Null, "null"),
            (Object::Function(Function { 
                name: "test".to_owned(),
                handler: |_| unreachable!(),
            }), "fn:test"),
        ];

        for (input, expected) in tests {
            let actual = format!("{}", input);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_is_truthy() {
        let tests: Vec<(Object, bool)> = vec![
            (Object::Null, false),
            ((-1).into(), true),
            (1.into(), true),
            (0.into(), false),
            ((-0.1).into(), true),
            (0.1.into(), true),
            (0.0.into(), false),
            ("abc".into(), true),
            ("".into(), false),
            (true.into(), true),
            (false.into(), false),
            (vec![1.into()].into(), true),
            (vec![].into(), false),
            (HashMap::from([("key".to_owned(), 1.into())]).into(), true),
            (HashMap::from([]).into(), false),
            (Object::Function(Function { 
                name: "fn".into(), 
                handler: |_| unreachable!(),
            }), true),
        ];

        for (input, expected) in tests {
            let actual = is_truthy(&input);
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_unify_operands() {
        struct TestCase {
            input_left: Object,
            input_right: Object,
            expected_left: Object,
            expected_right: Object,
        }

        let tests = vec![
            TestCase { input_left: 1.into(), input_right: Object::Null, expected_left: 1.into(), expected_right: 0.into() },
            TestCase { input_left: Object::Null, input_right: 1.into(), expected_left: 0.into(), expected_right: 1.into() },
            TestCase { input_left: 1.5.into(), input_right: Object::Null, expected_left: 1.5.into(), expected_right: 0.0.into() },
            TestCase { input_left: Object::Null, input_right: 1.5.into(), expected_left: 0.0.into(), expected_right: 1.5.into() },
            TestCase { input_left: "a".into(), input_right: Object::Null, expected_left: "a".into(), expected_right: "".into() },
            TestCase { input_left: Object::Null, input_right: "a".into(), expected_left: "".into(), expected_right: "a".into() },
            TestCase { input_left: true.into(), input_right: Object::Null, expected_left: true.into(), expected_right: false.into() },
            TestCase { input_left: Object::Null, input_right: true.into(), expected_left: false.into(), expected_right: true.into() },
            TestCase { input_left: 1.into(), input_right: 1.5.into(), expected_left: 1.0.into(), expected_right: 1.5.into() },
            TestCase { input_left: 1.5.into(), input_right: 1.into(), expected_left: 1.5.into(), expected_right: 1.0.into() },
        ];

        for test in tests {
            let (actual_left, actual_right) = unify_operands(test.input_left, test.input_right);
            assert_eq!(actual_left, test.expected_left);
            assert_eq!(actual_right, test.expected_right);
        }
    }

    #[test]
    fn test_function_eq() {
        let handler: NativeFn = |_| Err("error".into());

        let fn1a = Object::Function(Function { 
            name: "fn1".to_owned(), 
            handler,
        });

         let fn1b = Object::Function(Function { 
            name: "fn1".to_owned(), 
            handler,
        });

         let fn2 = Object::Function(Function { 
            name: "fn2".to_owned(), 
            handler,
        });

        assert_eq!(fn1a, fn1b);
        assert_ne!(fn1a, fn2);
    }
}