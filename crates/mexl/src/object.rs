use std::{collections::HashMap, fmt};

use crate::MexlError;

/// Represents a native function that can be invoked by the VM.
pub type NativeFn = fn(Vec<Object>) -> Result<Object, MexlError>;

/// Represents a function object.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub handler: NativeFn,
}

impl PartialEq for Function {
    /// Checks equality based on function name.
    fn eq(&self, other: &Function) -> bool {
        self.name == other.name
    }
}

/// Represents an object.
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
    /// Returns the default object, which is Null.
    fn default() -> Self {
        Object::Null
    }
}

impl Object {
    /// Returns the default integer object (0).
    pub const fn default_integer() -> Object {
        Object::Integer(0)
    }

    /// Returns the default float object (0.0).
    pub const fn default_float() -> Object {
        Object::Float(0.0)
    }

    /// Returns the default string object (empty string).
    pub fn default_string() -> Object {
        Object::String(String::new())
    }

    /// Returns the default boolean object (false).
    pub const fn default_boolean() -> Object {
        Object::Boolean(false)
    }

    /// Cast this object to an integer.
    pub fn cast_to_integer(self) -> Result<Object, MexlError> {
        match self {
            Object::Null => Ok(Object::default_integer()),
            Object::Integer(_) => Ok(self),
            Object::Float(f) => Ok((f as i64).into()),
            Object::String(s) => s.parse::<i64>().map(Object::Integer).map_err(|_| {
                MexlError::CastError(format!("cannot cast string '{}' to integer", s))
            }),
            other => Err(MexlError::CastError(format!(
                "cannot cast {} to integer",
                other
            ))),
        }
    }

    /// Cast this object to a float.
    pub fn cast_to_float(self) -> Result<Object, MexlError> {
        match self {
            Object::Null => Ok(Object::default_float()),
            Object::Integer(i) => Ok((i as f64).into()),
            Object::Float(_) => Ok(self),
            Object::String(s) => s
                .parse::<f64>()
                .map(Object::Float)
                .map_err(|_| MexlError::CastError(format!("cannot cast string '{}' to float", s))),
            other => Err(MexlError::CastError(format!(
                "cannot cast {} to float",
                other
            ))),
        }
    }

    /// Cast this object to a string.
    pub fn cast_to_string(self) -> Result<Object, MexlError> {
        match self {
            Object::Null => Ok(Object::default_string()),
            Object::Integer(i) => Ok(i.to_string().into()),
            Object::Float(f) => Ok(f.to_string().into()),
            Object::String(_) => Ok(self),
            Object::Boolean(b) => Ok(b.to_string().into()),
            other => Err(MexlError::CastError(format!(
                "cannot cast {} to string",
                other
            ))),
        }
    }

    /// Cast this object to a boolean according to truthiness rules.
    pub fn cast_to_boolean(self) -> Result<Object, MexlError> {
        if matches!(self, Object::Function(_)) {
            return Err(MexlError::CastError(format!(
                "cannot cast {} to boolean",
                self
            )));
        }
        Ok(self.is_truthy().into())
    }

    /// Determines if an object is truthy.
    pub fn is_truthy(&self) -> bool {
        match self {
            Object::Null => false,
            Object::Integer(i) => *i != 0,
            Object::Float(f) => *f != 0.0,
            Object::String(s) => !s.is_empty(),
            Object::Boolean(b) => *b,
            Object::Array(a) => !a.is_empty(),
            Object::Map(m) => !m.is_empty(),
            Object::Function(_) => true,
        }
    }
}

impl fmt::Display for Object {
    /// Formats the object as a string.
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
            }
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
    /// Converts an i64 to an Object::Integer.
    fn from(value: i64) -> Self {
        Object::Integer(value)
    }
}

impl From<f64> for Object {
    /// Converts an f64 to an Object::Float.
    fn from(value: f64) -> Self {
        Object::Float(value)
    }
}

impl From<&str> for Object {
    /// Converts a &str to an Object::String.
    fn from(value: &str) -> Self {
        Object::String(value.to_owned())
    }
}

impl From<String> for Object {
    /// Converts a String to an Object::String.
    fn from(value: String) -> Self {
        Object::String(value)
    }
}

impl From<bool> for Object {
    /// Converts a bool to an Object::Boolean.
    fn from(value: bool) -> Self {
        Object::Boolean(value)
    }
}

impl From<Vec<Object>> for Object {
    /// Converts a Vec<Object> to an Object::Array.
    fn from(value: Vec<Object>) -> Self {
        Object::Array(value)
    }
}

impl From<HashMap<String, Object>> for Object {
    /// Converts a HashMap<String, Object> to an Object::Map.
    fn from(value: HashMap<String, Object>) -> Self {
        Object::Map(value)
    }
}

#[cfg(feature = "serde")]
impl From<serde_json::Value> for Object {
    /// Converts a Value to an Object.
    fn from(value: serde_json::Value) -> Self {
        match value {
            serde_json::Value::Null => Object::Null,
            serde_json::Value::Bool(b) => Object::Boolean(b),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Object::Integer(i)
                } else {
                    Object::Float(n.as_f64().unwrap_or(0.0))
                }
            }
            serde_json::Value::String(s) => Object::String(s),
            serde_json::Value::Array(arr) => {
                Object::Array(arr.into_iter().map(Object::from).collect())
            }
            serde_json::Value::Object(map) => {
                let obj_map = map.into_iter().map(|(k, v)| (k, Object::from(v))).collect();
                Object::Map(obj_map)
            }
        }
    }
}

/// Unifies two operands by promoting types as necessary.
pub fn unify_operands(left: Object, right: Object) -> (Object, Object) {
    match (&left, &right) {
        (_, Object::Null) => match &left {
            Object::Integer(_) => (left, Object::default_integer()),
            Object::Float(_) => (left, Object::default_float()),
            Object::String(_) => (left, Object::default_string()),
            Object::Boolean(_) => (left, Object::default_boolean()),
            _ => (left, right),
        },
        (Object::Null, _) => match &right {
            Object::Integer(_) => (Object::default_integer(), right),
            Object::Float(_) => (Object::default_float(), right),
            Object::String(_) => (Object::default_string(), right),
            Object::Boolean(_) => (Object::default_boolean(), right),
            _ => (left, right),
        },
        (Object::Integer(l), Object::Float(_)) => (Object::Float(*l as f64), right),
        (Object::Float(_), Object::Integer(r)) => (left, Object::Float(*r as f64)),
        _ => (left, right),
    }
}

#[cfg(feature = "serde")]
mod ser {
    use super::*;

    use serde::{
        Serialize, Serializer,
        ser::{SerializeMap, SerializeSeq},
    };
    
    impl Serialize for Object {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                Object::Null => serializer.serialize_unit(),
                Object::Integer(i) => serializer.serialize_i64(*i),
                Object::Float(f) => serializer.serialize_f64(*f),
                Object::String(s) => serializer.serialize_str(s),
                Object::Boolean(b) => serializer.serialize_bool(*b),
                Object::Array(arr) => {
                    let mut seq = serializer.serialize_seq(Some(arr.len()))?;
                    for obj in arr {
                        seq.serialize_element(obj)?;
                    }
                    seq.end()
                }
                Object::Map(map) => {
                    let mut m = serializer.serialize_map(Some(map.len()))?;
                    for (k, v) in map {
                        m.serialize_entry(k, v)?;
                    }
                    m.end()
                }
                Object::Function(_) => serializer.serialize_str("<function>"),
            }
        }
    }
}

#[cfg(test)]
#[cfg(feature = "serde")]
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
        let tests: Vec<(Object, Result<Object, MexlError>)> = vec![
            (Object::Null, Ok(0.into())),
            (1.into(), Ok(1.into())),
            (1.5.into(), Ok(1.into())),
            ("1".into(), Ok(1.into())),
            (
                "a".into(),
                Err(MexlError::CastError(
                    "cannot cast string 'a' to integer".into(),
                )),
            ),
            (
                true.into(),
                Err(MexlError::CastError(
                    "cannot cast boolean 'true' to integer".into(),
                )),
            ),
        ];

        for (input, expected) in tests {
            match input.cast_to_integer() {
                Ok(actual) => {
                    assert_eq!(actual, expected.unwrap());
                }
                Err(_) => {
                    assert!(expected.is_err())
                }
            }
        }
    }

    #[test]
    fn test_cast_to_float() {
        let tests: Vec<(Object, Result<Object, MexlError>)> = vec![
            (Object::Null, Ok(0.0.into())),
            (1.into(), Ok(1.0.into())),
            (1.5.into(), Ok(1.5.into())),
            ("1.5".into(), Ok(1.5.into())),
            (
                "a".into(),
                Err(MexlError::CastError(
                    "cannot cast string 'a' to float".into(),
                )),
            ),
            (
                true.into(),
                Err(MexlError::CastError(
                    "cannot cast boolean 'true' to float".into(),
                )),
            ),
        ];

        for (input, expected) in tests {
            match input.cast_to_float() {
                Ok(actual) => {
                    assert_eq!(actual, expected.unwrap());
                }
                Err(_) => {
                    assert!(expected.is_err())
                }
            }
        }
    }

    #[test]
    fn test_cast_to_string() {
        let tests: Vec<(Object, Result<Object, MexlError>)> = vec![
            (Object::Null, Ok("".into())),
            (1.into(), Ok("1".into())),
            (1.5.into(), Ok("1.5".into())),
            ("1.5".into(), Ok("1.5".into())),
            (true.into(), Ok("true".into())),
            (
                vec![].into(),
                Err(MexlError::CastError("cannot cast array to string".into())),
            ),
        ];

        for (input, expected) in tests {
            match input.cast_to_string() {
                Ok(actual) => {
                    assert_eq!(actual, expected.unwrap());
                }
                Err(_) => {
                    assert!(expected.is_err())
                }
            }
        }
    }

    #[test]
    fn test_cast_to_boolean() {
        let tests: Vec<(Object, Result<Object, MexlError>)> = vec![
            (1.into(), Ok(true.into())),
            (0.0.into(), Ok(false.into())),
            (
                Object::Function(Function {
                    name: "fn".into(),
                    handler: |_| unreachable!(),
                }),
                Err(MexlError::CastError(
                    "cannot cast function to boolean".into(),
                )),
            ),
        ];

        for (input, expected) in tests {
            match input.cast_to_boolean() {
                Ok(actual) => {
                    assert_eq!(actual, expected.unwrap());
                }
                Err(_) => {
                    assert!(expected.is_err())
                }
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
            (
                vec![
                    1.into(),
                    1.5.into(),
                    "abc".into(),
                    true.into(),
                    Object::Null,
                ]
                .into(),
                r#"[1, 1.5, "abc", true, null]"#,
            ),
            (
                HashMap::from([(
                    "x".into(),
                    HashMap::from([("y".into(), true.into())]).into(),
                )])
                .into(),
                r#"{"x": {"y": true}}"#,
            ),
            (Object::Null, "null"),
            (
                Object::Function(Function {
                    name: "test".to_owned(),
                    handler: |_| unreachable!(),
                }),
                "fn:test",
            ),
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
            (
                Object::Function(Function {
                    name: "fn".into(),
                    handler: |_| unreachable!(),
                }),
                true,
            ),
        ];

        for (input, expected) in tests {
            let actual = input.is_truthy();
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn test_unify_operands() {
        let arr: Object = vec![1.into(), 2.into()].into();

        struct TestCase {
            input_left: Object,
            input_right: Object,
            expected_left: Object,
            expected_right: Object,
        }

        let tests = vec![
            TestCase {
                input_left: 1.into(),
                input_right: Object::Null,
                expected_left: 1.into(),
                expected_right: 0.into(),
            },
            TestCase {
                input_left: Object::Null,
                input_right: 1.into(),
                expected_left: 0.into(),
                expected_right: 1.into(),
            },
            TestCase {
                input_left: 1.5.into(),
                input_right: Object::Null,
                expected_left: 1.5.into(),
                expected_right: 0.0.into(),
            },
            TestCase {
                input_left: Object::Null,
                input_right: 1.5.into(),
                expected_left: 0.0.into(),
                expected_right: 1.5.into(),
            },
            TestCase {
                input_left: "a".into(),
                input_right: Object::Null,
                expected_left: "a".into(),
                expected_right: "".into(),
            },
            TestCase {
                input_left: Object::Null,
                input_right: "a".into(),
                expected_left: "".into(),
                expected_right: "a".into(),
            },
            TestCase {
                input_left: true.into(),
                input_right: Object::Null,
                expected_left: true.into(),
                expected_right: false.into(),
            },
            TestCase {
                input_left: Object::Null,
                input_right: arr.clone(),
                expected_left: Object::Null,
                expected_right: arr.clone(),
            },
            TestCase {
                input_left: arr.clone(),
                input_right: Object::Null,
                expected_left: arr.clone(),
                expected_right: Object::Null,
            },
            TestCase {
                input_left: Object::Null,
                input_right: true.into(),
                expected_left: false.into(),
                expected_right: true.into(),
            },
            TestCase {
                input_left: 1.into(),
                input_right: 1.5.into(),
                expected_left: 1.0.into(),
                expected_right: 1.5.into(),
            },
            TestCase {
                input_left: 1.5.into(),
                input_right: 1.into(),
                expected_left: 1.5.into(),
                expected_right: 1.0.into(),
            },
        ];

        for test in tests {
            let (actual_left, actual_right) = unify_operands(test.input_left, test.input_right);
            assert_eq!(actual_left, test.expected_left);
            assert_eq!(actual_right, test.expected_right);
        }
    }

    #[test]
    fn test_function_eq() {
        let handler: NativeFn = |_| Err(MexlError::RuntimeError("error".into()));

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

    #[test]
    fn test_serde_json_conversion() {
        struct TestCase {
            input: serde_json::Value,
            expected: Object,
        }

        let tests = vec![
            TestCase {
                input: serde_json::json!(null),
                expected: Object::Null,
            },
            TestCase {
                input: serde_json::json!(true),
                expected: Object::Boolean(true),
            },
            TestCase {
                input: serde_json::json!(false),
                expected: Object::Boolean(false),
            },
            TestCase {
                input: serde_json::json!(42),
                expected: Object::Integer(42),
            },
            TestCase {
                input: serde_json::json!(-100),
                expected: Object::Integer(-100),
            },
            TestCase {
                input: serde_json::json!(3.14),
                expected: Object::Float(3.14),
            },
            TestCase {
                input: serde_json::json!(-2.71),
                expected: Object::Float(-2.71),
            },
            TestCase {
                input: serde_json::json!("hello"),
                expected: Object::String("hello".into()),
            },
            TestCase {
                input: serde_json::json!([1, "two", 3.0, true, null]),
                expected: Object::Array(vec![
                    Object::Integer(1),
                    Object::String("two".into()),
                    Object::Float(3.0),
                    Object::Boolean(true),
                    Object::Null,
                ]),
            },
            TestCase {
                input: serde_json::json!([]),
                expected: Object::Array(vec![]),
            },
            TestCase {
                input: serde_json::json!({
                    "name": "Alice",
                    "age": 30,
                    "score": 95.5,
                    "active": true
                }),
                expected: {
                    let mut map = HashMap::new();
                    map.insert("name".into(), Object::String("Alice".into()));
                    map.insert("age".into(), Object::Integer(30));
                    map.insert("score".into(), Object::Float(95.5));
                    map.insert("active".into(), Object::Boolean(true));
                    Object::Map(map)
                },
            },
            TestCase {
                input: serde_json::json!({}),
                expected: Object::Map(HashMap::new()),
            },
            TestCase {
                input: serde_json::json!({
                    "user": {
                        "id": 1,
                        "tags": ["rust", "coding"]
                    }
                }),
                expected: {
                    let mut inner_map = HashMap::new();
                    inner_map.insert("id".into(), Object::Integer(1));
                    inner_map.insert(
                        "tags".into(),
                        Object::Array(vec![
                            Object::String("rust".into()),
                            Object::String("coding".into()),
                        ]),
                    );

                    let mut outer_map = HashMap::new();
                    outer_map.insert("user".into(), Object::Map(inner_map));
                    Object::Map(outer_map)
                },
            },
        ];

        for test in tests {
            let actual = Object::from(test.input);
            assert_eq!(actual, test.expected);
        }
    }

    #[test]
    fn test_serde_serialize() {
        let tests: Vec<(Object, &'static str)> = vec![
            (Object::Integer(1), "1"),
            (Object::Float(1.5), "1.5"),
            (Object::String("abc".into()), r#""abc""#),
            (Object::Boolean(true), "true"),
            (Object::Boolean(false), "false"),
            (
                vec![
                    1.into(),
                    1.5.into(),
                    "abc".into(),
                    true.into(),
                    Object::Null,
                ]
                .into(),
                r#"[1,1.5,"abc",true,null]"#,
            ),
            (
                HashMap::from([(
                    "x".into(),
                    HashMap::from([("y".into(), true.into())]).into(),
                )])
                .into(),
                r#"{"x":{"y":true}}"#,
            ),
            (Object::Null, "null"),
            (
                Object::Function(Function {
                    name: "test".into(),
                    handler: |_| unreachable!(),
                }),
                r#""<function>""#,
            ),
        ];

        for (input, expected) in tests {
            let actual = serde_json::to_string(&input).unwrap();
            assert_eq!(actual, expected);
        }
    }
}
