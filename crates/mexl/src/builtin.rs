use crate::{MexlError, Object};

/// Returns the length of a string or array.
pub fn len(args: Vec<Object>) -> Result<Object, MexlError> {
    if args.len() != 1 {
        return Err(MexlError::RuntimeError(format!(
            "len: wrong numer of arguments: {}",
            args.len()
        )));
    }

    match &args[0] {
        Object::String(s) => Ok((s.len() as i64).into()),
        Object::Array(a) => Ok((a.len() as i64).into()),
        Object::Null => Ok(Object::default_integer()),
        _ => Err(MexlError::RuntimeError(format!(
            "len: argument not supported: {}",
            args[0]
        ))),
    }
}

/// Converts a string to lowercase.
pub fn lower(args: Vec<Object>) -> Result<Object, MexlError> {
    if args.len() != 1 {
        return Err(MexlError::RuntimeError(format!(
            "lower: wrong numer of arguments: {}",
            args.len()
        )));
    }

    match &args[0] {
        Object::String(s) => Ok(s.to_lowercase().into()),
        Object::Null => Ok(Object::default_string()),
        _ => Err(MexlError::RuntimeError(format!(
            "lower: argument not supported: {}",
            args[0]
        ))),
    }
}

/// Converts a string to uppercase.
pub fn upper(args: Vec<Object>) -> Result<Object, MexlError> {
    if args.len() != 1 {
        return Err(MexlError::RuntimeError(format!(
            "upper: wrong numer of arguments: {}",
            args.len()
        )));
    }

    match &args[0] {
        Object::String(s) => Ok(s.to_uppercase().into()),
        Object::Null => Ok(Object::default_string()),
        _ => Err(MexlError::RuntimeError(format!(
            "upper: argument not supported: {}",
            args[0]
        ))),
    }
}

/// Trims whitespace from both ends of a string.
pub fn trim(args: Vec<Object>) -> Result<Object, MexlError> {
    if args.len() != 1 {
        return Err(MexlError::RuntimeError(format!(
            "trim: wrong numer of arguments: {}",
            args.len()
        )));
    }

    match &args[0] {
        Object::String(s) => Ok(s.trim().into()),
        Object::Null => Ok(Object::default_string()),
        _ => Err(MexlError::RuntimeError(format!(
            "trim: argument not supported: {}",
            args[0]
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_len() {
        let tests: Vec<(Vec<Object>, Result<Object, MexlError>)> = vec![
            (vec![], Err(MexlError::RuntimeError(String::new()))),
            (
                vec!["a".into(), "b".into()],
                Err(MexlError::RuntimeError(String::new())),
            ),
            (
                vec![true.into()],
                Err(MexlError::RuntimeError(String::new())),
            ),
            (vec!["abc".into()], Ok(3.into())),
            (vec![vec![1.into(), 2.into()].into()], Ok(2.into())),
            (vec![Object::Null], Ok(0.into())),
        ];

        for (input, expected) in tests {
            let actual = len(input);
            if actual.is_err() && expected.is_err() {
                continue;
            }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }

    #[test]
    fn test_lower() {
        let tests: Vec<(Vec<Object>, Result<Object, MexlError>)> = vec![
            (vec![], Err(MexlError::RuntimeError(String::new()))),
            (
                vec!["A".into(), "B".into()],
                Err(MexlError::RuntimeError(String::new())),
            ),
            (vec![1.into()], Err(MexlError::RuntimeError(String::new()))),
            (vec!["AbC".into()], Ok("abc".into())),
            (vec![Object::Null], Ok("".into())),
        ];

        for (input, expected) in tests {
            let actual = lower(input);
            if actual.is_err() && expected.is_err() {
                continue;
            }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }

    #[test]
    fn test_upper() {
        let tests: Vec<(Vec<Object>, Result<Object, MexlError>)> = vec![
            (vec![], Err(MexlError::RuntimeError(String::new()))),
            (
                vec!["a".into(), "b".into()],
                Err(MexlError::RuntimeError(String::new())),
            ),
            (vec![1.into()], Err(MexlError::RuntimeError(String::new()))),
            (vec!["aBC".into()], Ok("ABC".into())),
            (vec![Object::Null], Ok("".into())),
        ];

        for (input, expected) in tests {
            let actual = upper(input);
            if actual.is_err() && expected.is_err() {
                continue;
            }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }

    #[test]
    fn test_trim() {
        let tests: Vec<(Vec<Object>, Result<Object, MexlError>)> = vec![
            (vec![], Err(MexlError::RuntimeError(String::new()))),
            (
                vec![" a ".into(), " b ".into()],
                Err(MexlError::RuntimeError(String::new())),
            ),
            (vec![1.into()], Err(MexlError::RuntimeError(String::new()))),
            (vec!["  hello  ".into()], Ok("hello".into())),
            (vec![Object::Null], Ok("".into())),
        ];

        for (input, expected) in tests {
            let actual = trim(input);
            if actual.is_err() && expected.is_err() {
                continue;
            }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }
}
