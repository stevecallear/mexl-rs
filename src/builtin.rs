use crate::{Object};

/// Returns the length of a string or array.
pub fn len(args: Vec<Object>) -> Result<Object, String> {
    if args.len() != 1 {
        return Err(format!("len: wrong numer of arguments: {}", args.len()));
    }

    match &args[0] {
        Object::String(s) => Ok((s.len() as i64).into()),
        Object::Array(a) => Ok((a.len() as i64).into()),
        Object::Null => Ok(Object::default_integer()),
        _ => Err(format!("len: argument not supported: {}", args[0]))
    }
}

/// Converts a string to lowercase.
pub fn lower(args: Vec<Object>) -> Result<Object, String> {
    if args.len() != 1 {
        return Err(format!("lower: wrong numer of arguments: {}", args.len()));
    }

    match &args[0] {
        Object::String(s) => Ok(s.to_lowercase().into()),
        Object::Null => Ok(Object::default_string()),
        _ => Err(format!("lower: argument not supported: {}", args[0]))
    }
}

/// Converts a string to uppercase.
pub fn upper(args: Vec<Object>) -> Result<Object, String> {
    if args.len() != 1 {
        return Err(format!("upper: wrong numer of arguments: {}", args.len()));
    }

    match &args[0] {
        Object::String(s) => Ok(s.to_uppercase().into()),
        Object::Null => Ok(Object::default_string()),
        _ => Err(format!("upper: argument not supported: {}", args[0]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

     #[test]
    fn test_len() {
        let tests: Vec<(Vec<Object>, Result<Object, String>)> = vec![
            (vec![], Err("".into())),
            (vec!["a".into(), "b".into()], Err("".into())),
            (vec![true.into()], Err("".into())),
            (vec!["abc".into()], Ok(3.into())),
            (vec![vec![1.into(), 2.into()].into()], Ok(2.into())),
            (vec![Object::Null], Ok(0.into())),            
        ];

        for (input, expected) in tests {
            let actual = len(input);
            if actual.is_err() && expected.is_err() { continue }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }

    #[test]
    fn test_lower() {
        let tests: Vec<(Vec<Object>, Result<Object, String>)> = vec![
            (vec![], Err("".into())),
            (vec!["A".into(), "B".into()], Err("".into())),
            (vec![1.into()], Err("".into())),
            (vec!["AbC".into()], Ok("abc".into())),
            (vec![Object::Null], Ok("".into())),            
        ];

        for (input, expected) in tests {
            let actual = lower(input);
            if actual.is_err() && expected.is_err() { continue }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }

     #[test]
    fn test_upper() {
        let tests: Vec<(Vec<Object>, Result<Object, String>)> = vec![
            (vec![], Err("".into())),
            (vec!["a".into(), "b".into()], Err("".into())),
            (vec![1.into()], Err("".into())),
            (vec!["aBC".into()], Ok("ABC".into())),
            (vec![Object::Null], Ok("".into())),            
        ];

        for (input, expected) in tests {
            let actual = upper(input);
            if actual.is_err() && expected.is_err() { continue }

            assert_eq!(actual.unwrap(), expected.unwrap());
        }
    }
}