use std::collections::HashMap;

use crate::{Environment, Object};

pub struct TestCase {
    pub input: &'static str,
    pub expected: Object,
}

pub fn integer_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: "1",
            expected: 1.into(),
        },
        TestCase {
            input: "-1",
            expected: (-1).into(),
        },
        TestCase {
            input: "1 + 2",
            expected: 3.into(),
        },
        TestCase {
            input: "2 - 1",
            expected: 1.into(),
        },
        TestCase {
            input: "2 * 2",
            expected: 4.into(),
        },
        TestCase {
            input: "2 / 2",
            expected: 1.into(),
        },
        TestCase {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: 50.into(),
        },
    ]
}

pub fn float_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: "1.5",
            expected: 1.5.into(),
        },
        TestCase {
            input: "-1.5",
            expected: (-1.5).into(),
        },
        TestCase {
            input: "0.5 + 1.5",
            expected: 2.0.into(),
        },
        TestCase {
            input: "1.5 - 0.5",
            expected: 1.0.into(),
        },
        TestCase {
            input: "1.5 * 0.5",
            expected: 0.75.into(),
        },
        TestCase {
            input: "1.5 / 0.5",
            expected: 3.0.into(),
        },
        TestCase {
            input: "1 + 1.5",
            expected: 2.5.into(),
        },
        TestCase {
            input: "1.5 + 1",
            expected: 2.5.into(),
        },
        TestCase {
            input: "(2.5 + .5 * 2.5 + 15. / 3.0) * 0.5 + -1.375",
            expected: 3.0.into(),
        },
    ]
}

pub fn string_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: r#""""#,
            expected: "".into(),
        },
        TestCase {
            input: r#""abc""#,
            expected: "abc".into(),
        },
        TestCase {
            input: r#""a" + "b" + "c""#,
            expected: "abc".into(),
        },
        TestCase {
            input: r#""\"quoted\"""#,
            expected: r#""quoted""#.into(),
        },
    ]
}

pub fn boolean_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: "true",
            expected: true.into(),
        },
        TestCase {
            input: "false",
            expected: false.into(),
        },
        TestCase {
            input: "!true",
            expected: false.into(),
        },
        TestCase {
            input: "!false",
            expected: true.into(),
        },
        TestCase {
            input: "not true",
            expected: false.into(),
        },
        TestCase {
            input: "not false",
            expected: true.into(),
        },
        TestCase {
            input: "true == true",
            expected: true.into(),
        },
        TestCase {
            input: "true == false",
            expected: false.into(),
        },
        TestCase {
            input: "true eq true",
            expected: true.into(),
        },
        TestCase {
            input: "true eq false",
            expected: false.into(),
        },
        TestCase {
            input: "true != true",
            expected: false.into(),
        },
        TestCase {
            input: "true != false",
            expected: true.into(),
        },
        TestCase {
            input: "true ne true",
            expected: false.into(),
        },
        TestCase {
            input: "true ne false",
            expected: true.into(),
        },
        TestCase {
            input: "!null",
            expected: true.into(),
        },
        TestCase {
            input: "not null",
            expected: true.into(),
        },
        TestCase {
            input: "!5",
            expected: false.into(),
        },
        TestCase {
            input: "1 == 1",
            expected: true.into(),
        },
        TestCase {
            input: "1 == 2",
            expected: false.into(),
        },
        TestCase {
            input: "1 eq 1",
            expected: true.into(),
        },
        TestCase {
            input: "1 eq 2",
            expected: false.into(),
        },
        TestCase {
            input: "1 != 1",
            expected: false.into(),
        },
        TestCase {
            input: "1 != 2",
            expected: true.into(),
        },
        TestCase {
            input: "1 ne 1",
            expected: false.into(),
        },
        TestCase {
            input: "1 ne 2",
            expected: true.into(),
        },
        TestCase {
            input: "1 < 2",
            expected: true.into(),
        },
        TestCase {
            input: "1 > 2",
            expected: false.into(),
        },
        TestCase {
            input: "1 lt 2",
            expected: true.into(),
        },
        TestCase {
            input: "1 gt 2",
            expected: false.into(),
        },
        TestCase {
            input: "1 <= 2",
            expected: true.into(),
        },
        TestCase {
            input: "1 >= 2",
            expected: false.into(),
        },
        TestCase {
            input: "1 le 2",
            expected: true.into(),
        },
        TestCase {
            input: "1 ge 2",
            expected: false.into(),
        },
        TestCase {
            input: "1.5 == 1.5",
            expected: true.into(),
        },
        TestCase {
            input: "1.5 == 2.5",
            expected: false.into(),
        },
        TestCase {
            input: "1.5 eq 1.5",
            expected: true.into(),
        },
        TestCase {
            input: "1.5 eq 2.5",
            expected: false.into(),
        },
        TestCase {
            input: "1.5 != 1.5",
            expected: false.into(),
        },
        TestCase {
            input: "1.5 != 2.5",
            expected: true.into(),
        },
        TestCase {
            input: "1.5 ne 1.5",
            expected: false.into(),
        },
        TestCase {
            input: "1.5 ne 2.5",
            expected: true.into(),
        },
        TestCase {
            input: "1.0 < 2.0",
            expected: true.into(),
        },
        TestCase {
            input: "1.0 > 2.0",
            expected: false.into(),
        },
        TestCase {
            input: "1.0 lt 2.0",
            expected: true.into(),
        },
        TestCase {
            input: "1.0 gt 2.0",
            expected: false.into(),
        },
        TestCase {
            input: "1.0 <= 2.0",
            expected: true.into(),
        },
        TestCase {
            input: "1.0 >= 2.0",
            expected: false.into(),
        },
        TestCase {
            input: "1.0 le 2.0",
            expected: true.into(),
        },
        TestCase {
            input: "1.0 ge 2.0",
            expected: false.into(),
        },
        TestCase {
            input: "1 le 2.5",
            expected: true.into(),
        },
        TestCase {
            input: "2.5 ge 1",
            expected: true.into(),
        },
        TestCase {
            input: r#""a" == "a""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""a" == "b""#,
            expected: false.into(),
        },
        TestCase {
            input: r#""a" eq"a""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""a" eq "b""#,
            expected: false.into(),
        },
        TestCase {
            input: r#""a" != "a""#,
            expected: false.into(),
        },
        TestCase {
            input: r#""a" != "b""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""a" ne"a""#,
            expected: false.into(),
        },
        TestCase {
            input: r#""a" ne "b""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""a" in "abc""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""d" in "abc""#,
            expected: false.into(),
        },
        TestCase {
            input: r#"1 in [true, 1, "abc"]"#,
            expected: true.into(),
        },
        TestCase {
            input: r#"1 in [true, 1.0, "abc"]"#,
            expected: true.into(),
        },
        TestCase {
            input: r#""abc" sw "a""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""abc" sw "c""#,
            expected: false.into(),
        },
        TestCase {
            input: r#""abc" ew "c""#,
            expected: true.into(),
        },
        TestCase {
            input: r#""abc" ew "a""#,
            expected: false.into(),
        },
        TestCase {
            input: "true && true",
            expected: true.into(),
        },
        TestCase {
            input: "true && false",
            expected: false.into(),
        },
        TestCase {
            input: "true and true",
            expected: true.into(),
        },
        TestCase {
            input: "true and false",
            expected: false.into(),
        },
        TestCase {
            input: "true || false",
            expected: true.into(),
        },
        TestCase {
            input: "false || false",
            expected: false.into(),
        },
        TestCase {
            input: "true or false",
            expected: true.into(),
        },
        TestCase {
            input: "false or false",
            expected: false.into(),
        },
        TestCase {
            input: "null eq null",
            expected: true.into(),
        },
        TestCase {
            input: "null ne null",
            expected: false.into(),
        },
        TestCase {
            input: "null == null",
            expected: true.into(),
        },
        TestCase {
            input: "1 != null",
            expected: true.into(),
        },
        TestCase {
            input: "null == 1",
            expected: false.into(),
        },
    ]
}

pub fn ident_tests(context: &mut Environment) -> Vec<TestCase> {
    context.set("integer", 1.into());
    context.set("float", 1.5.into());
    context.set("boolean", true.into());
    context.set("string", "abc".into());
    context.set(
        "array",
        vec![1.into(), 0.5.into(), true.into(), "abc".into()].into(),
    );

    vec![
        TestCase {
            input: "invalid",
            expected: Object::Null,
        },
        TestCase {
            input: "integer",
            expected: context.get("integer").unwrap(),
        },
        TestCase {
            input: "float",
            expected: context.get("float").unwrap(),
        },
        TestCase {
            input: "boolean",
            expected: context.get("boolean").unwrap(),
        },
        TestCase {
            input: "string",
            expected: context.get("string").unwrap(),
        },
        TestCase {
            input: "array",
            expected: context.get("array").unwrap(),
        },
    ]
}

pub fn builtin_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: r#"len("abc")"#,
            expected: 3.into(),
        },
        TestCase {
            input: "len([1, 0.5, true])",
            expected: 3.into(),
        },
        TestCase {
            input: r#"lower("ABC")"#,
            expected: "abc".into(),
        },
        TestCase {
            input: r#"upper("abc")"#,
            expected: "ABC".into(),
        },
        TestCase {
            input: r#"trim("  hello  ")"#,
            expected: "hello".into(),
        },
    ]
}

pub fn cast_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: "null as int",
            expected: 0.into(),
        },
        TestCase {
            input: "null as float",
            expected: 0.0.into(),
        },
        TestCase {
            input: "null as string",
            expected: "".into(),
        },
        TestCase {
            input: "null as bool",
            expected: false.into(),
        },
        TestCase {
            input: "1 as int",
            expected: 1.into(),
        },
        TestCase {
            input: "1 as float",
            expected: 1.0.into(),
        },
        TestCase {
            input: "1 as string",
            expected: "1".into(),
        },
        TestCase {
            input: "0 as bool",
            expected: false.into(),
        },
        TestCase {
            input: "1 as bool",
            expected: true.into(),
        },
        TestCase {
            input: "1.5 as int",
            expected: 1.into(),
        },
        TestCase {
            input: "1.5 as float",
            expected: 1.5.into(),
        },
        TestCase {
            input: "1.5 as string",
            expected: "1.5".into(),
        },
        TestCase {
            input: "0.0 as bool",
            expected: false.into(),
        },
        TestCase {
            input: "1.5 as bool",
            expected: true.into(),
        },
        TestCase {
            input: "\"1\" as int",
            expected: 1.into(),
        },
        TestCase {
            input: "\"1.5\" as float",
            expected: 1.5.into(),
        },
        TestCase {
            input: "\"abc\" as string",
            expected: "abc".into(),
        },
        TestCase {
            input: "\"\" as bool",
            expected: false.into(),
        },
        TestCase {
            input: "\"abc\" as bool",
            expected: true.into(),
        },
        TestCase {
            input: "false as string",
            expected: "false".into(),
        },
        TestCase {
            input: "true as string",
            expected: "true".into(),
        },
        TestCase {
            input: "3 / 2",
            expected: 1.into(),
        },
        TestCase {
            input: "3 / 2 as float",
            expected: 1.5.into(),
        },
    ]
}

pub fn array_tests() -> Vec<TestCase> {
    vec![
        TestCase {
            input: "len([])",
            expected: 0.into(),
        },
        TestCase {
            input: "len([1, 2, 3])",
            expected: 3.into(),
        },
        TestCase {
            input: "2 in [1, 2, 3]",
            expected: true.into(),
        },
        TestCase {
            input: "5 in [1, 2, 3]",
            expected: false.into(),
        },
        TestCase {
            input: "2 in [1, 2.0, 3]",
            expected: true.into(),
        },
    ]
}

pub fn index_tests(env: &mut Environment) -> Vec<TestCase> {
    env.set("key", "y".into());
    env.set(
        "map",
        HashMap::from([("x".into(), 10.into()), ("y".into(), 20.into())]).into(),
    );

    vec![
        TestCase {
            input: "[1, 2, 3][0]",
            expected: 1.into(),
        },
        TestCase {
            input: "[1, [2, 3]][1][0]",
            expected: 2.into(),
        },
        TestCase {
            input: "[1, 2, 3][10]",
            expected: Object::default(),
        },
        TestCase {
            input: r#""abc"[1]"#,
            expected: "b".into(),
        },
        TestCase {
            input: r#""abc"[3]"#,
            expected: Object::default(),
        },
        TestCase {
            input: r#"map["x"]"#,
            expected: 10.into(),
        },
        TestCase {
            input: r#"map["invalid"]"#,
            expected: Object::default(),
        },
        TestCase {
            input: r#"map[key]"#,
            expected: 20.into(),
        },
        TestCase {
            input: "null[0]",
            expected: Object::default(),
        },
        TestCase {
            input: "[1][null]",
            expected: Object::default(),
        },
    ]
}

pub fn member_tests(env: &mut Environment) -> Vec<TestCase> {
    env.set("m_a_0", HashMap::from([("m_a_1".into(), 1.into())]).into());
    env.set(
        "m_b_0",
        HashMap::from([(
            "m_b_1".into(),
            HashMap::from([("m_b_2".into(), 1.into())]).into(),
        )])
        .into(),
    );

    vec![
        TestCase {
            input: "null_field.anything",
            expected: Object::default(),
        },
        TestCase {
            input: "m_a_0.null_field",
            expected: Object::default(),
        },
        TestCase {
            input: "m_a_0.m_a_1",
            expected: 1.into(),
        },
        TestCase {
            input: "m_b_0.m_b_1.null_field",
            expected: Object::default(),
        },
        TestCase {
            input: "m_b_0.m_b_1.m_b_2",
            expected: 1.into(),
        },
    ]
}

pub fn null_coalescing_tests() -> Vec<TestCase> {
    vec![
        // Null + numeric types coalesces to numeric default (0 for int, 0.0 for float)
        TestCase {
            input: "null + 5",
            expected: 5.into(),
        },
        TestCase {
            input: "5 + null",
            expected: 5.into(),
        },
        TestCase {
            input: "null + 1.5",
            expected: 1.5.into(),
        },
        TestCase {
            input: "1.5 + null",
            expected: 1.5.into(),
        },
    ]
}

pub struct ErrorTestCase {
    pub input: &'static str,
    pub should_error: bool,
}

pub fn error_cases(env: &mut Environment) -> Vec<ErrorTestCase> {
    env.set("invalid_fn", 1.into());
    env.set("invalid_map", 1.into());

    vec![
        ErrorTestCase {
            input: "null - null",
            should_error: true,
        },
        ErrorTestCase {
            input: "invalid_fn()",
            should_error: true,
        },
        ErrorTestCase {
            input: "invalid_map.x",
            should_error: true,
        },
        ErrorTestCase {
            input: "5 as invalid_type",
            should_error: true,
        },
        ErrorTestCase {
            input: r#"5 + "text""#,
            should_error: true,
        },
        ErrorTestCase {
            input: r#""text" - 5"#,
            should_error: true,
        },
        ErrorTestCase {
            input: "true + 5",
            should_error: true,
        },
        ErrorTestCase {
            input: r#"5 * "string""#,
            should_error: true,
        },
        ErrorTestCase {
            input: "len(5)",
            should_error: true,
        },
        ErrorTestCase {
            input: "len(true)",
            should_error: true,
        },
        ErrorTestCase {
            input: r#"lower(5)"#,
            should_error: true,
        },
        ErrorTestCase {
            input: "lower(true)",
            should_error: true,
        },
        ErrorTestCase {
            input: r#"upper([])"#,
            should_error: true,
        },
        // Valid cases that should NOT error
        ErrorTestCase {
            input: "null",
            should_error: false,
        },
        ErrorTestCase {
            input: r#"len(null)"#,
            should_error: false,
        },
        ErrorTestCase {
            input: r#"lower(null)"#,
            should_error: false,
        },
        ErrorTestCase {
            input: "5 + 5",
            should_error: false,
        },
        ErrorTestCase {
            input: r#""text" + "more""#,
            should_error: false,
        },
        ErrorTestCase {
            input: "null + 5",
            should_error: false,
        },
        ErrorTestCase {
            input: "5 == 5",
            should_error: false,
        },
        ErrorTestCase {
            input: "true[1]",
            should_error: true,
        },
        ErrorTestCase {
            input: "[1][true]",
            should_error: true,
        },
    ]
}
