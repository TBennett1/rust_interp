use std::{cell::RefCell, collections::HashMap, rc::Rc};

use rust_interp::{
    ast::Node,
    evaluator::eval,
    lexer::Lexer,
    object::{Environment, HashKey, Object},
    parser::Parser,
};
const NULL: Object = Object::Null;

#[test]
fn eval_integer_statement() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }
    let tests = vec![
        Test {
            input: "5",
            expected: 5,
        },
        Test {
            input: "5",
            expected: 5,
        },
        Test {
            input: "-5",
            expected: -5,
        },
        Test {
            input: "-10",
            expected: -10,
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10",
            expected: 10,
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2",
            expected: 32,
        },
        Test {
            input: "-50 + 100 + -50",
            expected: 0,
        },
        Test {
            input: "5 * 2 + 10",
            expected: 20,
        },
        Test {
            input: "5 + 2 * 10",
            expected: 25,
        },
        Test {
            input: "20 + 2 * -10",
            expected: 0,
        },
        Test {
            input: "50 / 2 * 2 + 10",
            expected: 60,
        },
        Test {
            input: "2 * (5 + 10)",
            expected: 30,
        },
        Test {
            input: "3 * 3 * 3 + 10",
            expected: 37,
        },
        Test {
            input: "3 * (3 * 3) + 10",
            expected: 37,
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: 50,
        },
    ];

    for t in tests {
        let evaluated = test_eval(t.input);
        test_integer_object(&evaluated, t.expected);
    }
}

#[test]
fn eval_boolean_statement() {
    struct Test<'a> {
        input: &'a str,
        expected: bool,
    }

    let tests = vec![
        Test {
            input: "true",
            expected: true,
        },
        Test {
            input: "false",
            expected: false,
        },
        Test {
            input: "1 < 2",
            expected: true,
        },
        Test {
            input: "1 > 2",
            expected: false,
        },
        Test {
            input: "1 < 1",
            expected: false,
        },
        Test {
            input: "1 > 1",
            expected: false,
        },
        Test {
            input: "1 == 1",
            expected: true,
        },
        Test {
            input: "1 != 1",
            expected: false,
        },
        Test {
            input: "1 == 2",
            expected: false,
        },
        Test {
            input: "1 != 2",
            expected: true,
        },
        Test {
            input: "true == true",
            expected: true,
        },
        Test {
            input: "false == false",
            expected: true,
        },
        Test {
            input: "true == false",
            expected: false,
        },
        Test {
            input: "true != false",
            expected: true,
        },
        Test {
            input: "false != true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == false",
            expected: false,
        },
        Test {
            input: "(1 > 2) == true",
            expected: false,
        },
        Test {
            input: "(1 > 2) == false",
            expected: true,
        },
    ];

    for t in tests {
        let evaluated = test_eval(t.input);
        test_boolean_object(&evaluated, t.expected);
    }
}

#[test]
fn bang_operator() {
    struct Test<'a> {
        input: &'a str,
        expected: bool,
    }

    let tests = vec![
        Test {
            input: "!true",
            expected: false,
        },
        Test {
            input: "!false",
            expected: true,
        },
        Test {
            input: "!5",
            expected: false,
        },
        Test {
            input: "!!true",
            expected: true,
        },
        Test {
            input: "!!false",
            expected: false,
        },
        Test {
            input: "!!5",
            expected: true,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input);
        test_boolean_object(&evaluated, test.expected);
    }
}

#[test]
fn if_else_expression() {
    struct Test<'a> {
        input: &'a str,
        expected: Object,
    }

    let tests = vec![
        Test {
            input: "if (true) { 10 }",
            expected: Object::Integer(10),
        },
        Test {
            input: "if (false) { 10 }",
            expected: NULL,
        },
        Test {
            input: "if (1) { 10 }",
            expected: Object::Integer(10),
        },
        Test {
            input: "if (1 < 2) { 10 }",
            expected: Object::Integer(10),
        },
        Test {
            input: "if (1 > 2) { 10 }",
            expected: NULL,
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::Integer(20),
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::Integer(10),
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input);
        assert_eq!(evaluated, test.expected)
    }
}

#[test]
fn return_statments() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "return 10;",
            expected: 10,
        },
        Test {
            input: "return 10; 9;",
            expected: 10,
        },
        Test {
            input: "return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: "9; return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: "if (10 > 1) {
                if (10 > 1) {
                  return 10;
           }
           return 1; }",
            expected: 10,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input);
        test_integer_object(&evaluated, test.expected)
    }
}

#[test]
fn error_handling() {
    struct Test<'a> {
        input: &'a str,
        expected: &'a str,
    }

    let tests = vec![
        Test {
            input: "5 + true;",
            expected: "type mismatch: Integer(5) + Boolean(true)",
        },
        Test {
            input: "5 + true; 5;",
            expected: "type mismatch: Integer(5) + Boolean(true)",
        },
        Test {
            input: "-true",
            expected: "unknown operator: -Boolean(true)",
        },
        Test {
            input: "true + false;",
            expected: "unknown operator: true + false",
        },
        Test {
            input: "5; true + false; 5",
            expected: "unknown operator: true + false",
        },
        Test {
            input: "if (10 > 1) { true + false; }",
            expected: "unknown operator: true + false",
        },
        Test {
            input: "
        if (10 > 1) {
          if (10 > 1) {
            return true + false;
          }
        return 1; }
        ",
            expected: "unknown operator: true + false",
        },
        Test {
            input: "foobar",
            expected: "identifier not found: foobar",
        },
        Test {
            input: "\"Hello\" - \"World\"",
            expected: "unknown operator: \"Hello\" - \"World\"",
        },
        Test {
            input: "{\"name\": \"Monkey\"}[fn(x) { x }];",
            expected: "unusable as hash key: fn ([IdentifierExpression { name: \"x\" }]) {\nx\n}",
        },
    ];

    for test in tests {
        let l = Lexer::new(test.input.into());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));
        let evaluated = eval(&Node::Program(Box::new(program)), env);

        match evaluated {
            Err(e) => assert_eq!(e.msg, test.expected),
            Ok(n) => panic!("expected error {}, got {:?}", test.expected, n),
        }
    }
}

#[test]
fn let_statement() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "let a = 5; a;",
            expected: 5,
        },
        Test {
            input: "let a = 5 * 5; a;",
            expected: 25,
        },
        Test {
            input: "let a = 5; let b = a; b;",
            expected: 5,
        },
        Test {
            input: "let a = 5; let b = a; let c = a + b + 5; c;",
            expected: 15,
        },
    ];

    for t in tests {
        test_integer_object(&test_eval(t.input), t.expected)
    }
}

#[test]
fn function_object() {
    let input = "fn(x) { x + 2; };";
    let eval = test_eval(input);
    match eval {
        Object::Function(f) => {
            if f.parameters.len() != 1 {
                panic!(
                    "function has wrong parameters. Parameters: {:?}",
                    f.parameters
                )
            }

            if f.parameters[0].name != *"x" {
                panic!("parameter is not x. got {}", f.parameters[0].name)
            }

            let expect_body = "(x + 2)".to_string();

            if f.body.statments[0].to_string() != expect_body {
                panic!("body is not {}. got {}", expect_body, f.body.statments[0])
            }
        }
        _ => panic!("object not a function: {:?}", eval),
    }
}

#[test]
fn function_application() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "let identity = fn(x) { x; }; identity(5);",
            expected: 5,
        },
        Test {
            input: "let identity = fn(x) { return x; }; identity(5);",
            expected: 5,
        },
        Test {
            input: "let double = fn(x) { x * 2; }; double(5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5, 5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            expected: 20,
        },
        Test {
            input: "fn(x) { x; }(5)",
            expected: 5,
        },
    ];

    for t in tests {
        test_integer_object(&test_eval(t.input), t.expected)
    }
}

#[test]
fn string_literal() {
    let input = r#""hello world";"#;
    let evaluated = test_eval(input);

    match evaluated {
        Object::String(s) => assert_eq!(s, "hello world"),
        _ => panic!("this is not a string {:?}", evaluated),
    }
}

#[test]
fn string_cat() {
    let input = r#""Hello" + " " + "World!""#;
    let evaluated = test_eval(input);

    match evaluated {
        Object::String(s) => assert_eq!(s, "Hello World!"),
        _ => panic!("this is not a string {:?}", evaluated),
    }
}

#[test]
fn builtin_function() {
    struct Test<'a> {
        input: &'a str,
        expected: i64,
    }

    let tests = [
        Test {
            input: r#"len("")"#,
            expected: 0,
        },
        Test {
            input: r#"len("four")"#,
            expected: 4,
        },
        Test {
            input: r#"len("hello world")"#,
            expected: 11,
        },
    ];

    for test in tests {
        let eval = test_eval(test.input);

        match eval {
            Object::Integer(i) => assert_eq!(test.expected, i),
            _ => panic!("{:?} not integer", eval),
        }
    }
}

#[test]
fn array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);

    match evaluated {
        Object::Array(a) => {
            test_integer_object(&a.elements[0], 1);
            test_integer_object(&a.elements[1], 4);
            test_integer_object(&a.elements[2], 6);
        }
        _ => panic!("expected array; got {:?}", evaluated),
    }
}

#[test]
fn array_index_expresssions() {
    struct Test<'a> {
        input: &'a str,
        expected: Object,
    }

    let tests = vec![
        Test {
            input: "[1, 2, 3][0]",
            expected: Object::Integer(1),
        },
        Test {
            input: "[1, 2, 3][1]",
            expected: Object::Integer(2),
        },
        Test {
            input: "[1, 2, 3][2]",
            expected: Object::Integer(3),
        },
        Test {
            input: "let i = 0; [1][i];",
            expected: Object::Integer(1),
        },
        Test {
            input: "[1, 2, 3][1 + 1];",
            expected: Object::Integer(3),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[2];",
            expected: Object::Integer(3),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            expected: Object::Integer(6),
        },
        Test {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            expected: Object::Integer(2),
        },
        Test {
            input: "[1, 2, 3][3]",
            expected: NULL,
        },
        Test {
            input: "[1, 2, 3][-1]",
            expected: NULL,
        },
    ];

    for test in tests {
        let eval = test_eval(test.input);
        assert_eq!(eval, test.expected)
    }
}

#[test]
fn hash_literals() {
    let input = r#"let two = "two";
                        {
                            "one": 10 - 9,
                            two: 1 + 1,
                            "thr" + "ee": 6 / 2,
                            4: 4,
                            true: 5,
                            false: 6
                        }"#;
    let result: Object = test_eval(input);
    let expected: HashMap<HashKey, i64> = HashMap::from([
        (
            Object::hash_key(Object::String("one".to_string())).unwrap(),
            1,
        ),
        (
            Object::hash_key(Object::String("two".to_string())).unwrap(),
            2,
        ),
        (
            Object::hash_key(Object::String("three".to_string())).unwrap(),
            3,
        ),
        (Object::hash_key(Object::Integer(4)).unwrap(), 4),
        (Object::hash_key(Object::Boolean(true)).unwrap(), 5),
        (Object::hash_key(Object::Boolean(false)).unwrap(), 6),
    ]);

    match result {
        Object::Hash(hash) => {
            assert_eq!(hash.pairs.len(), expected.len());
            for (expected_key, expected_value) in expected {
                let pair = hash.pairs.get(&expected_key).unwrap();
                test_integer_object(&pair.value, expected_value);
            }
        }
        _ => panic!("kol"),
    }
}

#[test]
fn hash_index_expressions() {
    struct Test<'a> {
        input: &'a str,
        expected: Object,
    }

    let tests = vec![
        Test {
            input: "{\"foo\": 5}[\"foo\"]",
            expected: Object::Integer(5),
        },
        Test {
            input: "{\"foo\": 5}[\"bar\"]",
            expected: NULL,
        },
        Test {
            input: "let key = \"foo\"; {\"foo\": 5}[key]",
            expected: Object::Integer(5),
        },
        Test {
            input: "{}[\"foo\"]",
            expected: NULL,
        },
        Test {
            input: "{5: 5}[5]",
            expected: Object::Integer(5),
        },
        Test {
            input: "{true: 5}[true]",
            expected: Object::Integer(5),
        },
        Test {
            input: "{false: 5}[false]",
            expected: Object::Integer(5),
        },
    ];

    for test in tests {
        let eval = test_eval(test.input);
        assert_eq!(eval, test.expected)
    }
}

// Helpers

fn test_eval(input: &str) -> Object {
    let l = Lexer::new(input.into());
    let mut p = Parser::new(l);
    let env = Rc::new(RefCell::new(Environment::new()));
    let program = p.parse_program();

    eval(&Node::Program(Box::new(program)), env).expect(input)
}

fn test_integer_object(obj: &Object, expected: i64) {
    match obj {
        Object::Integer(i) => assert_eq!(i, &expected),
        _ => panic!("this is not an integer {:?}", obj),
    }
}

fn test_boolean_object(obj: &Object, expected: bool) {
    match obj {
        Object::Boolean(b) => assert_eq!(b, &expected),
        _ => panic!("this is not an boolean {:?}", obj),
    }
}
