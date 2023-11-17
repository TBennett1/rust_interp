use crate::ast::*;
use crate::object::Object;
use crate::token::Token;
use anyhow::Result;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub fn eval(prog: Program) -> Object {
    let mut result = Object::Null;

    for stmt in &prog.statements {
        result = eval_statement(stmt).unwrap();
    }

    result
}

fn eval_statement(stmt: &Statement) -> Result<Object> {
    match stmt {
        Statement::Expression(exp) => eval_expression(&exp.expression),
        _ => todo!(),
    }
}

fn eval_expression(exp: &Expression) -> Result<Object> {
    match exp {
        Expression::Integer(i) => Ok(Object::Integer(*i)),
        Expression::Boolean(b) => Ok(native_bool_to_boolean_object(b)),
        Expression::Prefix(p) => {
            let right = eval_expression(&p.right)?;
            Ok(eval_prefix_expression(&p.operator, right))
        }
        Expression::Infix(infix) => {
            let right = eval_expression(&infix.right_value)?;
            let left = eval_expression(&infix.left_value)?;

            Ok(eval_infix_expression(&infix.operator, right, left))
        }
        _ => Ok(NULL),
    }
}

fn eval_infix_expression(op: &Token, right: Object, left: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(op, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(op, l, r),
        _ => NULL,
    }
}

fn eval_boolean_infix_expression(op: &Token, left: bool, right: bool) -> Object {
    match op {
        Token::Eq => native_bool_to_boolean_object(&(left == right)),
        Token::NotEq => native_bool_to_boolean_object(&(left != right)),
        _ => NULL,
    }
}

fn eval_integer_infix_expression(op: &Token, left: i64, right: i64) -> Object {
    match op {
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        Token::Lt => Object::Boolean(left < right),
        Token::Gt => Object::Boolean(left > right),
        Token::Eq => Object::Boolean(left == right),
        Token::NotEq => Object::Boolean(left != right),
        _ => NULL,
    }
}

fn eval_prefix_expression(op: &Token, right: Object) -> Object {
    match op {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => NULL,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => NULL,
    }
}

fn native_bool_to_boolean_object(bool: &bool) -> Object {
    if *bool {
        return TRUE;
    }
    FALSE
}

#[cfg(test)]
mod test {
    use core::panic;

    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

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

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        eval(program)
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
}
