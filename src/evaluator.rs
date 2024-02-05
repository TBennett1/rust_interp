use std::fmt;
use std::rc::Rc;

use crate::object::Object;
use crate::token::Token;
use crate::{ast::*, object};
use anyhow::Result;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub type EvalResult = Result<Object, EvalError>;

#[derive(Debug)]
pub struct EvalError {
    pub msg: String,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ERROR: {}", self.msg)
    }
}

impl std::error::Error for EvalError {
    fn description(&self) -> &str {
        &self.msg
    }
}

pub fn eval(node: &Node) -> EvalResult {
    match node {
        Node::Program(prog) => eval_program(prog),
        Node::Statement(stmt) => eval_statement(stmt),
        Node::Expression(exp) => eval_expression(exp),
    }
}

fn eval_program(prog: &Program) -> EvalResult {
    let mut result = Object::Null;

    for stmt in &prog.statements {
        let res = eval_statement(stmt)?;
        match &res {
            Object::Return(r) => return Ok(r.value.clone()),
            _ => result = res,
        }
    }
    Ok(result)
}

fn eval_block_statement(block: &BlockStatement) -> EvalResult {
    let mut result = Object::Null;
    for stmt in &block.statments {
        let res = eval_statement(stmt)?;

        match res {
            Object::Return(_) => return Ok(res),
            _ => result = res,
        }
    }
    Ok(result)
}

fn eval_statement(stmt: &Statement) -> EvalResult {
    match stmt {
        Statement::Expression(exp) => eval_expression(&exp.expression),
        Statement::Return(ret) => {
            let value = eval_expression(&ret.value)?;
            Ok(Object::Return(Rc::new(object::ReturnValue { value })))
        }
        _ => todo!(),
    }
}

fn eval_expression(exp: &Expression) -> EvalResult {
    match exp {
        Expression::Integer(i) => Ok(Object::Integer(*i)),
        Expression::Boolean(b) => Ok(native_bool_to_boolean_object(b)),
        Expression::Prefix(p) => {
            let right = eval_expression(&p.right)?;
            eval_prefix_expression(&p.operator, right)
        }
        Expression::Infix(infix) => {
            let right = eval_expression(&infix.right_value)?;
            let left = eval_expression(&infix.left_value)?;

            eval_infix_expression(&infix.operator, right, left)
        }
        Expression::If(ifexp) => eval_if_statement(ifexp),
        _ => Ok(NULL),
    }
}

fn eval_if_statement(ifexp: &IfExpression) -> EvalResult {
    let condition = eval_expression(&ifexp.condition).unwrap();
    match is_truthy(condition) {
        true => eval_block_statement(&ifexp.consequence),
        false => match &ifexp.alternative {
            Some(alt) => eval_block_statement(alt),
            None => Ok(NULL),
        },
    }
}

fn eval_infix_expression(op: &Token, right: Object, left: Object) -> EvalResult {
    match (&left, &right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(op, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(op, l, r),
        _ => Err(EvalError {
            msg: format!("type mismatch: {:?} {} {:?}", left, op, right),
        }),
    }
}

fn eval_boolean_infix_expression(op: &Token, left: &bool, right: &bool) -> EvalResult {
    match op {
        Token::Eq => Ok(native_bool_to_boolean_object(&(left == right))),
        Token::NotEq => Ok(native_bool_to_boolean_object(&(left != right))),
        _ => Err(EvalError {
            msg: format!("unknown operator: {:?} {} {:?}", left, op, right),
        }),
    }
}

fn eval_integer_infix_expression(op: &Token, left: &i64, right: &i64) -> EvalResult {
    match op {
        Token::Plus => Ok(Object::Integer(left + right)),
        Token::Minus => Ok(Object::Integer(left - right)),
        Token::Asterisk => Ok(Object::Integer(left * right)),
        Token::Slash => Ok(Object::Integer(left / right)),
        Token::Lt => Ok(Object::Boolean(left < right)),
        Token::Gt => Ok(Object::Boolean(left > right)),
        Token::Eq => Ok(Object::Boolean(left == right)),
        Token::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError {
            msg: format!("unknown operator: {:?} {} {:?}", left, op, right),
        }),
    }
}

fn eval_prefix_expression(op: &Token, right: Object) -> EvalResult {
    match op {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => Err(EvalError {
            msg: format!("unknown prefix operator: {}", op),
        }),
    }
}

fn eval_bang_operator_expression(right: Object) -> EvalResult {
    Ok(match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    })
}

fn eval_minus_prefix_operator_expression(right: Object) -> EvalResult {
    match right {
        Object::Integer(i) => Ok(Object::Integer(-i)),
        _ => Err(EvalError {
            msg: format!("unknown operator: -{:?}", right),
        }),
    }
}

fn native_bool_to_boolean_object(bool: &bool) -> Object {
    if *bool {
        return TRUE;
    }
    FALSE
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        TRUE => true,
        FALSE => false,
        Object::Null => false,
        _ => true,
    }
}

#[cfg(test)]
mod test {
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
        ];

        for test in tests {
            let l = Lexer::new(test.input.into());
            let mut p = Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval(&Node::Program(Box::new(program)));

            match evaluated {
                Err(e) => assert_eq!(e.msg, test.expected),
                Ok(n) => panic!("expected error {}, got {:?}", test.expected, n),
            }
        }
    }

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input.into());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        eval(&Node::Program(Box::new(program))).expect(input)
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
