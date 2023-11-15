// use crate::ast::*;
// use crate::object::Object;
// use anyhow::Result;

// pub fn eval(prog: Program) -> Object {
//     let mut result = Object::Null;

//     for stmt in &prog.statements {
//         result = eval_statement(&stmt).unwrap();
//     }

//     return result;
// }

// fn eval_statement(stmt: &Statement) -> Result<Object> {
//     match stmt {
//         Statement::Expression(exp) => eval_expression(exp),
//         _ => todo!(),
//     }
// }

// fn eval_expression(exp: &Expression) -> Result<Object> {
//     match exp {
//         Expression::IntegerLiteral(i) => Ok(Object::Integer(*i)),
//         _ => todo!(),
//     }
// }

// #[cfg(test)]
// mod test {
//     use core::panic;

//     use super::*;
//     use crate::{lexer::Lexer, parser::Parser};

//     #[test]
//     fn eval_integer_statement() {
//         struct Test {
//             input: String,
//             expected: i64,
//         }
//         let tests = vec![
//             Test {
//                 input: "5".to_string(),
//                 expected: 5,
//             },
//             Test {
//                 input: "5".into(),
//                 expected: 5,
//             },
//         ];

//         for t in tests {
//             let evaluated = test_eval(t.input);
//             test_integer_object(&evaluated, t.expected);
//         }
//     }

//     fn test_eval(input: String) -> Object {
//         let l = Lexer::new(input);
//         let mut p = Parser::new(l);
//         let program = p.parse_program();

//         return eval(program);
//     }

//     fn test_integer_object(obj: &Object, expected: i64) {
//         match obj {
//             Object::Integer(i) => assert_eq!(i, &expected),
//             _ => panic!("this is not an integer {:?}", obj),
//         }
//     }
// }
