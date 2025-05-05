use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::object::{Array, BuiltIn, Environment, Function, Hash, HashKey, HashPair, Object};
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

pub fn eval(node: &Node, env: Rc<RefCell<Environment>>) -> EvalResult {
    match node {
        Node::Program(prog) => eval_program(prog, env),
        Node::Statement(stmt) => eval_statement(stmt, env),
        Node::Expression(exp) => eval_expression(exp, env),
    }
}

fn eval_program(prog: &Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;

    for stmt in &prog.statements {
        let res = eval_statement(stmt, Rc::clone(&env))?;
        match &res {
            Object::Return(r) => return Ok(r.value.clone()),
            _ => result = res,
        }
    }
    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Object::Null;
    for stmt in &block.statments {
        let res = eval_statement(stmt, Rc::clone(&env))?;

        match res {
            Object::Return(_) => return Ok(res),
            _ => result = res,
        }
    }
    Ok(result)
}

fn eval_statement(stmt: &Statement, env: Rc<RefCell<Environment>>) -> EvalResult {
    match stmt {
        Statement::Expression(exp) => eval_expression(&exp.expression, env),
        Statement::Return(ret) => {
            let value = eval_expression(&ret.value, Rc::clone(&env))?;
            Ok(Object::Return(Rc::new(object::ReturnValue { value })))
        }
        Statement::Let(exp) => {
            let value = eval_expression(&exp.value, Rc::clone(&env))?;
            env.borrow_mut().set(exp.name.clone(), value.clone());
            Ok(value)
        }
    }
}

fn eval_expression(exp: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match exp {
        Expression::Integer(i) => Ok(Object::Integer(*i)),
        Expression::Boolean(b) => Ok(native_bool_to_boolean_object(b)),
        Expression::String(s) => Ok(Object::String(s.clone())),
        Expression::Prefix(p) => {
            let right = eval_expression(&p.right, Rc::clone(&env))?;
            eval_prefix_expression(&p.operator, right)
        }
        Expression::Infix(infix) => {
            let right = eval_expression(&infix.right_value, Rc::clone(&env))?;
            let left = eval_expression(&infix.left_value, Rc::clone(&env))?;

            eval_infix_expression(&infix.operator, right, left)
        }
        Expression::If(ifexp) => eval_if_statement(ifexp, Rc::clone(&env)),
        Expression::Identifier(id) => eval_identifier(id, Rc::clone(&env)),
        Expression::Function(f) => {
            let func = Function {
                parameters: f.parameters.clone(),
                body: f.body.clone(),
                env,
            };
            Ok(Object::Function(func))
        }
        Expression::Call(c) => {
            let func = eval_expression(&c.function, Rc::clone(&env))?;
            let args = eval_expressions(&c.arguments, env)?;
            apply_function(func, args)
        }
        Expression::Array(a) => {
            let elements = eval_expressions(&a.elements, env)?;
            Ok(Object::Array(Array { elements }))
        }
        Expression::Index(i) => {
            let left = eval_expression(&i.left, env.clone())?;
            let index = eval_expression(&i.index, env)?;
            eval_index_expression(&left, &index)
        }
        Expression::Hash(h) => eval_hash_literal(h, Rc::clone(&env)),
        Expression::None => Ok(NULL),
    }
}

fn eval_hash_literal(hash: &HashLiteral, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut pairs: HashMap<HashKey, HashPair> = HashMap::new();

    for (key_node, value_node) in &hash.pairs {
        let key = eval_expression(key_node, env.clone())?;
        let hash_key = Object::hash_key(key.clone());
        if hash_key.is_err() {
            return Err(EvalError {
                msg: format!("unusable as hash key: {:?}", key),
            });
        }

        let value = eval_expression(value_node, env.clone())?;
        pairs.insert(hash_key.unwrap(), HashPair { key, value });
    }
    Ok(Object::Hash(object::Hash { pairs }))
}

fn eval_index_expression(left: &Object, index: &Object) -> EvalResult {
    match (left, index) {
        (Object::Array(a), Object::Integer(i)) => Ok(eval_array_index_expression(a, i)),
        (Object::Hash(h), _) => eval_hash_index_expression(h, index),
        _ => Err(EvalError {
            msg: format!("index operator not supported: {:?}", left),
        }),
    }
}

fn eval_hash_index_expression(hash: &Hash, index: &Object) -> EvalResult {
    let key = Object::hash_key(index.clone());
    if key.is_err() {
        return Err(EvalError {
            msg: key.err().unwrap(),
        });
    }

    let pair = hash.pairs.get(&key.unwrap());
    match pair {
        Some(p) => Ok(p.value.clone()),
        None => Ok(NULL),
    }
}

fn eval_array_index_expression(array: &Array, index: &i64) -> Object {
    let max = array.elements.len() - 1;
    if *index < 0 || *index as usize > max {
        return NULL;
    }
    array.elements[*index as usize].clone()
}

fn eval_identifier(ident: &String, env: Rc<RefCell<Environment>>) -> EvalResult {
    match env.borrow().get(ident) {
        Some(obj) => Ok(obj),
        None => match BuiltIn::lookup(ident) {
            Some(obj) => Ok(obj),
            None => Err(EvalError {
                msg: format!("identifier not found: {}", ident),
            }),
        },
    }
}

fn eval_expressions(
    exps: &Vec<Expression>,
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, EvalError> {
    let mut result = Vec::with_capacity(exps.len());

    for exp in exps {
        let evalualted = eval_expression(exp, Rc::clone(&env))?;
        result.push(evalualted);
    }

    Ok(result)
}

fn eval_if_statement(ifexp: &IfExpression, env: Rc<RefCell<Environment>>) -> EvalResult {
    let condition = eval_expression(&ifexp.condition, Rc::clone(&env)).unwrap();
    match is_truthy(condition) {
        true => eval_block_statement(&ifexp.consequence, Rc::clone(&env)),
        false => match &ifexp.alternative {
            Some(alt) => eval_block_statement(alt, Rc::clone(&env)),
            None => Ok(NULL),
        },
    }
}

fn eval_infix_expression(op: &Token, right: Object, left: Object) -> EvalResult {
    match (&left, &right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(op, l, r),
        (Object::Boolean(l), Object::Boolean(r)) => eval_boolean_infix_expression(op, l, r),
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(op, l, r),
        _ => Err(EvalError {
            msg: format!("type mismatch: {:?} {} {:?}", left, op, right),
        }),
    }
}

fn eval_string_infix_expression(op: &Token, left: &String, right: &String) -> EvalResult {
    match op {
        Token::Plus => Ok(Object::String(format!("{}{}", left, right))),
        _ => Err(EvalError {
            msg: format!("unknown operator: {:?} {} {:?}", left, op, right),
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

fn apply_function(func: Object, args: Vec<Object>) -> EvalResult {
    match func {
        Object::Function(fun) => {
            let extended_env = extend_function_env(&fun, args);
            let evaluated = eval_block_statement(&fun.body, extended_env)?;
            Ok(unwrap_return_value(evaluated))
        }
        Object::BuiltIn(b) => match b.apply(&args) {
            Ok(obj) => Ok(obj),
            Err(e) => Err(EvalError { msg: e }),
        },
        _ => Err(EvalError {
            msg: format!("not a function: {}", func),
        }),
    }
}

fn extend_function_env(fun: &Function, args: Vec<Object>) -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(&fun.env))));
    let mut args_iter = args.into_iter();

    for param in &fun.parameters {
        let arg = args_iter.next().unwrap();
        env.borrow_mut().set(param.name.clone(), arg);
    }

    env
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::Return(ret) = &obj {
        return ret.value.clone();
    }

    obj
}
