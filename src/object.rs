use std::{cell::RefCell, collections::HashMap, fmt::*, rc::Rc};

use crate::ast::{BlockStatement, IdentifierExpression};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Rc<ReturnValue>),
    Function(Function),
    Null,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct ReturnValue {
    pub value: Object,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let _ = match self {
            Object::Integer(x) => write!(f, "{}", x),
            Object::Boolean(x) => write!(f, "{}", x),
            Object::Return(x) => write!(f, "{}", x.value),
            Object::Function(x) => write!(f, "fn ({:?}) {{\n{}\n}}", x.parameters, x.body),
            Object::Null => write!(f, "null"),
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(Rc::clone(&env)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(o) => o.borrow().get(name),
                _ => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Option<Object> {
        self.store.insert(name, value)
    }
}
