use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::ast::{BlockStatement, IdentifierExpression};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Return(Rc<ReturnValue>),
    Function(Function),
    BuiltIn(BuiltIn),
    Array(Array),
    Null,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct ReturnValue {
    pub value: Object,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = match self {
            Object::Integer(x) => write!(f, "{}", x),
            Object::Boolean(x) => write!(f, "{}", x),
            Object::Return(x) => write!(f, "{}", x.value),
            Object::String(x) => write!(f, "{}", x),
            Object::Function(x) => write!(f, "fn ({:?}) {{\n{}\n}}", x.parameters, x.body),
            Object::BuiltIn(x) => write!(f, "{:?}", x),
            Object::Array(x) => write!(f, "[{:?}]", x.elements),
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BuiltIn {
    Len,
    First,
    Last,
    Rest,
    Push,
}

impl BuiltIn {
    pub fn lookup(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::BuiltIn(BuiltIn::Len)),
            "first" => Some(Object::BuiltIn(BuiltIn::First)),
            "last" => Some(Object::BuiltIn(BuiltIn::Last)),
            "rest" => Some(Object::BuiltIn(BuiltIn::Rest)),
            "push" => Some(Object::BuiltIn(BuiltIn::Push)),
            _ => None,
        }
    }

    pub fn apply(&self, arg: &Vec<Object>) -> Result<Object, String> {
        match self {
            BuiltIn::Len => {
                if arg.len() != 1 {
                    return Err("len takes only 1 argument".to_string());
                }

                match &arg[0] {
                    Object::String(s) => Ok(Object::Integer(s.len() as i64)),
                    Object::Array(a) => Ok(Object::Integer(a.elements.len() as i64)),
                    _ => Err(format!("argument to `len` not supported, got {:?}", arg[0])),
                }
            }
            BuiltIn::First => {
                if arg.len() != 1 {
                    return Err("first takes only 1 argument".to_string());
                }

                match &arg[0] {
                    Object::Array(a) => Ok(a.elements.first().unwrap().clone()),
                    _ => Err(format!(
                        "argument of `first` must be ARRAY, got {}",
                        &arg[0]
                    )),
                }
            }
            BuiltIn::Last => {
                if arg.len() != 1 {
                    return Err("last takes only 1 argument".to_string());
                }

                match &arg[0] {
                    Object::Array(a) => Ok(a.elements.last().unwrap().clone()),
                    _ => Err(format!("argument of `last` must be ARRAY, got {}", &arg[0])),
                }
            }
            BuiltIn::Rest => {
                if arg.len() != 1 {
                    return Err("rest takes only 1 argument".to_string());
                }
                match &arg[0] {
                    Object::Array(a) => Ok(Object::Array(Array {
                        elements: a.elements[1..].to_vec(),
                    })),
                    _ => Err(format!("argument of `last` must be ARRAY, got {}", &arg[0])),
                }
            }
            BuiltIn::Push => {
                if arg.len() != 2 {
                    return Err("push takes only 2 argument".to_string());
                }

                match &arg[0] {
                    Object::Array(a) => {
                        let mut elements = a.elements.clone();
                        elements.push(arg[1].clone());
                        Ok(Object::Array(Array { elements }))
                    }
                    _ => Err(format!("argument of `last` must be ARRAY, got {}", &arg[0])),
                }
            }
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
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
