use std::{collections::HashMap, fmt::*, rc::Rc};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Return(Rc<ReturnValue>),
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
            Object::Null => write!(f, "null"),
        };
        Ok(())
    }
}

pub struct Environment {
    store: HashMap<String, Object>,
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
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.store.get(name).cloned()
    }

    pub fn set(&mut self, name: String, value: Object) -> Option<Object> {
        self.store.insert(name, value)
    }
}
