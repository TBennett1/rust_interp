use std::{fmt::*, rc::Rc};

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
