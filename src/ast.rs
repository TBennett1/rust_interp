use core::fmt::*;
use std::{collections::HashMap, hash::Hash};

use crate::token;

#[derive(Debug)]
pub enum Node {
    Program(Box<Program>),
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Statement::Let(let_stmt) => write!(f, "{}", let_stmt),
            Statement::Return(exp) => write!(f, "return {};", exp),
            Statement::Expression(exp) => write!(f, "{}", exp),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(bool),
    If(Box<IfExpression>),
    Function(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
    String(String),
    Array(Box<ArrayLiteral>),
    Index(Box<IndexExpression>),
    Hash(HashLiteral),
    None,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::Integer(value) => write!(f, "{}", value),
            Expression::Prefix(pref) => write!(f, "({}{})", pref.operator, pref.right),
            Expression::Infix(infix) => write!(
                f,
                "({} {} {})",
                infix.left_value, infix.operator, infix.right_value
            ),
            Expression::Boolean(bool) => write!(f, "{}", bool),
            Expression::If(ifexp) => write!(f, "{}", ifexp),
            Expression::Function(fnlit) => write!(f, "{}", fnlit),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::String(s) => write!(f, "\"{}\"", s),
            Expression::Array(a) => write!(f, "{}", a),
            Expression::Index(i) => write!(f, "{}", i),
            Expression::Hash(h) => write!(f, "{}", h),
            Expression::None => write!(f, "let value not implemented"),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Eq)]
pub struct HashLiteral {
    pub pairs: HashMap<Expression, Expression>,
}

impl Hash for HashLiteral {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{{ {:?} }}", self.pairs)
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct IndexExpression {
    pub left: Expression,
    pub index: Expression,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]

pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let elements: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elements.join(", "))
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let args: Vec<String> = self.arguments.iter().map(|a| a.to_string()).collect();
        write!(f, "{}({})", self.function, args.join(", "))
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct FunctionLiteral {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "({}) {}", params.join(", "), self.body)
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct IfExpression {
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;

        if let Some(ref stmt) = self.alternative {
            write!(f, "else {}", stmt)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct BlockStatement {
    pub statments: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for stmt in &self.statments {
            write!(f, "{}", stmt)?
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct IdentifierExpression {
    pub name: String,
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.name)
    }
}

pub struct IntegerLiteral {
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct PrefixExpression {
    pub operator: token::Token,
    pub right: Expression,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Hash)]
pub struct InfixExpression {
    pub operator: token::Token,
    pub right_value: Expression,
    pub left_value: Expression,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "({} {} {})",
            self.left_value, self.operator, self.right_value
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display() {
        let p = Program {
            statements: vec![Statement::Let(LetStatement {
                name: "myVar".to_string(),
                value: Expression::Identifier("anotherVar".to_string()),
            })],
        };

        let expected = "let myVar = anotherVar;";

        if p.to_string() != expected {
            panic!("expected {} but got {} instead", expected, p)
        }
    }
}
