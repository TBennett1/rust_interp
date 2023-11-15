use core::fmt::*;

use crate::token;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(bool),
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
            Expression::None => write!(f, "let value not implemented"),
        }
    }
}

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

#[derive(PartialEq, Debug)]
pub struct PrefixExpression {
    pub operator: token::Token,
    pub right: Expression,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(PartialEq, Debug)]
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
