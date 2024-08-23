use bytes::Bytes;

use crate::token::Token;

pub(crate) enum Expression {
    NilLiteral,
    BooleanLiteral(bool),
    NumberLiteral(f64),
    StringLiteral(Bytes),
    Ident(Bytes),
    GroupedExpression(Box<Expression>),
    PrefixExpression {
        operator: Token,
        expr: Box<Expression>,
    },
    InfixExpression {
        operator: Token,
        left_expr: Box<Expression>,
        right_expr: Box<Expression>,
    },
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::NilLiteral => write!(f, "nil"),
            Expression::BooleanLiteral(v) => write!(f, "{}", v),
            Expression::NumberLiteral(v) => write!(f, "{:?}", v),
            Expression::StringLiteral(bytes) => {
                let str = unsafe { std::str::from_utf8_unchecked(bytes.as_ref()) };
                write!(f, "{}", str)
            }
            Expression::GroupedExpression(e) => write!(f, "(group {:?})", e),
            Expression::PrefixExpression { operator, expr } => {
                write!(f, "({} {:?})", operator, expr)
            }
            Expression::InfixExpression {
                operator,
                left_expr,
                right_expr,
            } => write!(f, "({operator} {:?} {:?})", left_expr, right_expr),
            Expression::Ident(ident_bytes) => write!(f, "ident: {}", unsafe {
                std::str::from_utf8_unchecked(ident_bytes.as_ref())
            }),
        }
    }
}

#[derive(Clone)]
pub(crate) enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl Precedence {
    pub(crate) fn value(&self) -> u8 {
        self.clone() as u8
    }
}

pub(crate) struct VarDeclaration {
    pub(crate) identifier: Bytes,
    pub(crate) expr: Option<Expression>,
}

pub(crate) struct Assignment {
    pub(crate) identifier: Bytes,
    pub(crate) expr: Expression,
}

pub(crate) enum Statement {
    Expression(Expression),
    Print(Expression),
    VarDeclaration(VarDeclaration),
    Assignment(Assignment),
    Block(Vec<Box<Statement>>),
}

impl Statement {
    fn print_statements(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        prefix_whitespace: &str,
        statements: &Vec<Box<Statement>>,
    ) -> std::fmt::Result {
        for stmt in statements.iter() {
            match stmt.as_ref() {
                Statement::Block(stms) => {
                    write!(f, "{prefix_whitespace}{{\n")?;
                    let _ =
                        self.print_statements(f, format!("{prefix_whitespace}  ").as_str(), &stms)?;
                    write!(f, "{prefix_whitespace}}}\n")?;
                }
                st => write!(f, "{prefix_whitespace}{:?}\n", st)?,
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(e) => write!(f, "{:?};", e),
            Statement::Print(e) => write!(f, "print {:?};", e),
            Statement::VarDeclaration(VarDeclaration { identifier, expr }) => {
                let identifier = unsafe { std::str::from_utf8_unchecked(identifier.as_ref()) };
                match expr {
                    Some(expr) => write!(f, "var {} = {:?};", identifier, expr),
                    None => write!(f, "var {};", identifier),
                }
            }
            Statement::Assignment(Assignment { identifier, expr }) => {
                let identifier = unsafe { std::str::from_utf8_unchecked(identifier.as_ref()) };
                write!(f, "{identifier} = {:?}", expr)
            }
            Statement::Block(statements) => {
                let _ = write!(f, "{{\n")?;
                self.print_statements(f, "  ", statements)?;
                let _ = write!(f, "}}")?;

                Ok(())
            }
        }
    }
}
