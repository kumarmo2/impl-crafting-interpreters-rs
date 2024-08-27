#![allow(dead_code, unused_variables)]

use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use bytes::{BufMut, Bytes, BytesMut};

use crate::{
    parser::{
        expression::{Expression, IfStatement, Precedence, Statement, VarDeclaration, WhileLoop},
        ParseError, Parser,
    },
    token::Token,
};

#[derive(Clone, Debug)]
pub(crate) enum Object {
    Number(f64),
    Boolean(bool),
    String(Bytes),
    Nil,
}

impl Object {
    pub(crate) fn get_truthy_value(&self) -> bool {
        match self {
            Object::Number(_) => true,
            Object::Boolean(v) => *v,
            Object::String(_) => true,
            Object::Nil => false,
        }
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Number(v) => write!(f, "{}", v),
            Object::Boolean(v) => write!(f, "{}", v),
            Object::Nil => write!(f, "nil"),
            Object::String(bytes) => {
                let str = unsafe { std::str::from_utf8_unchecked(bytes.as_ref()) };
                write!(f, "{}", str)
            }
        }
    }
}

type Env = Rc<RefCell<Environment>>;

#[derive(Default, Debug)]
pub(crate) struct Environment {
    values: HashMap<Bytes, Object>,
    parent_env: Option<Env>, // 'p_env >= 'env
}

impl Environment {
    pub(crate) fn add(&mut self, key: Bytes, val: Object) -> Option<Object> {
        self.values.insert(key, val)
    }

    pub(crate) fn assign(&mut self, key: Bytes, val: Object) -> Option<Object> {
        if self.values.contains_key(key.as_ref()) {
            return self.values.insert(key, val);
        }
        if let Some(parent_env) = self.parent_env.as_ref() {
            return parent_env.clone().as_ref().borrow_mut().assign(key, val);
        }
        unreachable!()
    }

    pub(crate) fn get<K: AsRef<[u8]>>(&self, key: K) -> Object {
        if self.values.contains_key(key.as_ref()) {
            return self
                .values
                .get(key.as_ref())
                .map_or(Object::Nil, |v| v.clone());
        }
        if let Some(parent_env) = &self.parent_env {
            return parent_env.as_ref().borrow().get(key.as_ref());
        }
        Object::Nil
    }

    pub(crate) fn is_declared<K: AsRef<[u8]>>(&self, key: K) -> bool {
        if self.values.contains_key(key.as_ref()) {
            return true;
        }
        if let Some(parent_env) = &self.parent_env {
            return parent_env.as_ref().borrow().is_declared(key);
        }
        false
    }
}

pub(crate) struct Interpreter<W>
where
    W: Write,
{
    writer: W,
    parser: Parser,
}

pub(crate) enum EvaluationError {
    ParseError(ParseError),
    ExpectedSomethingButGotOther {
        expected: &'static str,
        got: Object,
    },
    Runtime(String),
    InvalidOperation {
        left: Object,
        operator: Token,
        right: Object,
    },
    UndefinedVariable {
        identifier: Bytes,
    },
}

impl std::fmt::Debug for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluationError::ParseError(e) => write!(f, "{:?}", e),
            EvaluationError::ExpectedSomethingButGotOther { expected, got } => {
                write!(f, "expected: {expected}, but got: {got}")
            }
            EvaluationError::InvalidOperation {
                left,
                operator,
                right,
            } => write!(
                f,
                "InvalidOperation: {operator}, left: {left}, right: {right}"
            ),
            EvaluationError::Runtime(str) => write!(f, "runtime error: {str}"),
            EvaluationError::UndefinedVariable { identifier } => {
                let ident = unsafe { std::str::from_utf8_unchecked(identifier) };
                write!(f, "undefined variable '{ident}'")
            }
        }
    }
}
fn evaluate_string_infix_operation(
    operator: Token,
    left: &Bytes,
    right: &Bytes,
) -> Result<Object, EvaluationError> {
    match operator.clone() {
        Token::PLUS => {
            let mut buf = BytesMut::with_capacity(left.len() + right.len());
            buf.put(left.as_ref());
            buf.put(right.as_ref());
            let bytes = buf.freeze();
            Ok(Object::String(bytes))
        }
        Token::EQUALEQUAL => {
            let left = unsafe { std::str::from_utf8_unchecked(left.as_ref()) };
            let right = unsafe { std::str::from_utf8_unchecked(right.as_ref()) };
            Ok(Object::Boolean(left == right))
        }
        Token::BANGEQUAL => {
            let left = unsafe { std::str::from_utf8_unchecked(left.as_ref()) };
            let right = unsafe { std::str::from_utf8_unchecked(right.as_ref()) };
            Ok(Object::Boolean(left != right))
        }
        token => Err(EvaluationError::InvalidOperation {
            left: Object::String(left.clone()),
            operator,
            right: Object::String(right.clone()),
        }),
    }
}
fn evaluate_numeric_infix_operation(operator: Token, left_value: f64, right_value: f64) -> Object {
    match operator {
        Token::STAR => Object::Number(left_value * right_value),
        Token::SLASH => Object::Number(left_value / right_value),
        Token::PLUS => Object::Number(left_value + right_value),
        Token::MINUS => Object::Number(left_value - right_value),
        Token::EQUALEQUAL => Object::Boolean(left_value == right_value),
        Token::BANGEQUAL => Object::Boolean(left_value != right_value),
        Token::LESS => Object::Boolean(left_value < right_value),
        Token::LESSEQUAL => Object::Boolean(left_value <= right_value),
        Token::GREATER => Object::Boolean(left_value > right_value),
        Token::GREATEREQUAL => Object::Boolean(left_value >= right_value),
        token => unimplemented!("{token}"),
    }
}

fn evaluate_infix_expression_for_different_types_of_operands(
    operator: Token,
    left: &Object,
    right: &Object,
) -> Result<Object, EvaluationError> {
    match operator {
        Token::EQUALEQUAL => match (left, right) {
            (Object::Nil, Object::Nil) => Ok(Object::Boolean(true)),
            _ => Ok(Object::Boolean(false)),
        },
        Token::BANGEQUAL => Ok(Object::Boolean(true)),
        Token::PLUS => {
            return Err(EvaluationError::Runtime(format!(
                "Operands must be two numbers or two strings."
            )))
        }
        Token::MINUS
        | Token::SLASH
        | Token::STAR
        | Token::LESS
        | Token::LESSEQUAL
        | Token::GREATER
        | Token::GREATEREQUAL => Err(EvaluationError::Runtime(format!(
            "Error: Operands must be numbers." // TODO: need to print the line number as well.
        ))),
        _ => Err(EvaluationError::InvalidOperation {
            left: left.clone(),
            operator: operator,
            right: right.clone(),
        }),
    }
}
impl<W> Interpreter<W>
where
    W: Write,
{
    pub(crate) fn from_source(source: String, writer: W) -> Result<Self, ParseError> {
        let parser = Parser::from_source(source)?;

        Ok(Self {
            writer,
            parser,
            // global_env: Environment::default(),
        })
    }

    fn evaluate_and_expression(
        &mut self,
        left_expr: &Expression,
        right_expr: &Expression,
        env: Env,
    ) -> Result<Object, EvaluationError> {
        let left_value = self.evaluate_expression(left_expr, env.clone())?;
        if !left_value.get_truthy_value() {
            Ok(left_value)
        } else {
            self.evaluate_expression(right_expr, env.clone())
        }
    }
    fn evaluate_or_expression(
        &mut self,
        left_expr: &Expression,
        right_expr: &Expression,
        env: Env,
    ) -> Result<Object, EvaluationError> {
        let left_value = self.evaluate_expression(left_expr, env.clone())?;
        if left_value.get_truthy_value() {
            Ok(left_value)
        } else {
            self.evaluate_expression(right_expr, env.clone())
        }
    }

    fn evaluate_assignment_infix_expression(
        &mut self,
        left_expr: &Expression,
        right_expr: &Expression,
        env: Env,
    ) -> Result<Object, EvaluationError> {
        let ident_bytes = match left_expr {
            Expression::Ident(ident_bytes) => ident_bytes,
            expr => {
                return Err(EvaluationError::Runtime(format!(
                    "expected expression but got {expr:?}"
                )))
            }
        };
        let value = self.evaluate_expression(right_expr, env.clone())?;
        if !env.as_ref().borrow().is_declared(ident_bytes.as_ref()) {
            return Err(EvaluationError::UndefinedVariable {
                identifier: ident_bytes.clone(),
            });
        }
        env.as_ref()
            .borrow_mut()
            .assign(ident_bytes.clone(), value.clone());
        Ok(value)
    }

    fn evaluate_infix_expression(
        &mut self,
        operator: Token,
        left_expr: &Expression,
        right_expr: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Object, EvaluationError> {
        if let Token::EQUAL = operator {
            return self.evaluate_assignment_infix_expression(left_expr, right_expr, env);
        }
        if let Token::And = operator {
            return self.evaluate_and_expression(left_expr, right_expr, env);
        }
        if let Token::Or = operator {
            return self.evaluate_or_expression(left_expr, right_expr, env);
        }
        let left_value = self.evaluate_expression(left_expr, env.clone())?;
        let right_value = self.evaluate_expression(right_expr, env.clone())?;
        match (&left_value, &right_value) {
            (Object::Number(left), Object::Number(right)) => {
                Ok(evaluate_numeric_infix_operation(operator, *left, *right))
            }
            (Object::String(left), Object::String(right)) => {
                evaluate_string_infix_operation(operator, left, right)
            }
            (Object::Boolean(left), Object::Boolean(right)) => match operator.clone() {
                Token::EQUALEQUAL => Ok(Object::Boolean(*left == *right)),
                Token::BANGEQUAL => Ok(Object::Boolean(*left != *right)),
                token => Err(EvaluationError::InvalidOperation {
                    left: left_value,
                    operator: operator.clone(),
                    right: right_value,
                }),
            },
            _ => evaluate_infix_expression_for_different_types_of_operands(
                operator,
                &left_value,
                &right_value,
            ),
        }
    }

    fn evaluate_prefix_expression(
        &mut self,
        operator: Token,
        expression: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Object, EvaluationError> {
        let value = self.evaluate_expression(expression, env)?;
        let object = match operator {
            Token::BANG => Object::Boolean(!value.get_truthy_value()),
            Token::MINUS => match value {
                Object::Number(v) => Object::Number(-v),
                object => {
                    return Err(EvaluationError::Runtime(format!(
                        "Error: Operand must be a number.\n[line {}]",
                        self.parser.get_curr_line()
                    )))
                }
            },
            t => unreachable!("token: {}", t),
        };
        Ok(object)
    }

    fn evaluate_expression(
        &mut self,
        expression: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Object, EvaluationError> {
        let val = match expression {
            Expression::NilLiteral => Object::Nil,
            Expression::Ident(ident_bytes) => {
                if !env.as_ref().borrow().is_declared(ident_bytes) {
                    return Err(EvaluationError::UndefinedVariable {
                        identifier: ident_bytes.clone(),
                    });
                }
                env.as_ref().borrow().get(ident_bytes)
            }
            Expression::BooleanLiteral(v) => Object::Boolean(*v),
            Expression::NumberLiteral(v) => Object::Number(*v),
            Expression::StringLiteral(bytes) => Object::String(bytes.clone()),
            Expression::GroupedExpression(expr) => self.evaluate_expression(expr.as_ref(), env)?,
            Expression::PrefixExpression { operator, expr } => {
                self.evaluate_prefix_expression(operator.clone(), expr.as_ref(), env)?
            }
            Expression::InfixExpression {
                operator,
                left_expr,
                right_expr,
            } => self.evaluate_infix_expression(
                operator.clone(),
                left_expr.as_ref(),
                right_expr.as_ref(),
                env,
            )?,
            Expression::Print(e) => {
                let val = self.evaluate_expression(e.as_ref(), env.clone())?;
                let _ = writeln!(self.writer, "{}", val);
                Object::Nil
            }
        };
        Ok(val)
    }

    pub(crate) fn evaluate(&mut self) -> Result<Object, EvaluationError> {
        let env = Environment::default();
        let expression = self
            .parser
            .parse_expression(Precedence::Lowest)
            .or_else(|e| Err(EvaluationError::ParseError(e)))?;

        self.evaluate_expression(&expression, Rc::new(RefCell::new(env)))
    }

    pub(crate) fn evaluate_stmt(
        &mut self,
        stmt: &Statement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), EvaluationError> {
        match stmt {
            Statement::Expression(e) => {
                self.evaluate_expression(e, env)?;
            }
            Statement::Print(e) => {
                let val = self.evaluate_expression(e, env)?;
                let _ = writeln!(self.writer, "{}", val);
            }
            Statement::VarDeclaration(VarDeclaration { identifier, expr }) => {
                if let Some(expr) = expr {
                    let val = self.evaluate_expression(expr, env.clone())?;
                    env.as_ref().borrow_mut().add(identifier.clone(), val);
                } else {
                    env.as_ref()
                        .borrow_mut()
                        .add(identifier.clone(), Object::Nil);
                }
            }
            Statement::Block(stmts) => {
                let child_env = Rc::new(RefCell::new(Environment {
                    values: HashMap::new(),
                    parent_env: Some(env.clone()),
                }));
                for stmt in stmts.iter() {
                    self.evaluate_stmt(stmt.as_ref(), child_env.clone())?;
                }
            }
            Statement::IfStatement(if_statement) => {
                self.evaluate_if_statement(if_statement, env.clone())?
            }
            Statement::WhileLoop(while_loop) => {
                self.evaluate_while_statement(while_loop, env.clone())?
            }
        };
        Ok(())
    }
    fn evaluate_while_statement(
        &mut self,
        while_loop: &WhileLoop,
        env: Env,
    ) -> Result<(), EvaluationError> {
        loop {
            let mut val = true;
            if let Some(expr) = &while_loop.expr {
                let x = self.evaluate_expression(expr, env.clone())?;
                val = x.get_truthy_value();
            }
            if !val {
                break;
            }
            self.evaluate_stmt(while_loop.block.as_ref(), env.clone())?
        }

        Ok(())
    }
    fn evaluate_if_statement(
        &mut self,
        if_statement: &IfStatement,
        env: Env,
    ) -> Result<(), EvaluationError> {
        let expr = &if_statement.expr;
        let val = self.evaluate_expression(expr, env.clone())?;
        let val = val.get_truthy_value();
        if val {
            self.evaluate_stmt(&if_statement.if_block, env.clone())?
        } else if let Some(else_block) = &if_statement.else_block {
            self.evaluate_stmt(else_block, env.clone())?;
        }
        Ok(())
    }

    pub(crate) fn writer(&self) -> &W {
        &self.writer
    }

    pub(crate) fn evaluate_program(&mut self) -> Result<(), EvaluationError> {
        let statements = self
            .parser
            .parse_program()
            .or_else(|e| Err(EvaluationError::ParseError(e)))?;

        let global_env = Rc::new(RefCell::new(Environment::default()));

        for stmt in statements.iter() {
            self.evaluate_stmt(stmt, global_env.clone())?;
        }
        Ok(())
    }
}
