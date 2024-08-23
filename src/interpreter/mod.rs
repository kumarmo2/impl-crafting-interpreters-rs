#![allow(dead_code, unused_variables)]

use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    marker::PhantomData,
    ops::Deref,
    rc::Rc,
};

use bytes::{BufMut, Bytes, BytesMut};

use crate::{
    parser::{
        expression::{Assignment, Expression, Precedence, Statement, VarDeclaration},
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

#[derive(Default, Debug)]
pub(crate) struct Environment {
    _phantom: PhantomData<()>,
    values: HashMap<Bytes, Object>,
    parent_env: Option<Rc<RefCell<Environment>>>, // 'p_env >= 'env
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

pub(crate) struct Interpreter {
    parser: Parser,
}

pub(crate) enum EvaluationError {
    ParseError(ParseError),
    ExpectedSomethingButGotOther {
        expected: &'static str,
        got: Object,
    },
    Adhoc(String),
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
            EvaluationError::Adhoc(str) => write!(f, "{str}"),
            EvaluationError::UndefinedVariable { identifier } => {
                let ident = unsafe { std::str::from_utf8_unchecked(identifier) };
                write!(f, "undefined variable '{ident}'")
            }
        }
    }
}

impl Interpreter {
    pub(crate) fn from_source(source: String) -> Result<Self, ParseError> {
        let parser = Parser::from_source(source)?;

        Ok(Self {
            parser,
            // global_env: Environment::default(),
        })
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

    fn evaluate_numeric_infix_operation(
        operator: Token,
        left_value: f64,
        right_value: f64,
    ) -> Object {
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
            Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::STAR
            | Token::LESS
            | Token::LESSEQUAL
            | Token::GREATER
            | Token::GREATEREQUAL => {
                Err(EvaluationError::Adhoc(format!("Operands must be numbers.")))
            }
            _ => Err(EvaluationError::InvalidOperation {
                left: left.clone(),
                operator: operator,
                right: right.clone(),
            }),
        }
    }

    fn evaluate_infix_expression(
        &self,
        operator: Token,
        left_expr: &Expression,
        right_expr: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Object, EvaluationError> {
        let left_value = self.evaluate_expression(left_expr, env.clone())?;
        let right_value = self.evaluate_expression(right_expr, env.clone())?;
        match (&left_value, &right_value) {
            (Object::Number(left), Object::Number(right)) => Ok(
                Interpreter::evaluate_numeric_infix_operation(operator, *left, *right),
            ),
            (Object::String(left), Object::String(right)) => {
                Interpreter::evaluate_string_infix_operation(operator, left, right)
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
            _ => Interpreter::evaluate_infix_expression_for_different_types_of_operands(
                operator,
                &left_value,
                &right_value,
            ),
        }
    }

    fn evaluate_prefix_expression(
        &self,
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
                    return Err(EvaluationError::Adhoc(format!(
                        "Operand must be a number.\n[line {}]",
                        self.parser.get_curr_line()
                    )))
                }
            },
            t => unreachable!("token: {}", t),
        };
        Ok(object)
    }

    fn evaluate_expression(
        &self,
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
        &self,
        stmt: &Statement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<(), EvaluationError> {
        match stmt {
            Statement::Expression(e) => {
                let _ = self.evaluate_expression(e, env);
            }
            Statement::Print(e) => {
                let val = self.evaluate_expression(e, env)?;
                println!("{}", val);
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
            Statement::Assignment(Assignment { identifier, expr }) => {
                if !env.as_ref().borrow().is_declared(identifier) {
                    return Err(EvaluationError::UndefinedVariable {
                        identifier: identifier.clone(),
                    });
                }
                let val = self.evaluate_expression(expr, env.clone())?;
                env.as_ref().borrow_mut().assign(identifier.clone(), val);
            }
            Statement::Block(stmts) => {
                let child_env = Rc::new(RefCell::new(Environment {
                    values: HashMap::new(),
                    parent_env: Some(env.clone()),
                    _phantom: PhantomData {},
                }));
                // println!("child_env: {:?}", child_env);
                for stmt in stmts.iter() {
                    self.evaluate_stmt(stmt.as_ref(), child_env.clone())?;
                }
            }
        };
        Ok(())
    }

    pub(crate) fn evaluate_program(&mut self) -> Result<(), EvaluationError> {
        let statements = self
            .parser
            .parse_program()
            .or_else(|e| Err(EvaluationError::ParseError(e)))?;

        let mut global_env = Rc::new(RefCell::new(Environment::default()));

        for stmt in statements.iter() {
            self.evaluate_stmt(stmt, global_env.clone())?;
        }
        Ok(())
    }
}
