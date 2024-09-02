#![allow(dead_code, unused_variables, unused_mut)]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use bytes::Bytes;

use crate::{
    parser::expression::{
        Expression, FunctionExpression, IdentExpression, IfStatement, Statement, VarDeclaration,
        WhileLoop,
    },
    token::Token,
};

pub(crate) enum ResolutionError {
    ReferenceAVariableInItsInitializer(Bytes),
    InValidHops,
}

impl std::fmt::Debug for ResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolutionError::ReferenceAVariableInItsInitializer(ident_bytes) => {
                let ident = unsafe { std::str::from_utf8_unchecked(ident_bytes.as_ref()) };
                write!(
                    f,
                    "Cant read local variable in its own initializer. {ident}"
                )
            }
            ResolutionError::InValidHops => write!(f, "Invalid hops found for resolution"),
        }
    }
}

pub(crate) struct Resolver {
    scopes: Vec<HashMap<Bytes, bool>>,
}

type ResolutionResult<T> = Result<T, ResolutionError>;

impl Resolver {
    pub(crate) fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    fn resolve_ident_expr(&self, ident_expr: &mut IdentExpression) -> ResolutionResult<()> {
        for (rev_index, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(ident_expr.name.as_ref()) {
                match ident_expr.resolve_hops.as_mut() {
                    None => {
                        let ident =
                            unsafe { std::str::from_utf8_unchecked(ident_expr.name.as_ref()) };
                        ident_expr.resolve_hops = Some(rev_index);
                    }
                    Some(t) => {
                        let name =
                            unsafe { std::str::from_utf8_unchecked(ident_expr.name.as_ref()) };
                        unreachable!(
                            "an ident: {name} expression shouldn't be resolved more than once"
                        )
                    }
                }
                return Ok(());
            }
        }
        Ok(())
    }

    fn resolve_function_expression(
        &mut self,
        func_expr: Rc<RefCell<FunctionExpression>>,
    ) -> ResolutionResult<()> {
        let mut func_expr = func_expr.as_ref().borrow_mut();
        if let Some(token) = &func_expr.name {
            match token {
                Token::Identifier(ident_bytes) => {
                    self.declare_var(ident_bytes.clone())?;
                    self.define_var(ident_bytes.clone());
                }
                token => unreachable!("name token must be an identifier"),
            }
        }

        self.begin_scope();
        if let Some(parameters) = &func_expr.parameters {
            for token in parameters.iter() {
                match token {
                    Token::Identifier(ident_bytes) => {
                        self.declare_var(ident_bytes.clone())?;
                        self.define_var(ident_bytes.clone());
                    }
                    t => unreachable!("function parameters must be identifier tokens"),
                }
            }
        }
        for stmt in func_expr.body.iter_mut() {
            self.resolve_stmt(stmt)?;
        }
        self.end_scope();
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expression) -> ResolutionResult<()> {
        match expr {
            Expression::NilLiteral
            | Expression::BooleanLiteral(_)
            | Expression::NumberLiteral(_)
            | Expression::StringLiteral(_) => {}
            Expression::Print(box_expr) => self.resolve_expr(box_expr)?,
            Expression::Ident(ident_expr) => {
                if let Some(last_scope) = self.scopes.last() {
                    match last_scope.get(ident_expr.as_ref()) {
                        Some(false) => {
                            return Err(ResolutionError::ReferenceAVariableInItsInitializer(
                                ident_expr.clone(),
                            ))
                        }
                        _ => (),
                    }
                }
                self.resolve_ident_expr(ident_expr)?;
            }
            Expression::GroupedExpression(grp_expr) => self.resolve_expr(grp_expr.as_mut())?,
            Expression::PrefixExpression { expr, .. } => self.resolve_expr(expr)?,
            Expression::InfixExpression {
                operator,
                left_expr,
                right_expr,
            } => {
                self.resolve_expr(left_expr)?;
                self.resolve_expr(right_expr)?;
            }
            Expression::Function(func_expr) => {
                self.resolve_function_expression(func_expr.clone())?;
            }
            Expression::Call(call_expr) => {
                self.resolve_expr(&mut call_expr.callee)?;
                if let Some(args) = &mut call_expr.arguments {
                    for arg in args.iter_mut() {
                        self.resolve_expr(arg)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_var(&mut self, ident_bytes: Bytes) -> ResolutionResult<()> {
        match self.scopes.last_mut() {
            None => Ok(()),
            Some(scope) => {
                scope.insert(ident_bytes, false);
                Ok(())
            } // None => Ok(()),
              // Some(true) => Ok(()),
              // Some(false) => Err(ResolutionError::ReferenceAVariableInItsInitializer(
              //     ident_bytes.clone(),
              // )),
        }
    }

    fn define_var(&mut self, ident_bytes: Bytes) {
        match self.scopes.last_mut() {
            Some(scope) => match scope.get_mut(ident_bytes.as_ref()) {
                None => unreachable!(),
                Some(val) => {
                    *val = true;
                }
            },
            None => {}
        }
    }

    fn resolve_stmt(&mut self, stmt: &mut Statement) -> ResolutionResult<()> {
        let _ = match stmt {
            Statement::Expression(expr) | Statement::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Statement::VarDeclaration(VarDeclaration { expr, identifier }) => {
                self.declare_var(identifier.clone())?;
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
                self.define_var(identifier.clone());
            }
            Statement::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve_stmt(stmt)?;
                }
                self.end_scope();
            }
            Statement::IfStatement(if_stmt) => {
                let IfStatement {
                    expr,
                    if_block,
                    else_block,
                } = if_stmt.as_mut();
                self.resolve_expr(expr)?;
                self.resolve_stmt(if_block)?;
                if let Some(else_block) = else_block {
                    self.resolve_stmt(else_block)?;
                }
            }
            Statement::WhileLoop(WhileLoop { expr, block }) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?
                }
                self.resolve_stmt(block)?;
            }
            Statement::Return(expr) => {
                self.resolve_expr(expr)?;
            }
        };
        Ok(())
    }

    pub(crate) fn resolve<'a, T: Iterator<Item = &'a mut Statement>>(
        mut self,
        stmts: T,
    ) -> ResolutionResult<()> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }
}
