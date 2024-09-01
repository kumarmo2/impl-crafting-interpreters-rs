#![allow(dead_code, unused_variables, unused_mut)]

use std::collections::HashMap;

use bytes::Bytes;

use crate::parser::expression::{Expression, IfStatement, Statement, VarDeclaration, WhileLoop};

pub(crate) struct Resolver<'a, T>
where
    T: Iterator<Item = &'a mut Statement>,
{
    statements: T,
    scopes: Vec<HashMap<Bytes, bool>>,
}

impl<'a, T: Iterator<Item = &'a mut Statement>> Resolver<'a, T> {
    pub(crate) fn new(statements: T) -> Self {
        Self {
            statements,
            scopes: Vec::new(),
        }
    }

    fn resolve_expr(&self, expr: &mut Expression) {
        match expr {
            Expression::NilLiteral
            | Expression::BooleanLiteral(_)
            | Expression::NumberLiteral(_)
            | Expression::StringLiteral(_) => {}
            Expression::Print(box_expr) => self.resolve_expr(box_expr.as_mut()),
            Expression::Ident(_) => todo!(),
            Expression::GroupedExpression(grp_expr) => self.resolve_expr(grp_expr.as_mut()),
            Expression::PrefixExpression { expr, .. } => self.resolve_expr(expr),
            Expression::InfixExpression {
                operator,
                left_expr,
                right_expr,
            } => todo!(),
            Expression::Function(_) => todo!(),
            Expression::Call(_) => todo!(),
        }
    }

    pub(crate) fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(crate) fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_stmt(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Expression(expr) | Statement::Print(expr) => self.resolve_expr(expr),
            Statement::VarDeclaration(VarDeclaration { expr, identifier }) => {
                todo!()
            }
            Statement::Block(stmts) => {
                self.begin_scope();
                self.resolve_stmt(stmt);
                self.end_scope();
            }
            Statement::IfStatement(if_stmt) => {
                let IfStatement {
                    expr,
                    if_block,
                    else_block,
                } = if_stmt.as_mut();
                self.resolve_expr(expr);
                self.resolve_stmt(if_block);
                if let Some(else_block) = else_block {
                    self.resolve_stmt(else_block);
                }
            }
            Statement::WhileLoop(WhileLoop { expr, block }) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)
                }
                self.resolve_stmt(block);
            }
            Statement::Return(expr) => self.resolve_expr(expr),
        }
    }

    pub(crate) fn resolve(&mut self) {
        for stmt in &mut self.statements {}
        todo!()
    }
}
