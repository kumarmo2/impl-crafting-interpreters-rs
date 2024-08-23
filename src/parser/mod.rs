#![allow(dead_code)]

use expression::{Expression, Precedence, Statement, VarDeclaration};

use crate::token::{LexicalError, Scanner, Token, TokenIterator};
pub(crate) mod expression;

pub(crate) struct Parser {
    _scanner: Scanner,
    _token_iterator: TokenIterator,
    curr_token: Token,
    peek_token: Token,
}

pub(crate) enum ParseError {
    EmptySource,
    ImpossibleError,
    LexicalError(LexicalError),
    ExpectedTokenNotFound {
        expected: &'static str,
        got: Token,
        line: u32,
    },
    UnmatchedParentheses,
}

impl std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::EmptySource => write!(f, "EmptySource"),
            ParseError::ImpossibleError => write!(f, "ImpossibleError"),
            ParseError::LexicalError(e) => write!(f, "{:?}", e),
            ParseError::ExpectedTokenNotFound {
                line,
                got,
                expected,
            } => write!(f, "[line {line}] Error at '{got}': expect {expected}"),
            ParseError::UnmatchedParentheses => write!(f, "Error: Unmatched parentheses."),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub(crate) fn from_source(source: String) -> Result<Self, ParseError> {
        let scanner = Scanner::new(source);
        let mut token_iterator = scanner.iter();

        let curr_token = token_iterator
            .next()
            .ok_or(ParseError::EmptySource)?
            .map_err(|e| ParseError::LexicalError(e))?;

        if let Token::EOF = curr_token {
            return Err(ParseError::EmptySource);
        }

        let peek_token = token_iterator
            .next()
            .ok_or_else(|| unreachable!())?
            .map_err(|e| ParseError::LexicalError(e))?;

        Ok(Self {
            _scanner: scanner,
            _token_iterator: token_iterator,
            curr_token,
            peek_token,
        })
    }

    pub(crate) fn get_curr_line(&self) -> u32 {
        self._token_iterator.get_curr_line()
    }

    fn advance_token(&mut self) {
        let should_forward_peek_token = if let Token::EOF = self.peek_token {
            false
        } else {
            true
        };
        std::mem::swap(&mut self.curr_token, &mut self.peek_token);
        // eprintln!(">>advance_token called: curr_token: {:?}", self.curr_token);
        if should_forward_peek_token {
            // TODO: remove unwraps
            self.peek_token = self._token_iterator.next().unwrap().unwrap();
        }
    }

    fn parse_prefix_grouped_expression(&mut self) -> ParseResult<Expression> {
        if let Token::RParen = self.peek_token {
            return Err(ParseError::ExpectedTokenNotFound {
                expected: "expression",
                got: Token::RParen,
                line: self._token_iterator.get_curr_line(),
            });
        }
        self.advance_token();

        let expr = self.parse_expression(Precedence::Lowest)?;
        let Token::RParen = self.peek_token else {
            return Err(ParseError::UnmatchedParentheses);
        };
        self.advance_token();
        Ok(Expression::GroupedExpression(Box::new(expr)))
    }

    fn parse_prefix_operator_expression(&mut self) -> ParseResult<Expression> {
        let operator = self.curr_token.clone();
        self.advance_token();
        let expression = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::PrefixExpression {
            operator: operator,
            expr: Box::new(expression),
        })
    }

    fn parse_infix_operator_expression(
        &mut self,
        left_expr: Expression,
    ) -> ParseResult<Expression> {
        let operator = self.curr_token.clone();
        self.advance_token();
        let right_expr = self.parse_expression(operator.get_precedence())?;
        Ok(Expression::InfixExpression {
            operator: operator,
            left_expr: Box::new(left_expr),
            right_expr: Box::new(right_expr),
        })
    }

    #[allow(unused_variables)]
    pub(crate) fn parse_expression(
        &mut self,
        precendence: Precedence,
    ) -> Result<Expression, ParseError> {
        let mut left_expr = match self.curr_token.clone() {
            Token::True => Expression::BooleanLiteral(true),
            Token::False => Expression::BooleanLiteral(false),
            Token::NumberLiteral(val, _) => Expression::NumberLiteral(val),
            Token::StringLiteral(bytes) => Expression::StringLiteral(bytes.clone()),
            Token::LParen => self.parse_prefix_grouped_expression()?,
            Token::MINUS | Token::BANG => self.parse_prefix_operator_expression()?,
            Token::Identifier(ident_bytes) => Expression::Ident(ident_bytes.clone()),
            Token::Nil => {
                self.advance_token();
                Expression::NilLiteral
            }
            t => {
                return Err(ParseError::ExpectedTokenNotFound {
                    expected: "expression....",
                    got: t,
                    line: self._token_iterator.get_curr_line(),
                })
            }
        };

        loop {
            // eprintln!(
            //     "curr_token: {curr_token}, precendence: {precendence}",
            //     curr_token = self.curr_token,
            //     precendence = self.curr_token.get_precedence().value()
            // );
            if let Token::EOF = self.peek_token {
                // eprintln!("&&&&&&&&&&&&& found EOF");
                break;
            }
            if precendence.value() >= self.peek_token.get_precedence().value() {
                break;
            }
            left_expr = match self.peek_token.clone() {
                Token::PLUS
                | Token::MINUS
                | Token::SLASH
                | Token::STAR
                | Token::LESS
                | Token::LESSEQUAL
                | Token::GREATER
                | Token::GREATEREQUAL
                | Token::EQUALEQUAL
                | Token::BANGEQUAL => {
                    self.advance_token();
                    let expr = self.parse_infix_operator_expression(left_expr)?;
                    expr
                }
                t => unimplemented!("unimplemented for token: {}", t),
            }
        }

        Ok(left_expr)
    }

    fn ensure_semicolon_at_statement_end(&mut self) -> Result<(), ParseError> {
        match &self.peek_token {
            Token::SEMICOLON => {
                self.advance_token();
                Ok(())
            }
            _ => {
                return Err(ParseError::ExpectedTokenNotFound {
                    expected: ";",
                    got: self.peek_token.clone(),
                    line: self.get_curr_line(),
                });
            }
        }
    }

    fn parse_var_declaration(&mut self) -> Result<Statement, ParseError> {
        self.advance_token();
        let ident_bytes = match self.curr_token.clone() {
            Token::Identifier(iden_bytes) => iden_bytes,
            token => {
                return Err(ParseError::ExpectedTokenNotFound {
                    expected: "Identifier",
                    got: token,
                    line: self.get_curr_line(),
                })
            }
        };
        eprintln!(
            "found ident_bytes in var dev: {}, peek_token: {}",
            unsafe { std::str::from_utf8_unchecked(ident_bytes.as_ref()) },
            self.peek_token
        );
        match self.peek_token.clone() {
            Token::SEMICOLON => Ok(Statement::VarDeclaration(VarDeclaration {
                identifier: ident_bytes,
                expr: None,
            })),
            Token::EQUAL => {
                self.advance_token();
                eprintln!(
                    "found ====, curr_token before advanceing: {}",
                    self.curr_token
                );
                self.advance_token();
                eprintln!("curr_token after advanceing: {}", self.curr_token);
                let expr = self.parse_expression(Precedence::Lowest)?;
                Ok(Statement::VarDeclaration(VarDeclaration {
                    identifier: ident_bytes,
                    expr: Some(expr),
                }))
            }
            token => Err(ParseError::ExpectedTokenNotFound {
                expected: "expression",
                got: token,
                line: self.get_curr_line(),
            }),
        }
    }

    pub(crate) fn parse_program(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = vec![];
        loop {
            if let Token::EOF = self.curr_token {
                break;
            }
            match &self.curr_token {
                Token::Print => {
                    self.advance_token();
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    statements.push(Statement::Print(expr));
                }
                Token::Var => {
                    let dec = self.parse_var_declaration()?;
                    eprintln!("var dec: {:?}", dec);
                    statements.push(dec);
                }
                _ => {
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    statements.push(Statement::Expression(expr));
                }
            }
            self.ensure_semicolon_at_statement_end()?;
            eprintln!("%%%% before advancing: curr_token: {}", self.curr_token);
            self.advance_token();
            eprintln!("%%%% after advancing: curr_token: {}", self.curr_token);
        }
        Ok(statements)
    }
}
