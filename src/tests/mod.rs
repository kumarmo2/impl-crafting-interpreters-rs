use core::panic;

use crate::{
    parser::{
        expression::{Expression, Statement},
        Parser,
    },
    token::Scanner,
};

#[test]
fn it_works() {
    let src = "5;".to_string();

    let mut parser = Parser::from_source(src).unwrap();
    let statements = parser.parse_program().unwrap();
    assert_eq!(1, statements.len());

    let stmt = &statements[0];
    let expr = match stmt {
        Statement::Expression(expr) => expr,
        stmt => panic!("expected ExpressionStatement, found : {stmt:?}"),
    };

    let _ = match expr {
        Expression::NumberLiteral(val) => val,
        expr => panic!("expected NumberLiteral, found: {expr:?}"),
    };
}

#[test]
fn scanning_number_positive_tests() {
    let source = include_str!("../../lox-test/scanning/numbers.lox");
    scanning_test_from_source(source);
}

#[test]
fn scanning_punctuators_positive_tests() {
    let source = include_str!("../../lox-test/scanning/punctuators.lox");
    scanning_test_from_source(source);
}

#[test]
fn scanning_strings_positive_tests() {
    let source = include_str!("../../lox-test/scanning/strings.lox");
    scanning_test_from_source(source);
}

#[test]
fn scanning_identifiers_positive_tests() {
    let source = include_str!("../../lox-test/scanning/identifiers.lox");
    scanning_test_from_source(source);
}

#[test]
fn scanning_keywords_positive_tests() {
    let source = include_str!("../../lox-test/scanning/keywords.lox");
    scanning_test_from_source(source);
}

fn scanning_test_from_source(source: &str) {
    let (source, expects) = source.split_once("\n\n").unwrap();
    let expect_lines = expects
        .split("\n")
        .filter(|line| line.len() > 11)
        .map(|line| &line[11..])
        .collect::<Vec<_>>();

    let scanner = Scanner::new(source.to_string());
    let mut tokens = scanner.iter();

    let expect_lines = expect_lines.into_iter();
    for expect in expect_lines {
        let token = tokens.next().unwrap().unwrap();
        let dbg_print = format!("{token:?}");
        assert_eq!(dbg_print, expect);
    }
}

