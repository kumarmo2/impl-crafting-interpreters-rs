use core::panic;

use crate::interpreter::{EvaluationError, Interpreter};

#[test]
fn empty_body() {
    let source = include_str!("../../../lox-test/function/empty_body.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    interpreter.evaluate_program().unwrap();
    assert_eq!(interpreter.writer(), b"nil\n");
}

#[test]
fn extra_arguments() {
    let source = include_str!("../../../lox-test/function/extra_arguments.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    match interpreter.evaluate_program() {
        Ok(_) => panic!("was supposed to return error but instead got success."),
        Err(err) => match err {
            EvaluationError::Runtime(s) => {
                assert_eq!(s.as_str(), "Expected 2 arguments but got 4.")
            }
            err => panic!("expected runtime error, but got {err:?}"),
        },
    };
}
