use crate::{interpreter::Interpreter, tests::test_positive_test};

#[test]
fn collide_with_parameter() {
    let source = include_str!("../../../lox-test/variable/collide_with_parameter.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();

    match interpreter.evaluate_program() {
        Ok(_) => panic!("expected error but got success"),
        Err(e) => {
            println!("got error in test {e:?}");
        }
    }
}

#[test]
fn duplicate_local() {
    let source = include_str!("../../../lox-test/variable/duplicate_local.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();

    match interpreter.evaluate_program() {
        Ok(_) => panic!("expected error but got success"),
        Err(e) => match e {
            crate::interpreter::EvaluationError::ResolutionError(e) => match e {
                crate::symantic_analysis::resolver::ResolutionError::AlreadyDeclaredVariable(_) => {
                    ()
                }
                e => panic!("expected EvaluationError::EvaluationError::AlreadyDeclaredVariable, but got {e:?}"),
            },
            e => panic!(
                "expected EvaluationError::EvaluationError::AlreadyDeclaredVariable, but got {e:?}"
            ),
        },
    }
}

#[test]
fn duplicate_parameter() {
    let source = include_str!("../../../lox-test/variable/duplicate_parameter.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();

    match interpreter.evaluate_program() {
        Ok(_) => panic!("expected error but got success"),
        Err(e) => match e {
            crate::interpreter::EvaluationError::ResolutionError(e) => match e {
                crate::symantic_analysis::resolver::ResolutionError::AlreadyDeclaredVariable(_) => {
                    ()
                }
                e => panic!("expected EvaluationError::EvaluationError::AlreadyDeclaredVariable, but got {e:?}"),
            },
            e => panic!(
                "expected EvaluationError::EvaluationError::AlreadyDeclaredVariable, but got {e:?}"
            ),
        },
    }
}

#[test]
fn early_bound() {
    let source = include_str!("../../../lox-test/variable/early_bound.lox").to_string();
    let expected = "outer\nouter\n";
    test_positive_test(source, expected);
}
