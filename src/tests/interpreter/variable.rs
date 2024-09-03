use crate::{
    interpreter::{EvaluationError, Interpreter},
    parser::ParseError,
    symantic_analysis::resolver::ResolutionError,
    tests::test_positive_test,
};

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

#[test]
fn in_middle_of_block() {
    let source = include_str!("../../../lox-test/variable/in_middle_of_block.lox").to_string();
    let expected = "a\na b\na c\na b d\n";
    test_positive_test(source, expected);
}

#[test]
fn in_nested_block() {
    let source = include_str!("../../../lox-test/variable/in_nested_block.lox").to_string();
    let expected = "outer\n";
    test_positive_test(source, expected);
}

#[test]
fn redeclare_global() {
    let source = include_str!("../../../lox-test/variable/redeclare_global.lox").to_string();
    let expected = "nil\n";
    test_positive_test(source, expected);
}

#[test]
fn redefine_global() {
    let source = include_str!("../../../lox-test/variable/redefine_global.lox").to_string();
    let expected = "2\n";
    test_positive_test(source, expected);
}

#[test]
fn scope_reuse_in_different_blocks() {
    let source =
        include_str!("../../../lox-test/variable/scope_reuse_in_different_blocks.lox").to_string();
    let expected = "first\nsecond\n";
    test_positive_test(source, expected);
}

#[test]
fn shadow_and_local() {
    let source = include_str!("../../../lox-test/variable/shadow_and_local.lox").to_string();
    let expected = "outer\ninner\n";
    test_positive_test(source, expected);
}
#[test]
fn shadow_global() {
    let source = include_str!("../../../lox-test/variable/shadow_global.lox").to_string();
    let expected = "shadow\nglobal\n";
    test_positive_test(source, expected);
}

#[test]
fn shadow_local() {
    let source = include_str!("../../../lox-test/variable/shadow_local.lox").to_string();
    let expected = "shadow\nlocal\n";
    test_positive_test(source, expected);
}

#[test]
fn undefined_global() {
    let source = include_str!("../../../lox-test/variable/undefined_global.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    match interpreter.evaluate() {
        Ok(_) => panic!("should have erred out"),
        Err(e) => match e {
            EvaluationError::UndefinedVariable { .. } => (),
            e => {
                panic!("Should have been EvaluationError::UndefinedVariable but instead got {e:?}")
            }
        },
    }
}
#[test]
fn undefined_local() {
    let source = include_str!("../../../lox-test/variable/undefined_local.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    match interpreter.evaluate_program() {
        Ok(_) => panic!("should have erred out"),
        Err(e) => match e {
            EvaluationError::UndefinedVariable { .. } => (),
            e => {
                panic!("Should have been EvaluationError::UndefinedVariable but instead got {e:?}")
            }
        },
    }
}

#[test]
fn uninitialized() {
    let source = include_str!("../../../lox-test/variable/uninitialized.lox").to_string();
    let expected = "nil\n";
    test_positive_test(source, expected);
}

#[test]
fn unreached_undefined() {
    let source = include_str!("../../../lox-test/variable/unreached_undefined.lox").to_string();
    let expected = "ok\n";
    test_positive_test(source, expected);
}

#[test]
fn use_false_as_var() {
    let source = include_str!("../../../lox-test/variable/use_false_as_var.lox").to_string();
    let writer = vec![];

    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    match interpreter.evaluate_program() {
        Err(e) => match e {
            EvaluationError::ParseError(e) => println!("got parse error: {e:?}"),
            e => panic!("expected ParseError, but got {e:?}"),
        },
        Ok(()) => panic!("expected to err out"),
    }
}

#[test]
fn use_global_in_initializer() {
    let source =
        include_str!("../../../lox-test/variable/use_global_in_initializer.lox").to_string();
    let expected = "value\n";
    test_positive_test(source, expected);
}

#[test]
fn use_local_in_initializer() {
    let source =
        include_str!("../../../lox-test/variable/use_local_in_initializer.lox").to_string();
    let writer = vec![];

    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    match interpreter.evaluate_program() {
        Err(e) => match e {
            EvaluationError::ResolutionError(e) => match e {
                ResolutionError::ReferenceAVariableInItsInitializer(_) => (),
                e => panic!("expected EvaluationError::ResolutionError::ReferenceAVariableInItsInitializer, but got {e:?} ")
            },
            e => panic!("expected EvaluationError::ResolutionError, but got {e:?}"),
        },
        Ok(()) => panic!("expected to err out"),
    }
}

// TODO: once the `class` support is done. need to add more variable tests.

#[test]
fn use_nil_as_var() {
    let source = include_str!("../../../lox-test/variable/use_nil_as_var.lox").to_string();
    let writer = vec![];

    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    match interpreter.evaluate_program() {
        Err(e) => match e {
            EvaluationError::ParseError(e) => match e {
                ParseError::ExpectedTokenNotFound { .. } => (),
                e => panic!(
                    "expected EvaluationError::ParseError::ExpectedTokenNotFound, but found {e:?}"
                ),
            },
            e => panic!("expected ParseError, but got {e:?}"),
        },
        Ok(()) => panic!("expected to err out"),
    }
}
