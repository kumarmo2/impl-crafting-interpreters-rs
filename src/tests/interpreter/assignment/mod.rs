use crate::interpreter::Interpreter;

#[test]
fn test_associativity() {
    let source = include_str!("../../../../lox-test/assignment/associativity.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    interpreter.evaluate_program().unwrap();
    assert_eq!(interpreter.writer(), b"c\nc\nc\n");
}

#[test]
fn test_local() {
    let source = include_str!("../../../../lox-test/assignment/local.lox").to_string();
    let writer = vec![];
    let mut interpreter = Interpreter::from_source(source, writer).unwrap();
    interpreter.evaluate_program().unwrap();
    assert_eq!(interpreter.writer(), b"before\nafter\narg\narg\n");
}
