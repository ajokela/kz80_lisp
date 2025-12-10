//! S-expression printer

use crate::types::Value;
use std::fmt::Write;

/// Print a LISP value to a string
pub fn print(value: &Value) -> String {
    let mut output = String::new();
    print_to(value, &mut output);
    output
}

/// Print a LISP value to a writer
pub fn print_to(value: &Value, output: &mut String) {
    match value {
        Value::Nil => output.push_str("NIL"),
        Value::T => output.push_str("T"),
        Value::Fixnum(n) => write!(output, "{}", n).unwrap(),
        Value::Symbol(name) => output.push_str(name),
        Value::Cons(_) => print_list(value, output),
        Value::Builtin(f) => write!(output, "<builtin:{}>", f.name).unwrap(),
        Value::Lambda { params, .. } => {
            output.push_str("<lambda:(");
            for (i, p) in params.iter().enumerate() {
                if i > 0 {
                    output.push(' ');
                }
                output.push_str(p);
            }
            output.push_str(")>");
        }
    }
}

/// Print a list, handling proper vs improper lists
fn print_list(value: &Value, output: &mut String) {
    output.push('(');

    let mut current = value.clone();
    let mut first = true;

    while let Value::Cons(cell) = current {
        if !first {
            output.push(' ');
        }
        first = false;

        let (car, cdr) = {
            let borrowed = cell.borrow();
            (borrowed.0.clone(), borrowed.1.clone())
        };

        print_to(&car, output);
        current = cdr;
    }

    // Handle improper list (dotted pair)
    if !current.is_nil() {
        output.push_str(" . ");
        print_to(&current, output);
    }

    output.push(')');
}

/// Print a value with a newline
pub fn println(value: &Value) -> String {
    let mut s = print(value);
    s.push('\n');
    s
}
