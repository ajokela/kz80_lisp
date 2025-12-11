//! kz80_lisp - A minimal LISP interpreter for RetroShield Z80
//!
//! This is a Rust-based LISP interpreter that can:
//! 1. Run natively for development/testing
//! 2. Generate Z80 binary output for the RetroShield

mod types;
mod reader;
mod printer;
mod interpret;
mod builtins;
mod codegen;

use std::io::{self, Write, BufRead};
use std::env;
use std::fs::File;
use types::{Env, LispError};
use interpret::interpret;

const VERSION: &str = "0.1.0";

fn main() {
    let args: Vec<String> = env::args().collect();

    // Check for --generate-z80 flag
    if args.len() > 1 && args[1] == "--generate-z80" {
        let output_file = args.get(2).map(|s| s.as_str()).unwrap_or("lisp.bin");
        generate_z80(output_file);
        return;
    }

    println!("kz80_lisp v{} - A minimal LISP for RetroShield Z80", VERSION);
    println!("Type (HELP) for help, (QUIT) to exit\n");

    let mut env = Env::new();
    builtins::register_builtins(&mut env);

    // Add HELP and QUIT as special commands
    env.define("HELP", types::Value::Builtin(types::BuiltinFn {
        name: "HELP",
        func: |_, _| {
            println!("kz80_lisp - A minimal LISP interpreter\n");
            println!("Special Forms:");
            println!("  (QUOTE x) or 'x  - Return x unevaluated");
            println!("  (IF test then else) - Conditional");
            println!("  (COND (test expr)...) - Multi-way conditional");
            println!("  (LAMBDA (args) body) - Create function");
            println!("  (DEFINE name value) - Define variable");
            println!("  (DEFINE (name args) body) - Define function");
            println!("  (SETQ name value) - Set variable");
            println!("  (LET ((var val)...) body) - Local bindings");
            println!("  (PROGN expr...) - Sequential execution");
            println!("  (AND expr...) - Short-circuit AND");
            println!("  (OR expr...) - Short-circuit OR\n");
            println!("List Functions:");
            println!("  CAR CDR CONS LIST LENGTH APPEND REVERSE NTH\n");
            println!("Predicates:");
            println!("  NULL ATOM CONSP NUMBERP SYMBOLP EQ EQUAL\n");
            println!("Arithmetic:");
            println!("  + - * / MOD ABS\n");
            println!("Comparisons:");
            println!("  = < > <= >= ZEROP\n");
            println!("I/O:");
            println!("  PRINT PRINC TERPRI\n");
            println!("Other:");
            println!("  NOT (QUIT) (HELP)");
            Ok(types::Value::Nil)
        },
    }));

    env.define("QUIT", types::Value::Builtin(types::BuiltinFn {
        name: "QUIT",
        func: |_, _| {
            std::process::exit(0);
        },
    }));

    // REPL - use BufReader to properly handle buffered input from pipes
    use std::io::BufRead;
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut stdout = io::stdout();

    loop {
        print!("> ");
        stdout.flush().unwrap();

        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {}
            Err(e) => {
                eprintln!("Read error: {}", e);
                break;
            }
        }

        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        // Handle multi-line input (count parens)
        let mut input = line.to_string();
        while !is_balanced(&input) {
            print!("  ");
            stdout.flush().unwrap();
            let mut more = String::new();
            match reader.read_line(&mut more) {
                Ok(0) => break,
                Ok(_) => {
                    input.push('\n');
                    input.push_str(more.trim());
                }
                Err(_) => break,
            }
        }

        // Parse and interpret
        match reader::read(&input) {
            Ok(expr) => {
                match interpret(&expr, &mut env) {
                    Ok(result) => {
                        println!("{}", printer::print(&result));
                    }
                    Err(e) => {
                        println!("Error: {}", e);
                    }
                }
            }
            Err(LispError::EndOfInput) => {
                // Empty input, ignore
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }
    }

    println!("\nGoodbye!");
}

/// Generate Z80 binary
fn generate_z80(output_file: &str) {
    use std::io::Write;

    println!("Generating Z80 LISP interpreter binary...");

    let binary = codegen::generate_z80_binary();

    let mut file = File::create(output_file).expect("Failed to create output file");
    file.write_all(&binary).expect("Failed to write binary");

    println!("Generated {} ({} bytes)", output_file, binary.len());
}

/// Check if parentheses are balanced (handles strings and comments)
fn is_balanced(s: &str) -> bool {
    let mut depth = 0i32;
    let mut in_string = false;
    let mut in_comment = false;
    let mut escape = false;

    for ch in s.chars() {
        // Newline ends comments
        if ch == '\n' {
            in_comment = false;
            continue;
        }

        // Skip everything inside comments
        if in_comment {
            continue;
        }

        // Handle escape sequences in strings
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' && in_string {
            escape = true;
            continue;
        }

        // Handle string delimiters
        if ch == '"' {
            in_string = !in_string;
            continue;
        }

        // Skip everything inside strings
        if in_string {
            continue;
        }

        // Semicolon starts a comment
        if ch == ';' {
            in_comment = true;
            continue;
        }

        // Count parentheses
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            _ => {}
        }
    }

    depth <= 0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(code: &str) -> Result<types::Value, LispError> {
        let mut env = Env::new();
        builtins::register_builtins(&mut env);
        let expr = reader::read(code)?;
        interpret(&expr, &mut env)
    }

    fn run_all(code: &str) -> Result<types::Value, LispError> {
        let mut env = Env::new();
        builtins::register_builtins(&mut env);
        let exprs = reader::read_all(code)?;
        let mut result = types::Value::Nil;
        for expr in exprs {
            result = interpret(&expr, &mut env)?;
        }
        Ok(result)
    }

    #[test]
    fn test_atoms() {
        assert!(matches!(run("42").unwrap(), types::Value::Fixnum(42)));
        assert!(matches!(run("-5").unwrap(), types::Value::Fixnum(-5)));
        assert!(matches!(run("NIL").unwrap(), types::Value::Nil));
        assert!(matches!(run("T").unwrap(), types::Value::T));
    }

    #[test]
    fn test_quote() {
        let result = run("'(1 2 3)").unwrap();
        assert!(result.is_cons());
    }

    #[test]
    fn test_arithmetic() {
        assert!(matches!(run("(+ 1 2 3)").unwrap(), types::Value::Fixnum(6)));
        assert!(matches!(run("(- 10 3)").unwrap(), types::Value::Fixnum(7)));
        assert!(matches!(run("(* 4 5)").unwrap(), types::Value::Fixnum(20)));
        assert!(matches!(run("(/ 20 4)").unwrap(), types::Value::Fixnum(5)));
    }

    #[test]
    fn test_list_ops() {
        assert!(matches!(run("(car '(1 2 3))").unwrap(), types::Value::Fixnum(1)));
        assert!(matches!(run("(length '(1 2 3))").unwrap(), types::Value::Fixnum(3)));
    }

    #[test]
    fn test_if() {
        assert!(matches!(run("(if t 1 2)").unwrap(), types::Value::Fixnum(1)));
        assert!(matches!(run("(if nil 1 2)").unwrap(), types::Value::Fixnum(2)));
    }

    #[test]
    fn test_define_and_lambda() {
        let result = run_all("(define (square x) (* x x)) (square 5)").unwrap();
        assert!(matches!(result, types::Value::Fixnum(25)));
    }

    #[test]
    fn test_recursion() {
        let code = r#"
            (define (factorial n)
              (if (<= n 1)
                  1
                  (* n (factorial (- n 1)))))
            (factorial 5)
        "#;
        let result = run_all(code).unwrap();
        assert!(matches!(result, types::Value::Fixnum(120)));
    }

    #[test]
    fn test_let() {
        let result = run("(let ((x 10) (y 20)) (+ x y))").unwrap();
        assert!(matches!(result, types::Value::Fixnum(30)));
    }
}
