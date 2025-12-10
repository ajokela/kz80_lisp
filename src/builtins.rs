//! Built-in LISP functions

use crate::types::{Value, Env, LispError, BuiltinFn};
use crate::printer;

/// Register all built-in functions in an environment
pub fn register_builtins(env: &mut Env) {
    // List operations
    env.define("CAR", Value::Builtin(BuiltinFn { name: "CAR", func: builtin_car }));
    env.define("CDR", Value::Builtin(BuiltinFn { name: "CDR", func: builtin_cdr }));
    env.define("CONS", Value::Builtin(BuiltinFn { name: "CONS", func: builtin_cons }));
    env.define("LIST", Value::Builtin(BuiltinFn { name: "LIST", func: builtin_list }));
    env.define("LENGTH", Value::Builtin(BuiltinFn { name: "LENGTH", func: builtin_length }));
    env.define("APPEND", Value::Builtin(BuiltinFn { name: "APPEND", func: builtin_append }));
    env.define("REVERSE", Value::Builtin(BuiltinFn { name: "REVERSE", func: builtin_reverse }));
    env.define("NTH", Value::Builtin(BuiltinFn { name: "NTH", func: builtin_nth }));

    // Predicates
    env.define("NULL", Value::Builtin(BuiltinFn { name: "NULL", func: builtin_null }));
    env.define("ATOM", Value::Builtin(BuiltinFn { name: "ATOM", func: builtin_atom }));
    env.define("CONSP", Value::Builtin(BuiltinFn { name: "CONSP", func: builtin_consp }));
    env.define("NUMBERP", Value::Builtin(BuiltinFn { name: "NUMBERP", func: builtin_numberp }));
    env.define("SYMBOLP", Value::Builtin(BuiltinFn { name: "SYMBOLP", func: builtin_symbolp }));
    env.define("EQ", Value::Builtin(BuiltinFn { name: "EQ", func: builtin_eq }));
    env.define("EQUAL", Value::Builtin(BuiltinFn { name: "EQUAL", func: builtin_equal }));

    // Arithmetic
    env.define("+", Value::Builtin(BuiltinFn { name: "+", func: builtin_add }));
    env.define("-", Value::Builtin(BuiltinFn { name: "-", func: builtin_sub }));
    env.define("*", Value::Builtin(BuiltinFn { name: "*", func: builtin_mul }));
    env.define("/", Value::Builtin(BuiltinFn { name: "/", func: builtin_div }));
    env.define("MOD", Value::Builtin(BuiltinFn { name: "MOD", func: builtin_mod }));
    env.define("ABS", Value::Builtin(BuiltinFn { name: "ABS", func: builtin_abs }));

    // Comparisons
    env.define("=", Value::Builtin(BuiltinFn { name: "=", func: builtin_num_eq }));
    env.define("<", Value::Builtin(BuiltinFn { name: "<", func: builtin_lt }));
    env.define(">", Value::Builtin(BuiltinFn { name: ">", func: builtin_gt }));
    env.define("<=", Value::Builtin(BuiltinFn { name: "<=", func: builtin_le }));
    env.define(">=", Value::Builtin(BuiltinFn { name: ">=", func: builtin_ge }));
    env.define("ZEROP", Value::Builtin(BuiltinFn { name: "ZEROP", func: builtin_zerop }));

    // Logical
    env.define("NOT", Value::Builtin(BuiltinFn { name: "NOT", func: builtin_not }));

    // I/O
    env.define("PRINT", Value::Builtin(BuiltinFn { name: "PRINT", func: builtin_print }));
    env.define("PRINC", Value::Builtin(BuiltinFn { name: "PRINC", func: builtin_princ }));
    env.define("TERPRI", Value::Builtin(BuiltinFn { name: "TERPRI", func: builtin_terpri }));
}

// Helper to get exactly one argument
fn one_arg(args: &[Value], name: &str) -> Result<Value, LispError> {
    if args.len() != 1 {
        Err(LispError::WrongArity { expected: 1, got: args.len() })
    } else {
        Ok(args[0].clone())
    }
}

// Helper to get exactly two arguments
fn two_args(args: &[Value], name: &str) -> Result<(Value, Value), LispError> {
    if args.len() != 2 {
        Err(LispError::WrongArity { expected: 2, got: args.len() })
    } else {
        Ok((args[0].clone(), args[1].clone()))
    }
}

// Helper to extract fixnum
fn as_fixnum(v: &Value) -> Result<i16, LispError> {
    match v {
        Value::Fixnum(n) => Ok(*n),
        _ => Err(LispError::TypeError {
            expected: "fixnum",
            got: v.type_name().to_string(),
        }),
    }
}

// List operations

fn builtin_car(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    one_arg(args, "CAR")?.car()
}

fn builtin_cdr(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    one_arg(args, "CDR")?.cdr()
}

fn builtin_cons(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (car, cdr) = two_args(args, "CONS")?;
    Ok(Value::cons(car, cdr))
}

fn builtin_list(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    Ok(Value::from_vec(args.to_vec()))
}

fn builtin_length(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let list = one_arg(args, "LENGTH")?;
    let vec = list.to_vec()?;
    Ok(Value::Fixnum(vec.len() as i16))
}

fn builtin_append(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let mut result = Vec::new();
    for arg in args {
        if !arg.is_nil() {
            result.extend(arg.to_vec()?);
        }
    }
    Ok(Value::from_vec(result))
}

fn builtin_reverse(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let list = one_arg(args, "REVERSE")?;
    let mut vec = list.to_vec()?;
    vec.reverse();
    Ok(Value::from_vec(vec))
}

fn builtin_nth(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (n, list) = two_args(args, "NTH")?;
    let idx = as_fixnum(&n)? as usize;
    let vec = list.to_vec()?;
    if idx < vec.len() {
        Ok(vec[idx].clone())
    } else {
        Ok(Value::Nil)
    }
}

// Predicates

fn builtin_null(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "NULL")?;
    Ok(if v.is_nil() { Value::T } else { Value::Nil })
}

fn builtin_atom(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "ATOM")?;
    Ok(if v.is_atom() { Value::T } else { Value::Nil })
}

fn builtin_consp(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "CONSP")?;
    Ok(if v.is_cons() { Value::T } else { Value::Nil })
}

fn builtin_numberp(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "NUMBERP")?;
    Ok(if matches!(v, Value::Fixnum(_)) { Value::T } else { Value::Nil })
}

fn builtin_symbolp(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "SYMBOLP")?;
    Ok(if matches!(v, Value::Symbol(_)) { Value::T } else { Value::Nil })
}

fn builtin_eq(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, "EQ")?;
    let equal = match (&a, &b) {
        (Value::Nil, Value::Nil) => true,
        (Value::T, Value::T) => true,
        (Value::Fixnum(x), Value::Fixnum(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        _ => false,
    };
    Ok(if equal { Value::T } else { Value::Nil })
}

fn builtin_equal(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, "EQUAL")?;
    Ok(if values_equal(&a, &b) { Value::T } else { Value::Nil })
}

fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Nil, Value::Nil) => true,
        (Value::T, Value::T) => true,
        (Value::Fixnum(x), Value::Fixnum(y)) => x == y,
        (Value::Symbol(x), Value::Symbol(y)) => x == y,
        (Value::Cons(x), Value::Cons(y)) => {
            let (x_car, x_cdr) = {
                let borrowed = x.borrow();
                (borrowed.0.clone(), borrowed.1.clone())
            };
            let (y_car, y_cdr) = {
                let borrowed = y.borrow();
                (borrowed.0.clone(), borrowed.1.clone())
            };
            values_equal(&x_car, &y_car) && values_equal(&x_cdr, &y_cdr)
        }
        _ => false,
    }
}

// Arithmetic

fn builtin_add(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let mut sum: i16 = 0;
    for arg in args {
        sum = sum.wrapping_add(as_fixnum(arg)?);
    }
    Ok(Value::Fixnum(sum))
}

fn builtin_sub(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    if args.is_empty() {
        return Err(LispError::WrongArity { expected: 1, got: 0 });
    }
    if args.len() == 1 {
        return Ok(Value::Fixnum(-as_fixnum(&args[0])?));
    }
    let mut result = as_fixnum(&args[0])?;
    for arg in &args[1..] {
        result = result.wrapping_sub(as_fixnum(arg)?);
    }
    Ok(Value::Fixnum(result))
}

fn builtin_mul(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let mut product: i16 = 1;
    for arg in args {
        product = product.wrapping_mul(as_fixnum(arg)?);
    }
    Ok(Value::Fixnum(product))
}

fn builtin_div(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    if args.len() < 2 {
        return Err(LispError::WrongArity { expected: 2, got: args.len() });
    }
    let mut result = as_fixnum(&args[0])?;
    for arg in &args[1..] {
        let divisor = as_fixnum(arg)?;
        if divisor == 0 {
            return Err(LispError::DivisionByZero);
        }
        result /= divisor;
    }
    Ok(Value::Fixnum(result))
}

fn builtin_mod(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, "MOD")?;
    let divisor = as_fixnum(&b)?;
    if divisor == 0 {
        return Err(LispError::DivisionByZero);
    }
    Ok(Value::Fixnum(as_fixnum(&a)? % divisor))
}

fn builtin_abs(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "ABS")?;
    let n = as_fixnum(&v)?;
    Ok(Value::Fixnum(n.abs()))
}

// Comparisons

fn builtin_num_eq(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, "=")?;
    Ok(if as_fixnum(&a)? == as_fixnum(&b)? { Value::T } else { Value::Nil })
}

fn builtin_lt(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, "<")?;
    Ok(if as_fixnum(&a)? < as_fixnum(&b)? { Value::T } else { Value::Nil })
}

fn builtin_gt(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, ">")?;
    Ok(if as_fixnum(&a)? > as_fixnum(&b)? { Value::T } else { Value::Nil })
}

fn builtin_le(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, "<=")?;
    Ok(if as_fixnum(&a)? <= as_fixnum(&b)? { Value::T } else { Value::Nil })
}

fn builtin_ge(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let (a, b) = two_args(args, ">=")?;
    Ok(if as_fixnum(&a)? >= as_fixnum(&b)? { Value::T } else { Value::Nil })
}

fn builtin_zerop(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "ZEROP")?;
    Ok(if as_fixnum(&v)? == 0 { Value::T } else { Value::Nil })
}

// Logical

fn builtin_not(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "NOT")?;
    Ok(if v.is_nil() { Value::T } else { Value::Nil })
}

// I/O

fn builtin_print(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "PRINT")?;
    println!("{}", printer::print(&v));
    Ok(v)
}

fn builtin_princ(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    let v = one_arg(args, "PRINC")?;
    print!("{}", printer::print(&v));
    Ok(v)
}

fn builtin_terpri(args: &[Value], _env: &mut Env) -> Result<Value, LispError> {
    if !args.is_empty() {
        return Err(LispError::WrongArity { expected: 0, got: args.len() });
    }
    println!();
    Ok(Value::Nil)
}
