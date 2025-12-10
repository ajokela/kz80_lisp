//! LISP interpreter core
//!
//! This implements the standard LISP evaluation/apply cycle for S-expressions.

use crate::types::{Value, Env, LispError};

/// Interpret a LISP expression in an environment
pub fn interpret(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    match expr {
        // Self-evaluating forms
        Value::Nil | Value::T | Value::Fixnum(_) | Value::Builtin(_) | Value::Lambda { .. } => {
            Ok(expr.clone())
        }

        // Symbol lookup
        Value::Symbol(name) => env
            .get(name)
            .ok_or_else(|| LispError::Unbound(name.clone())),

        // Function application or special form
        Value::Cons(_) => {
            let car = expr.car()?;

            // Check for special forms
            if let Value::Symbol(name) = &car {
                match name.as_str() {
                    "QUOTE" => return special_quote(expr),
                    "IF" => return special_if(expr, env),
                    "COND" => return special_cond(expr, env),
                    "LAMBDA" => return special_lambda(expr, env),
                    "DEFINE" => return special_define(expr, env),
                    "SETQ" => return special_setq(expr, env),
                    "LET" => return special_let(expr, env),
                    "PROGN" => return special_progn(expr, env),
                    "AND" => return special_and(expr, env),
                    "OR" => return special_or(expr, env),
                    _ => {}
                }
            }

            // Regular function application
            let func = interpret(&car, env)?;
            let args = interpret_args(&expr.cdr()?, env)?;
            apply(&func, &args, env)
        }
    }
}

/// Interpret arguments (for function calls)
fn interpret_args(args: &Value, env: &mut Env) -> Result<Vec<Value>, LispError> {
    let mut result = Vec::new();
    let mut current = args.clone();

    while !current.is_nil() {
        result.push(interpret(&current.car()?, env)?);
        current = current.cdr()?;
    }

    Ok(result)
}

/// Apply a function to arguments
/// The `calling_env` is the environment from which the function was called,
/// used for resolving recursive function references.
pub fn apply(func: &Value, args: &[Value], calling_env: &mut Env) -> Result<Value, LispError> {
    match func {
        Value::Builtin(f) => (f.func)(args, calling_env),
        Value::Lambda { params, body, env: closure_env } => {
            if params.len() != args.len() {
                return Err(LispError::WrongArity {
                    expected: params.len(),
                    got: args.len(),
                });
            }

            // Create new environment with parameter bindings
            // Use the calling environment as parent to support recursion
            // (the function can find itself in the calling environment)
            let mut local_env = Env::with_parent(calling_env.clone());

            // Also merge in any closed-over bindings from the closure environment
            // that aren't in the calling environment
            for (name, value) in closure_env.bindings_iter() {
                if calling_env.get(name).is_none() {
                    local_env.define(name, value.clone());
                }
            }

            for (param, arg) in params.iter().zip(args.iter()) {
                local_env.define(param, arg.clone());
            }

            interpret(body, &mut local_env)
        }
        _ => Err(LispError::NotAFunction(format!("{:?}", func))),
    }
}

/// (QUOTE expr) - return expr unevaluated
fn special_quote(expr: &Value) -> Result<Value, LispError> {
    let args = expr.cdr()?;
    args.car()
}

/// (IF test then else) - conditional
fn special_if(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let args = expr.cdr()?;
    let test = args.car()?;
    let rest = args.cdr()?;
    let then_clause = rest.car()?;
    let else_clause = rest.cdr()?.car().unwrap_or(Value::Nil);

    let test_result = interpret(&test, env)?;
    if !test_result.is_nil() {
        interpret(&then_clause, env)
    } else {
        interpret(&else_clause, env)
    }
}

/// (COND (test1 expr1) (test2 expr2) ...) - multi-way conditional
fn special_cond(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let mut clauses = expr.cdr()?;

    while !clauses.is_nil() {
        let clause = clauses.car()?;
        let test = clause.car()?;

        // Check for T (else clause)
        let test_result = if matches!(&test, Value::T) {
            Value::T
        } else {
            interpret(&test, env)?
        };

        if !test_result.is_nil() {
            // Interpret body expressions, return last
            let mut body = clause.cdr()?;
            let mut result = test_result;
            while !body.is_nil() {
                result = interpret(&body.car()?, env)?;
                body = body.cdr()?;
            }
            return Ok(result);
        }

        clauses = clauses.cdr()?;
    }

    Ok(Value::Nil)
}

/// (LAMBDA (params...) body) - create a function
fn special_lambda(expr: &Value, env: &Env) -> Result<Value, LispError> {
    let args = expr.cdr()?;
    let params_expr = args.car()?;
    let body = args.cdr()?.car()?;

    // Extract parameter names
    let mut params = Vec::new();
    let mut p = params_expr;
    while !p.is_nil() {
        if let Value::Symbol(name) = p.car()? {
            params.push(name);
        } else {
            return Err(LispError::Syntax("Lambda params must be symbols".to_string()));
        }
        p = p.cdr()?;
    }

    Ok(Value::Lambda {
        params,
        body: Box::new(body),
        env: env.clone(),
    })
}

/// (DEFINE name value) or (DEFINE (name args...) body) - define a variable or function
fn special_define(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let args = expr.cdr()?;
    let first = args.car()?;
    let rest = args.cdr()?;

    match first {
        Value::Symbol(name) => {
            // (DEFINE name value)
            let value = interpret(&rest.car()?, env)?;
            env.define(&name, value);
            Ok(Value::Symbol(name))
        }
        Value::Cons(_) => {
            // (DEFINE (name args...) body) - shorthand for lambda
            let name = if let Value::Symbol(n) = first.car()? {
                n
            } else {
                return Err(LispError::Syntax("Expected function name".to_string()));
            };

            let params_list = first.cdr()?;
            let body = rest.car()?;

            // Extract parameter names
            let mut params = Vec::new();
            let mut p = params_list;
            while !p.is_nil() {
                if let Value::Symbol(pname) = p.car()? {
                    params.push(pname);
                } else {
                    return Err(LispError::Syntax("Parameters must be symbols".to_string()));
                }
                p = p.cdr()?;
            }

            let lambda = Value::Lambda {
                params,
                body: Box::new(body),
                env: env.clone(),
            };

            env.define(&name, lambda);
            Ok(Value::Symbol(name))
        }
        _ => Err(LispError::Syntax("Invalid DEFINE form".to_string())),
    }
}

/// (SETQ name value) - set a variable
fn special_setq(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let args = expr.cdr()?;
    let name = if let Value::Symbol(n) = args.car()? {
        n
    } else {
        return Err(LispError::Syntax("SETQ requires a symbol".to_string()));
    };

    let value = interpret(&args.cdr()?.car()?, env)?;
    env.set(&name, value.clone())?;
    Ok(value)
}

/// (LET ((var1 val1) ...) body) - local bindings
fn special_let(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let args = expr.cdr()?;
    let bindings = args.car()?;
    let body = args.cdr()?;

    let mut local_env = Env::with_parent(env.clone());

    // Process bindings
    let mut b = bindings;
    while !b.is_nil() {
        let binding = b.car()?;
        let var = if let Value::Symbol(n) = binding.car()? {
            n
        } else {
            return Err(LispError::Syntax("LET binding must start with symbol".to_string()));
        };
        let val = interpret(&binding.cdr()?.car()?, env)?;
        local_env.define(&var, val);
        b = b.cdr()?;
    }

    // Interpret body
    let mut result = Value::Nil;
    let mut current = body;
    while !current.is_nil() {
        result = interpret(&current.car()?, &mut local_env)?;
        current = current.cdr()?;
    }

    Ok(result)
}

/// (PROGN expr...) - interpret expressions sequentially
fn special_progn(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let mut result = Value::Nil;
    let mut current = expr.cdr()?;

    while !current.is_nil() {
        result = interpret(&current.car()?, env)?;
        current = current.cdr()?;
    }

    Ok(result)
}

/// (AND expr...) - short-circuit AND
fn special_and(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let mut current = expr.cdr()?;
    let mut result = Value::T;

    while !current.is_nil() {
        result = interpret(&current.car()?, env)?;
        if result.is_nil() {
            return Ok(Value::Nil);
        }
        current = current.cdr()?;
    }

    Ok(result)
}

/// (OR expr...) - short-circuit OR
fn special_or(expr: &Value, env: &mut Env) -> Result<Value, LispError> {
    let mut current = expr.cdr()?;

    while !current.is_nil() {
        let result = interpret(&current.car()?, env)?;
        if !result.is_nil() {
            return Ok(result);
        }
        current = current.cdr()?;
    }

    Ok(Value::Nil)
}
