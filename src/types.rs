//! Core LISP types for kz80_lisp
//!
//! Uses a tagged enum representation. For Z80 target, we'll generate
//! code that uses 16-bit cells with tagged pointers.

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

/// A LISP value
#[derive(Clone, Debug)]
pub enum Value {
    /// NIL - the empty list / false value
    Nil,
    /// T - the true value
    T,
    /// A small integer (fits in 13 bits for Z80: -4096 to 4095)
    Fixnum(i16),
    /// A symbol (interned string)
    Symbol(String),
    /// A cons cell (pair)
    Cons(Rc<RefCell<(Value, Value)>>),
    /// A built-in function
    Builtin(BuiltinFn),
    /// A user-defined function (lambda)
    Lambda {
        params: Vec<String>,
        body: Box<Value>,
        env: Env,
    },
}

/// A built-in function type
#[derive(Clone)]
pub struct BuiltinFn {
    pub name: &'static str,
    pub func: fn(&[Value], &mut Env) -> Result<Value, LispError>,
}

impl std::fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin:{}>", self.name)
    }
}

/// Environment for variable bindings
#[derive(Clone, Debug, Default)]
pub struct Env {
    bindings: HashMap<String, Value>,
    parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_parent(parent: Env) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.bindings.insert(name.to_string(), value);
    }

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), LispError> {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            Ok(())
        } else if let Some(ref mut parent) = self.parent {
            parent.set(name, value)
        } else {
            Err(LispError::Unbound(name.to_string()))
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.bindings.get(name) {
            Some(val.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    /// Iterate over bindings in this environment level (not parents)
    pub fn bindings_iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.bindings.iter()
    }
}

/// LISP errors
#[derive(Debug, Clone)]
pub enum LispError {
    Syntax(String),
    Unbound(String),
    NotAFunction(String),
    WrongArity { expected: usize, got: usize },
    TypeError { expected: &'static str, got: String },
    DivisionByZero,
    EndOfInput,
}

impl std::fmt::Display for LispError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LispError::Syntax(msg) => write!(f, "Syntax error: {}", msg),
            LispError::Unbound(name) => write!(f, "Unbound symbol: {}", name),
            LispError::NotAFunction(name) => write!(f, "Not a function: {}", name),
            LispError::WrongArity { expected, got } => {
                write!(f, "Wrong number of arguments: expected {}, got {}", expected, got)
            }
            LispError::TypeError { expected, got } => {
                write!(f, "Type error: expected {}, got {}", expected, got)
            }
            LispError::DivisionByZero => write!(f, "Division by zero"),
            LispError::EndOfInput => write!(f, "End of input"),
        }
    }
}

impl Value {
    /// Check if value is NIL
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    /// Check if value is a cons cell
    pub fn is_cons(&self) -> bool {
        matches!(self, Value::Cons(_))
    }

    /// Check if value is an atom (not a cons)
    pub fn is_atom(&self) -> bool {
        !self.is_cons()
    }

    /// Create a cons cell
    pub fn cons(car: Value, cdr: Value) -> Value {
        Value::Cons(Rc::new(RefCell::new((car, cdr))))
    }

    /// Get CAR of a cons cell
    pub fn car(&self) -> Result<Value, LispError> {
        match self {
            Value::Cons(cell) => Ok(cell.borrow().0.clone()),
            Value::Nil => Ok(Value::Nil),
            _ => Err(LispError::TypeError {
                expected: "cons",
                got: self.type_name().to_string(),
            }),
        }
    }

    /// Get CDR of a cons cell
    pub fn cdr(&self) -> Result<Value, LispError> {
        match self {
            Value::Cons(cell) => Ok(cell.borrow().1.clone()),
            Value::Nil => Ok(Value::Nil),
            _ => Err(LispError::TypeError {
                expected: "cons",
                got: self.type_name().to_string(),
            }),
        }
    }

    /// Get type name for error messages
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "nil",
            Value::T => "t",
            Value::Fixnum(_) => "fixnum",
            Value::Symbol(_) => "symbol",
            Value::Cons(_) => "cons",
            Value::Builtin(_) => "builtin",
            Value::Lambda { .. } => "lambda",
        }
    }

    /// Convert a list to a Vec
    pub fn to_vec(&self) -> Result<Vec<Value>, LispError> {
        let mut result = Vec::new();
        let mut current = self.clone();
        while !current.is_nil() {
            result.push(current.car()?);
            current = current.cdr()?;
        }
        Ok(result)
    }

    /// Create a list from a Vec
    pub fn from_vec(items: Vec<Value>) -> Value {
        let mut result = Value::Nil;
        for item in items.into_iter().rev() {
            result = Value::cons(item, result);
        }
        result
    }
}
