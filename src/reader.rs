//! S-expression reader/parser

use crate::types::{Value, LispError};

/// Reader state
pub struct Reader<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Reader<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    /// Peek at current character
    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    /// Advance and return current character
    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    /// Skip whitespace and comments
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else if ch == ';' {
                // Skip comment to end of line
                while let Some(c) = self.advance() {
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    /// Read a single S-expression
    pub fn read(&mut self) -> Result<Value, LispError> {
        self.skip_whitespace();

        match self.peek() {
            None => Err(LispError::EndOfInput),
            Some('(') => self.read_list(),
            Some('\'') => self.read_quote(),
            Some('"') => self.read_string(),
            Some(ch) if ch == '-' || ch == '+' || ch.is_ascii_digit() => {
                self.read_number_or_symbol()
            }
            Some(_) => self.read_symbol(),
        }
    }

    /// Read a list
    fn read_list(&mut self) -> Result<Value, LispError> {
        self.advance(); // consume '('
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();
            match self.peek() {
                None => return Err(LispError::Syntax("Unclosed list".to_string())),
                Some(')') => {
                    self.advance();
                    return Ok(Value::from_vec(items));
                }
                Some('.') => {
                    // Dotted pair
                    self.advance();
                    self.skip_whitespace();

                    // Check if this is actually a symbol starting with '.'
                    if let Some(ch) = self.peek() {
                        if ch == ')' || ch.is_whitespace() {
                            // It's a dotted pair
                            let cdr = self.read()?;
                            self.skip_whitespace();
                            if self.peek() != Some(')') {
                                return Err(LispError::Syntax(
                                    "Expected ')' after dotted pair".to_string(),
                                ));
                            }
                            self.advance();
                            // Build the dotted list
                            let mut result = cdr;
                            for item in items.into_iter().rev() {
                                result = Value::cons(item, result);
                            }
                            return Ok(result);
                        }
                    }
                    // It's a symbol starting with '.'
                    self.pos -= 1; // back up
                    items.push(self.read_symbol()?);
                }
                _ => {
                    items.push(self.read()?);
                }
            }
        }
    }

    /// Read a quoted expression
    fn read_quote(&mut self) -> Result<Value, LispError> {
        self.advance(); // consume '\''
        let quoted = self.read()?;
        Ok(Value::from_vec(vec![
            Value::Symbol("QUOTE".to_string()),
            quoted,
        ]))
    }

    /// Read a string literal (converted to a list of fixnums for Z80)
    fn read_string(&mut self) -> Result<Value, LispError> {
        self.advance(); // consume '"'
        let mut chars = Vec::new();

        loop {
            match self.advance() {
                None => return Err(LispError::Syntax("Unclosed string".to_string())),
                Some('"') => break,
                Some('\\') => {
                    // Escape sequence
                    match self.advance() {
                        Some('n') => chars.push(Value::Fixnum(10)),
                        Some('r') => chars.push(Value::Fixnum(13)),
                        Some('t') => chars.push(Value::Fixnum(9)),
                        Some('\\') => chars.push(Value::Fixnum(92)),
                        Some('"') => chars.push(Value::Fixnum(34)),
                        Some(c) => chars.push(Value::Fixnum(c as i16)),
                        None => return Err(LispError::Syntax("Unexpected end of string".to_string())),
                    }
                }
                Some(c) => chars.push(Value::Fixnum(c as i16)),
            }
        }

        Ok(Value::from_vec(chars))
    }

    /// Read a number or symbol (handles negative numbers)
    fn read_number_or_symbol(&mut self) -> Result<Value, LispError> {
        let start = self.pos;

        // Consume sign if present
        if let Some(ch) = self.peek() {
            if ch == '-' || ch == '+' {
                self.advance();
            }
        }

        // Check if followed by digits
        let has_digits = self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false);

        if has_digits {
            // It's a number
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }

            let num_str = &self.input[start..self.pos];
            match num_str.parse::<i16>() {
                Ok(n) => Ok(Value::Fixnum(n)),
                Err(_) => Err(LispError::Syntax(format!("Invalid number: {}", num_str))),
            }
        } else {
            // It's a symbol starting with +/-
            self.pos = start;
            self.read_symbol()
        }
    }

    /// Read a symbol
    fn read_symbol(&mut self) -> Result<Value, LispError> {
        let start = self.pos;

        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || ch == '(' || ch == ')' || ch == '\'' || ch == '"' || ch == ';' {
                break;
            }
            self.advance();
        }

        if self.pos == start {
            return Err(LispError::Syntax("Expected symbol".to_string()));
        }

        let name = self.input[start..self.pos].to_uppercase();

        // Handle special symbols
        match name.as_str() {
            "NIL" => Ok(Value::Nil),
            "T" => Ok(Value::T),
            _ => Ok(Value::Symbol(name)),
        }
    }

    /// Check if there's more input
    pub fn has_more(&mut self) -> bool {
        self.skip_whitespace();
        self.peek().is_some()
    }
}

/// Parse a string into a LISP value
pub fn read(input: &str) -> Result<Value, LispError> {
    let mut reader = Reader::new(input);
    reader.read()
}

/// Parse all expressions in a string
pub fn read_all(input: &str) -> Result<Vec<Value>, LispError> {
    let mut reader = Reader::new(input);
    let mut results = Vec::new();

    while reader.has_more() {
        results.push(reader.read()?);
    }

    Ok(results)
}
