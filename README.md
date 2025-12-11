# kz80_lisp

A minimal LISP interpreter and compiler for the RetroShield Z80. This project provides both a Rust-based development interpreter and a native Z80 LISP system that runs on actual Z80 hardware.

## Features

- **Rust Interpreter**: Full LISP interpreter for development and testing
- **Z80 Code Generator**: Generates native Z80 machine code for the RetroShield
- **Self-Hosted Compiler**: A LISP-to-Z80 compiler written in LISP itself
- **Floating Point Library**: Pure LISP decimal floating point arithmetic
- **Comprehensive Test Suite**: Tests for interpreter, compiler, and float library

## Quick Start

### Building

```bash
cargo build --release
```

### Running the REPL

```bash
./target/release/kz80_lisp
```

Example session:
```lisp
kz80_lisp v0.1.0 - A minimal LISP for RetroShield Z80
Type (HELP) for help, (QUIT) to exit

> (+ 1 2 3)
6
> (DEFINE (FACTORIAL N) (IF (<= N 1) 1 (* N (FACTORIAL (- N 1)))))
FACTORIAL
> (FACTORIAL 5)
120
> (QUIT)
```

### Generating Z80 Binary

```bash
./target/release/kz80_lisp --generate-z80 lisp.bin
```

This generates a 32KB ROM image that can be run on the RetroShield Z80 emulator or actual hardware.

### Running on the Emulator

```bash
../emulator/retroshield lisp.bin
```

## Language Reference

### Special Forms

| Form | Description |
|------|-------------|
| `(QUOTE x)` or `'x` | Return x unevaluated |
| `(IF test then else)` | Conditional expression |
| `(COND (test1 result1) ...)` | Multi-way conditional |
| `(LAMBDA (args) body)` | Anonymous function |
| `(DEFINE name value)` | Define a variable |
| `(DEFINE (name args) body)` | Define a function (shorthand) |
| `(SETQ name value)` | Set a variable's value |
| `(LET ((var val) ...) body)` | Local bindings |
| `(PROGN expr ...)` | Sequential evaluation |
| `(AND expr ...)` | Logical and (short-circuit) |
| `(OR expr ...)` | Logical or (short-circuit) |

### Built-in Functions

#### List Operations
| Function | Description |
|----------|-------------|
| `(CAR list)` | First element of a list |
| `(CDR list)` | Rest of a list (all but first) |
| `(CONS a b)` | Construct a pair |
| `(LIST a b ...)` | Create a list |
| `(LENGTH list)` | Number of elements |
| `(APPEND list1 list2)` | Concatenate lists |
| `(REVERSE list)` | Reverse a list |
| `(NTH n list)` | Get nth element (0-indexed) |

#### Predicates
| Function | Description |
|----------|-------------|
| `(NULL x)` | Is x NIL? |
| `(ATOM x)` | Is x an atom (not a cons)? |
| `(CONSP x)` | Is x a cons cell? |
| `(NUMBERP x)` | Is x a number? |
| `(SYMBOLP x)` | Is x a symbol? |
| `(EQ a b)` | Are a and b identical? |
| `(EQUAL a b)` | Are a and b structurally equal? |
| `(ZEROP n)` | Is n zero? |

#### Arithmetic
| Function | Description |
|----------|-------------|
| `(+ a b ...)` | Addition |
| `(- a b)` | Subtraction |
| `(* a b ...)` | Multiplication |
| `(/ a b)` | Integer division |
| `(MOD a b)` | Modulo |
| `(ABS n)` | Absolute value |

#### Comparison
| Function | Description |
|----------|-------------|
| `(= a b)` | Numeric equality |
| `(< a b)` | Less than |
| `(> a b)` | Greater than |
| `(<= a b)` | Less than or equal |
| `(>= a b)` | Greater than or equal |

#### Logic
| Function | Description |
|----------|-------------|
| `(NOT x)` | Logical not |

#### I/O
| Function | Description |
|----------|-------------|
| `(PRINT x)` | Print with newline |
| `(PRINC x)` | Print without newline |
| `(TERPRI)` | Print newline |

## Libraries

### Floating Point Library (float.lisp)

A pure LISP implementation of decimal floating point arithmetic. Floats are represented as cons cells: `(SIGN . (EXP . MANT))`.

```lisp
;; Load the library
> (DEFINE F1 (FINT 42))      ; Convert integer to float
> (DEFINE F2 (FINT 10))
> (FPLUS F1 F2)              ; Add floats
(0 129 . 5200)
> (FTRUNC (FPLUS F1 F2))     ; Convert back to integer
52
```

#### Float Functions

| Function | Description |
|----------|-------------|
| `(FMAKE sign exp mant)` | Construct a float |
| `(FSIGN f)` | Get sign (0=positive, 1=negative) |
| `(FEXP f)` | Get exponent |
| `(FMANT f)` | Get mantissa |
| `(FZEROP f)` | Is float zero? |
| `(FNEGP f)` | Is float negative? |
| `(FPOSP f)` | Is float positive? |
| `(FNEG f)` | Negate float |
| `(FABS f)` | Absolute value |
| `(FINT n)` | Integer to float |
| `(FTRUNC f)` | Float to integer (truncate) |
| `(FPLUS a b)` | Float addition |
| `(FMINUS a b)` | Float subtraction |
| `(FTIMES a b)` | Float multiplication |
| `(FDIVIDE a b)` | Float division |

### Self-Hosted Compiler (compiler.lisp)

A LISP-to-Z80 compiler written in LISP. Compiles arithmetic expressions to native Z80 machine code.

```lisp
> (COMPILE '(+ 1 2))
21
> (DUMP 256 32)  ; View generated code
0100: 21 05 00 E5 21 09 00 D1 19 21 01 00 D1 B7 ED 52
0110: 76 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
```

The compiler supports:
- Integer arithmetic: `+`, `-`, `*`, `/`
- Nested expressions: `(+ 1 (+ 2 3))`
- Float operations via the float library

## Project Structure

```
kz80_lisp/
├── src/
│   ├── main.rs        # REPL and CLI
│   ├── types.rs       # Core LISP types
│   ├── reader.rs      # S-expression parser
│   ├── printer.rs     # Value printer
│   ├── interpret.rs   # Interpreter core
│   ├── builtins.rs    # Built-in functions
│   └── codegen.rs     # Z80 code generator
├── tests/
│   ├── test_interpreter.lisp   # Interpreter tests
│   ├── test_float_library.lisp # Float library tests
│   ├── test_compiler.lisp      # Compiler tests
│   └── run_all_tests.sh        # Test runner
├── float.lisp         # Floating point library
├── compiler.lisp      # Self-hosted compiler
├── Cargo.toml
├── LICENSE            # BSD 3-Clause
└── README.md
```

## Testing

Run the Rust unit tests:
```bash
cargo test
```

Run the LISP test suites:
```bash
# Interpreter tests
cat tests/test_interpreter.lisp | ./target/release/kz80_lisp

# Float library tests
cat float.lisp tests/test_float_library.lisp | ./target/release/kz80_lisp

# Compiler tests
cat compiler.lisp tests/test_compiler.lisp | ./target/release/kz80_lisp

# Or run all tests
./tests/run_all_tests.sh
```

## Technical Details

### Value Representation

The interpreter uses a tagged enum representation:
- `NIL` - The empty list / false
- `T` - True
- `Fixnum(i16)` - 16-bit signed integers
- `Symbol(String)` - Interned symbols
- `Cons(Rc<RefCell<(Value, Value)>>)` - Cons cells
- `Lambda { params, body, env }` - User functions
- `Builtin(fn)` - Built-in functions

### Z80 Target

The Z80 code generator produces:
- 32KB ROM image (0x0000-0x7FFF)
- Uses MC6850 ACIA for serial I/O (ports 0x80/0x81)
- Tagged pointer representation with 2-bit tags
- Garbage collection via mark-and-sweep

### Memory Map (Z80)

| Address | Size | Description |
|---------|------|-------------|
| 0x0000-0x1FFF | 8KB | ROM (interpreter) |
| 0x2000-0x7FFF | 24KB | RAM (heap, stack) |

## Examples

### Recursive Functions

```lisp
;; Fibonacci
(DEFINE (FIB N)
  (COND ((<= N 1) N)
        (T (+ (FIB (- N 1)) (FIB (- N 2))))))

(FIB 10)  ; => 55
```

### Higher-Order Functions

```lisp
;; Map function
(DEFINE (MAP F LST)
  (IF (NULL LST)
      NIL
      (CONS (F (CAR LST)) (MAP F (CDR LST)))))

(MAP (LAMBDA (X) (* X X)) '(1 2 3 4 5))
; => (1 4 9 16 25)
```

### Floating Point Arithmetic

```lisp
;; Load float library first
;; Calculate area of circle with radius 5
(DEFINE PI (FMAKE 0 128 3142))  ; 3.142
(DEFINE R (FINT 5))
(DEFINE AREA (FTIMES PI (FTIMES R R)))
(FTRUNC AREA)  ; => 78
```

## License

BSD 3-Clause License. See [LICENSE](LICENSE) for details.

## Related Projects

- [retroshield-arduino](https://gitlab.com/ajokela/retroshield-arduino) - RetroShield Z80 firmware collection
- [RetroShield Z80 Emulator](../emulator/) - Z80 emulator for testing
