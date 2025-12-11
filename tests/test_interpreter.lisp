;;; Test Suite for kz80_lisp Rust Interpreter
;;; Run with: cat tests/test_interpreter.lisp | ./target/release/kz80_lisp

;;; Simple test framework - prints PASS or FAIL for each test
(DEFINE (TEST-EQ NAME EXPECTED ACTUAL)
  (IF (EQUAL EXPECTED ACTUAL)
      (PRINT (LIST 'PASS NAME))
      (PRINT (LIST 'FAIL NAME 'expected EXPECTED 'got ACTUAL))))

(DEFINE (TEST-TRUE NAME EXPR)
  (TEST-EQ NAME T (IF EXPR T NIL)))

(DEFINE (TEST-NIL NAME EXPR)
  (TEST-EQ NAME NIL EXPR))

;;; ============================================
;;; Basic Atoms
;;; ============================================
(PRINT 'BASIC-ATOMS)
(TEST-EQ 'fixnum-positive 42 42)
(TEST-EQ 'fixnum-negative -5 -5)
(TEST-EQ 'fixnum-zero 0 0)
(TEST-TRUE 'T-is-true T)
(TEST-NIL 'NIL-is-nil NIL)

;;; ============================================
;;; Arithmetic
;;; ============================================
(PRINT 'ARITHMETIC)
(TEST-EQ 'add-two (+ 1 2) 3)
(TEST-EQ 'add-three (+ 1 2 3) 6)
(TEST-EQ 'add-negative (+ 5 -3) 2)
(TEST-EQ 'sub-two (- 10 3) 7)
(TEST-EQ 'sub-negative (- 5 10) -5)
(TEST-EQ 'mul-two (* 4 5) 20)
(TEST-EQ 'mul-negative (* -3 4) -12)
(TEST-EQ 'div-exact (/ 20 4) 5)
(TEST-EQ 'div-truncate (/ 7 2) 3)
(TEST-EQ 'mod-positive (MOD 7 3) 1)
(TEST-EQ 'abs-positive (ABS 5) 5)
(TEST-EQ 'abs-negative (ABS -5) 5)

;;; ============================================
;;; Comparisons
;;; ============================================
(PRINT 'COMPARISONS)
(TEST-TRUE 'eq-numbers (= 5 5))
(TEST-NIL 'eq-different (= 5 6))
(TEST-TRUE 'lt-true (< 3 5))
(TEST-NIL 'lt-false (< 5 3))
(TEST-TRUE 'gt-true (> 5 3))
(TEST-NIL 'gt-false (> 3 5))
(TEST-TRUE 'le-less (<= 3 5))
(TEST-TRUE 'le-equal (<= 5 5))
(TEST-NIL 'le-greater (<= 6 5))
(TEST-TRUE 'ge-greater (>= 5 3))
(TEST-TRUE 'ge-equal (>= 5 5))
(TEST-NIL 'ge-less (>= 3 5))
(TEST-TRUE 'zerop-zero (ZEROP 0))
(TEST-NIL 'zerop-nonzero (ZEROP 5))

;;; ============================================
;;; List Operations
;;; ============================================
(PRINT 'LIST-OPERATIONS)
(TEST-EQ 'car-list (CAR '(1 2 3)) 1)
(TEST-EQ 'cdr-list (CDR '(1 2 3)) '(2 3))
(TEST-TRUE 'cons-atoms (CONSP (CONS 1 2)))
(TEST-EQ 'cons-list (CONS 1 '(2 3)) '(1 2 3))
(TEST-EQ 'list-three (LIST 1 2 3) '(1 2 3))
(TEST-EQ 'length-three (LENGTH '(1 2 3)) 3)
(TEST-EQ 'length-empty (LENGTH NIL) 0)
(TEST-EQ 'append-lists (APPEND '(1 2) '(3 4)) '(1 2 3 4))
(TEST-EQ 'reverse-list (REVERSE '(1 2 3)) '(3 2 1))
(TEST-EQ 'nth-second (NTH 1 '(a b c)) 'b)

;;; ============================================
;;; Predicates
;;; ============================================
(PRINT 'PREDICATES)
(TEST-TRUE 'null-nil (NULL NIL))
(TEST-NIL 'null-list (NULL '(1)))
(TEST-TRUE 'atom-number (ATOM 42))
(TEST-TRUE 'atom-symbol (ATOM 'foo))
(TEST-NIL 'atom-list (ATOM '(1 2)))
(TEST-TRUE 'consp-list (CONSP '(1 2)))
(TEST-NIL 'consp-atom (CONSP 42))
(TEST-TRUE 'numberp-fixnum (NUMBERP 42))
(TEST-NIL 'numberp-symbol (NUMBERP 'foo))
(TEST-TRUE 'symbolp-symbol (SYMBOLP 'foo))
(TEST-NIL 'symbolp-number (SYMBOLP 42))
(TEST-TRUE 'eq-same (EQ 'foo 'foo))
(TEST-NIL 'eq-different (EQ 'foo 'bar))
(TEST-TRUE 'equal-lists (EQUAL '(1 2) '(1 2)))

;;; ============================================
;;; Special Forms
;;; ============================================
(PRINT 'SPECIAL-FORMS)
(TEST-EQ 'quote-list (QUOTE (1 2 3)) '(1 2 3))
(TEST-EQ 'if-true (IF T 1 2) 1)
(TEST-EQ 'if-false (IF NIL 1 2) 2)
(TEST-EQ 'if-no-else (IF T 42) 42)
(TEST-EQ 'cond-first (COND (T 1) (T 2)) 1)
(TEST-EQ 'cond-second (COND (NIL 1) (T 2)) 2)
(TEST-EQ 'progn-last (PROGN 1 2 3) 3)
(TEST-EQ 'let-simple (LET ((X 10)) X) 10)
(TEST-EQ 'let-multiple (LET ((X 10) (Y 20)) (+ X Y)) 30)

;;; ============================================
;;; Logic
;;; ============================================
(PRINT 'LOGIC)
(TEST-TRUE 'not-nil (NOT NIL))
(TEST-NIL 'not-t (NOT T))
(TEST-TRUE 'and-all-true (AND T T T))
(TEST-NIL 'and-one-false (AND T NIL T))
(TEST-TRUE 'or-one-true (OR NIL T NIL))
(TEST-NIL 'or-all-false (OR NIL NIL NIL))

;;; ============================================
;;; Functions
;;; ============================================
(PRINT 'FUNCTIONS)
(DEFINE (SQUARE X) (* X X))
(TEST-EQ 'define-function (SQUARE 5) 25)

(DEFINE (FACTORIAL N)
  (IF (<= N 1)
      1
      (* N (FACTORIAL (- N 1)))))
(TEST-EQ 'recursive-factorial (FACTORIAL 5) 120)

(DEFINE (FIB N)
  (COND ((<= N 1) N)
        (T (+ (FIB (- N 1)) (FIB (- N 2))))))
(TEST-EQ 'recursive-fib (FIB 10) 55)

;;; Lambda
(TEST-EQ 'lambda-call ((LAMBDA (X) (* X X)) 4) 16)
(TEST-EQ 'lambda-closure
  (LET ((Y 10))
    ((LAMBDA (X) (+ X Y)) 5))
  15)

;;; ============================================
;;; Summary
;;; ============================================
(PRINT 'ALL-TESTS-COMPLETE)
