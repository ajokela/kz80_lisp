;;; Test Suite for Self-Hosted Compiler
;;; Run with: cat compiler.lisp tests/test_compiler.lisp | ./target/release/kz80_lisp

;;; Test framework
(DEFINE *TESTS-RUN* 0)
(DEFINE *TESTS-PASSED* 0)

(DEFINE (TEST-EQ NAME EXPECTED ACTUAL)
  (SETQ *TESTS-RUN* (+ *TESTS-RUN* 1))
  (IF (EQUAL EXPECTED ACTUAL)
      (PROGN
        (SETQ *TESTS-PASSED* (+ *TESTS-PASSED* 1))
        (PRINT (LIST 'PASS NAME)))
      (PROGN
        (PRINT (LIST 'FAIL NAME 'expected EXPECTED 'got ACTUAL)))))

(DEFINE (TEST-TRUE NAME EXPR)
  (TEST-EQ NAME T (IF EXPR T NIL)))

(DEFINE (TEST-GT NAME VAL MIN)
  (SETQ *TESTS-RUN* (+ *TESTS-RUN* 1))
  (IF (> VAL MIN)
      (PROGN
        (SETQ *TESTS-PASSED* (+ *TESTS-PASSED* 1))
        (PRINT (LIST 'PASS NAME VAL)))
      (PROGN
        (PRINT (LIST 'FAIL NAME 'got VAL 'expected> MIN)))))

;;; ============================================
;;; Emit Functions
;;; ============================================
(PRINT '--- EMIT FUNCTIONS ---)

;; Test EMIT - should write byte and return PC+1
(DEFINE START-PC 512)
(DEFINE AFTER-EMIT (EMIT START-PC 42))
(TEST-EQ 'emit-returns-pc+1 AFTER-EMIT 513)
(TEST-EQ 'emit-writes-byte (PEEK START-PC) 42)

;; Test EMIT16 - should write word little-endian
(DEFINE AFTER-EMIT16 (EMIT16 514 4660))  ; 0x1234
(TEST-EQ 'emit16-returns-pc+2 AFTER-EMIT16 516)
(TEST-EQ 'emit16-low-byte (PEEK 514) 52)   ; 0x34 = 52
(TEST-EQ 'emit16-high-byte (PEEK 515) 18)  ; 0x12 = 18

;;; ============================================
;;; Z80 Instructions
;;; ============================================
(PRINT '--- Z80 INSTRUCTIONS ---)

;; Test LDHLNN (LD HL, nn)
(DEFINE AFTER-LDHL (LDHLNN 600 1000))
(TEST-EQ 'ldhlnn-size (- AFTER-LDHL 600) 3)
(TEST-EQ 'ldhlnn-opcode (PEEK 600) 33)  ; 0x21

;; Test PUSHHL
(DEFINE AFTER-PUSH (PUSHHL 700))
(TEST-EQ 'pushhl-size (- AFTER-PUSH 700) 1)
(TEST-EQ 'pushhl-opcode (PEEK 700) 229)  ; 0xE5

;; Test POPHL
(DEFINE AFTER-POP (POPHL 701))
(TEST-EQ 'pophl-opcode (PEEK 701) 225)  ; 0xE1

;; Test POPDE
(DEFINE AFTER-POPDE (POPDE 702))
(TEST-EQ 'popde-opcode (PEEK 702) 209)  ; 0xD1

;; Test ADDHLDE
(DEFINE AFTER-ADD (ADDHLDE 703))
(TEST-EQ 'addhlde-opcode (PEEK 703) 25)  ; 0x19

;; Test SBCHLDE (ED 52)
(DEFINE AFTER-SBC (SBCHLDE 704))
(TEST-EQ 'sbchlde-size (- AFTER-SBC 704) 2)
(TEST-EQ 'sbchlde-prefix (PEEK 704) 237)  ; 0xED
(TEST-EQ 'sbchlde-opcode (PEEK 705) 82)   ; 0x52

;; Test ORA
(DEFINE AFTER-ORA (ORA 706))
(TEST-EQ 'ora-opcode (PEEK 706) 183)  ; 0xB7

;; Test HALTZ
(DEFINE AFTER-HALT (HALTZ 707))
(TEST-EQ 'halt-opcode (PEEK 707) 118)  ; 0x76

;;; ============================================
;;; Value Representation
;;; ============================================
(PRINT '--- VALUE REPRESENTATION ---)

(TEST-EQ 'mkfix-zero (MKFIX 0) 1)
(TEST-EQ 'mkfix-one (MKFIX 1) 5)
(TEST-EQ 'mkfix-42 (MKFIX 42) 169)
(TEST-EQ 'mkfix-neg (MKFIX -5) -19)

;;; ============================================
;;; Compiling Numbers
;;; ============================================
(PRINT '--- COMPILING NUMBERS ---)

;; Compile a simple number
(DEFINE CNUM-END (CNUM 800 42))
(TEST-EQ 'cnum-size (- CNUM-END 800) 3)
(TEST-EQ 'cnum-opcode (PEEK 800) 33)  ; LD HL,
(TEST-EQ 'cnum-value-low (PEEK 801) 169)  ; MKFIX 42 = 169

;;; ============================================
;;; Compiling Addition
;;; ============================================
(PRINT '--- COMPILING ADDITION ---)

;; Compile (+ 1 2)
(DEFINE ADD-SIZE (COMPILE '(+ 1 2)))
(TEST-GT 'add-compiles ADD-SIZE 10)

;; The generated code should load 1, push, load 2, pop, add
(DEFINE ADD-START 256)
(TEST-EQ 'add-first-load (PEEK ADD-START) 33)  ; LD HL, nn

;;; ============================================
;;; Compiling Subtraction
;;; ============================================
(PRINT '--- COMPILING SUBTRACTION ---)

(DEFINE SUB-SIZE (COMPILE '(- 10 3)))
(TEST-GT 'sub-compiles SUB-SIZE 10)

;;; ============================================
;;; Compiling Multiplication
;;; ============================================
(PRINT '--- COMPILING MULTIPLICATION ---)

(DEFINE MUL-SIZE (COMPILE '(* 4 5)))
(TEST-GT 'mul-compiles MUL-SIZE 5)

;;; ============================================
;;; Compiling Division
;;; ============================================
(PRINT '--- COMPILING DIVISION ---)

(DEFINE DIV-SIZE (COMPILE '(/ 20 4)))
(TEST-GT 'div-compiles DIV-SIZE 5)

;;; ============================================
;;; Compiling Nested Expressions
;;; ============================================
(PRINT '--- COMPILING NESTED EXPRESSIONS ---)

;; Compile (+ 1 (+ 2 3))
(DEFINE NESTED-SIZE (COMPILE '(+ 1 (+ 2 3))))
(TEST-GT 'nested-add NESTED-SIZE 15)

;; Compile (- (+ 10 5) 3)
(DEFINE NESTED2-SIZE (COMPILE '(- (+ 10 5) 3)))
(TEST-GT 'nested-sub-add NESTED2-SIZE 15)

;;; ============================================
;;; CEXP Dispatcher
;;; ============================================
(PRINT '--- CEXP DISPATCHER ---)

;; Numbers should compile
(DEFINE CEXP-NUM (CEXP 900 42))
(TEST-GT 'cexp-number CEXP-NUM 900)

;; Lists should compile
(DEFINE CEXP-LIST (CEXP 950 '(+ 1 2)))
(TEST-GT 'cexp-list CEXP-LIST 960)

;;; ============================================
;;; Summary
;;; ============================================
(PRINT '--- SUMMARY ---)
(PRINT (LIST 'Tests-run *TESTS-RUN*))
(PRINT (LIST 'Tests-passed *TESTS-PASSED*))
(IF (= *TESTS-RUN* *TESTS-PASSED*)
    (PRINT 'ALL-TESTS-PASSED)
    (PRINT (LIST 'FAILED (- *TESTS-RUN* *TESTS-PASSED*))))
