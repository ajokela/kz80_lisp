;;; Test Suite for Pure LISP Float Library
;;; Run with: cat float.lisp tests/test_float_library.lisp | ./target/release/kz80_lisp

;;; Simple test framework
(DEFINE (TEST-EQ NAME EXPECTED ACTUAL)
  (IF (EQUAL EXPECTED ACTUAL)
      (PRINT (LIST 'PASS NAME))
      (PRINT (LIST 'FAIL NAME 'expected EXPECTED 'got ACTUAL))))

(DEFINE (TEST-TRUE NAME EXPR)
  (TEST-EQ NAME T (IF EXPR T NIL)))

(DEFINE (TEST-NIL NAME EXPR)
  (TEST-EQ NAME NIL EXPR))

;;; Helper to compare floats by components
(DEFINE (FLOAT-EQ F1 F2)
  (AND (EQ (FSIGN F1) (FSIGN F2))
       (EQ (FEXP F1) (FEXP F2))
       (EQ (FMANT F1) (FMANT F2))))

(DEFINE (TEST-FLOAT NAME EXPECTED ACTUAL)
  (IF (FLOAT-EQ EXPECTED ACTUAL)
      (PRINT (LIST 'PASS NAME))
      (PROGN
        (PRINT (LIST 'FAIL NAME))
        (PRINT (LIST 'expected EXPECTED))
        (PRINT (LIST 'got ACTUAL)))))

;;; ============================================
;;; Float Construction and Accessors
;;; ============================================
(PRINT 'FLOAT-CONSTRUCTION)

;; Test FMAKE
(DEFINE F1 (FMAKE 0 128 1000))
(TEST-EQ 'fmake-sign (FSIGN F1) 0)
(TEST-EQ 'fmake-exp (FEXP F1) 128)
(TEST-EQ 'fmake-mant (FMANT F1) 1000)

;; Test constants
(TEST-TRUE 'fzero-is-zero (FZEROP FZERO))
(TEST-EQ 'fone-sign (FSIGN FONE) 0)
(TEST-EQ 'fone-mant (FMANT FONE) 1000)

;;; ============================================
;;; Float Predicates
;;; ============================================
(PRINT 'FLOAT-PREDICATES)

(TEST-TRUE 'fzerop-zero (FZEROP FZERO))
(TEST-NIL 'fzerop-nonzero (FZEROP FONE))

(DEFINE FNEG1 (FMAKE 1 128 1000))
(TEST-TRUE 'fnegp-negative (FNEGP FNEG1))
(TEST-NIL 'fnegp-positive (FNEGP FONE))

(TEST-TRUE 'fposp-positive (FPOSP FONE))
(TEST-NIL 'fposp-negative (FPOSP FNEG1))
(TEST-NIL 'fposp-zero (FPOSP FZERO))

;;; ============================================
;;; Float Negation and Absolute Value
;;; ============================================
(PRINT 'NEGATION-AND-ABS)

(DEFINE NEG-ONE (FNEG FONE))
(TEST-EQ 'fneg-positive-sign (FSIGN NEG-ONE) 1)
(TEST-EQ 'fneg-positive-mant (FMANT NEG-ONE) 1000)

(DEFINE DOUBLE-NEG (FNEG NEG-ONE))
(TEST-EQ 'fneg-negative-sign (FSIGN DOUBLE-NEG) 0)

(TEST-TRUE 'fneg-zero (FZEROP (FNEG FZERO)))

(DEFINE ABS-NEG (FABS NEG-ONE))
(TEST-EQ 'fabs-negative (FSIGN ABS-NEG) 0)
(TEST-EQ 'fabs-positive (FSIGN (FABS FONE)) 0)

;;; ============================================
;;; Integer to Float Conversion
;;; ============================================
(PRINT 'INT-TO-FLOAT)

(TEST-TRUE 'fint-zero (FZEROP (FINT 0)))

(DEFINE F42 (FINT 42))
(TEST-EQ 'fint-positive-sign (FSIGN F42) 0)
(TEST-EQ 'fint-positive-exp (FEXP F42) 131)
(TEST-EQ 'fint-positive-mant (FMANT F42) 42)

(DEFINE FNEG5 (FINT -5))
(TEST-EQ 'fint-negative-sign (FSIGN FNEG5) 1)
(TEST-EQ 'fint-negative-mant (FMANT FNEG5) 5)

;;; ============================================
;;; Float to Integer Truncation
;;; ============================================
(PRINT 'FLOAT-TO-INT)

(TEST-EQ 'ftrunc-zero (FTRUNC FZERO) 0)
(TEST-EQ 'ftrunc-one (FTRUNC FONE) 1)
(TEST-EQ 'ftrunc-42 (FTRUNC (FINT 42)) 42)
(TEST-EQ 'ftrunc-neg5 (FTRUNC (FINT -5)) -5)

;; Test 3.14 -> 3
(DEFINE F314 (FMAKE 0 128 3140))
(TEST-EQ 'ftrunc-314 (FTRUNC F314) 3)

;;; ============================================
;;; Float Addition
;;; ============================================
(PRINT 'FLOAT-ADDITION)

;; 0 + 1 = 1
(TEST-FLOAT 'fplus-zero-left FONE (FPLUS FZERO FONE))
(TEST-FLOAT 'fplus-zero-right FONE (FPLUS FONE FZERO))

;; 1 + 1 = 2
(DEFINE FTWO (FPLUS FONE FONE))
(TEST-EQ 'fplus-one-one-sign (FSIGN FTWO) 0)
(TEST-EQ 'fplus-one-one-mant (FMANT FTWO) 2000)

;; 42 + 8 = 50
(DEFINE F50 (FPLUS (FINT 42) (FINT 8)))
(TEST-EQ 'fplus-42-8 (FTRUNC F50) 50)

;; -5 + 5 = 0
(DEFINE FSUM (FPLUS (FINT -5) (FINT 5)))
(TEST-TRUE 'fplus-cancel (FZEROP FSUM))

;;; ============================================
;;; Float Subtraction
;;; ============================================
(PRINT 'FLOAT-SUBTRACTION)

;; 5 - 3 = 2
(DEFINE FDIFF (FMINUS (FINT 5) (FINT 3)))
(TEST-EQ 'fminus-5-3 (FTRUNC FDIFF) 2)

;; 3 - 5 = -2
(DEFINE FDIFF2 (FMINUS (FINT 3) (FINT 5)))
(TEST-EQ 'fminus-3-5 (FTRUNC FDIFF2) -2)

;; x - 0 = x
(TEST-EQ 'fminus-zero (FTRUNC (FMINUS (FINT 42) FZERO)) 42)

;;; ============================================
;;; Float Multiplication
;;; ============================================
(PRINT 'FLOAT-MULTIPLICATION)

;; 0 * x = 0
(TEST-TRUE 'ftimes-zero-left (FZEROP (FTIMES FZERO (FINT 42))))
(TEST-TRUE 'ftimes-zero-right (FZEROP (FTIMES (FINT 42) FZERO)))

;; 1 * x = x
(TEST-EQ 'ftimes-one (FTRUNC (FTIMES FONE (FINT 42))) 42)

;; 3 * 4 = 12
(DEFINE FPROD (FTIMES (FINT 3) (FINT 4)))
(TEST-EQ 'ftimes-3-4 (FTRUNC FPROD) 12)

;; -3 * 4 = -12
(DEFINE FPROD2 (FTIMES (FINT -3) (FINT 4)))
(TEST-EQ 'ftimes-neg (FTRUNC FPROD2) -12)

;; -3 * -4 = 12
(DEFINE FPROD3 (FTIMES (FINT -3) (FINT -4)))
(TEST-EQ 'ftimes-neg-neg (FTRUNC FPROD3) 12)

;;; ============================================
;;; Float Division
;;; ============================================
(PRINT 'FLOAT-DIVISION)

;; 0 / x = 0
(TEST-TRUE 'fdiv-zero-num (FZEROP (FDIVIDE FZERO (FINT 5))))

;; x / 0 = NIL (error)
(TEST-NIL 'fdiv-by-zero (FDIVIDE (FINT 5) FZERO))

;; 20 / 4 = 5
(DEFINE FQUOT (FDIVIDE (FINT 20) (FINT 4)))
(TEST-EQ 'fdiv-20-4 (FTRUNC FQUOT) 5)

;; -20 / 4 = -5
(DEFINE FQUOT2 (FDIVIDE (FINT -20) (FINT 4)))
(TEST-EQ 'fdiv-neg (FTRUNC FQUOT2) -5)

;; -20 / -4 = 5
(DEFINE FQUOT3 (FDIVIDE (FINT -20) (FINT -4)))
(TEST-EQ 'fdiv-neg-neg (FTRUNC FQUOT3) 5)

;;; ============================================
;;; Magnitude Comparison
;;; ============================================
(PRINT 'MAGNITUDE-COMPARISON)

(TEST-EQ 'fcmpmag-equal (FCMPMAG FONE FONE) 0)
(TEST-EQ 'fcmpmag-greater (FCMPMAG (FINT 5) (FINT 3)) 1)
(TEST-EQ 'fcmpmag-less (FCMPMAG (FINT 3) (FINT 5)) -1)

;;; ============================================
;;; Summary
;;; ============================================
(PRINT 'ALL-FLOAT-TESTS-COMPLETE)
