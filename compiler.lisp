;;; LISP-to-Z80 Compiler (Functional Style)
;;; Uses single-letter variable names for Z80 compatibility

;; Emit a byte and return new PC
;; P = PC, B = byte value
(DEFINE EMIT (LAMBDA (P B) (PROGN (POKE P B) (+ P 1))))

;; Emit a 16-bit word (little-endian) and return new PC
;; P = PC, W = word value
(DEFINE EMIT16 (LAMBDA (P W)
  (EMIT (EMIT P (% W 256)) (/ W 256))))

;;; Z80 Instructions (each returns new PC)

;; LD HL, nn (opcode 0x21 = 33)
(DEFINE LDHLNN (LAMBDA (P N) (EMIT16 (EMIT P 33) N)))

;; PUSH HL (opcode 0xE5 = 229)
(DEFINE PUSHHL (LAMBDA (P) (EMIT P 229)))

;; POP HL (opcode 0xE1 = 225)
(DEFINE POPHL (LAMBDA (P) (EMIT P 225)))

;; POP DE (opcode 0xD1 = 209)
(DEFINE POPDE (LAMBDA (P) (EMIT P 209)))

;; ADD HL, DE (opcode 0x19 = 25)
(DEFINE ADDHLDE (LAMBDA (P) (EMIT P 25)))

;; SBC HL, DE (ED 52 = 237, 82)
(DEFINE SBCHLDE (LAMBDA (P) (EMIT (EMIT P 237) 82)))

;; OR A (opcode 0xB7 = 183)
(DEFINE ORA (LAMBDA (P) (EMIT P 183)))

;; HALT (opcode 0x76 = 118)
(DEFINE HALTZ (LAMBDA (P) (EMIT P 118)))

;;; Value representation
(DEFINE TAGFIX 1)

;; Make a fixnum from an integer
(DEFINE MKFIX (LAMBDA (N) (+ (* N 4) TAGFIX)))

;;; Compiler

;; Compile a number - load into HL as tagged fixnum
;; P = PC, N = number
(DEFINE CNUM (LAMBDA (P N) (LDHLNN P (MKFIX N))))

;; Forward declaration handled by explicit lookup
;; Compile (+ A B)
;; P = PC, X = first arg, Y = second arg
(DEFINE CADD (LAMBDA (P X Y)
  (LET ((Q (CEXP P X)))
    (LET ((R (PUSHHL Q)))
      (LET ((S (CEXP R Y)))
        (LET ((U (POPDE S)))
          (LET ((V (ADDHLDE U)))
            (LET ((W (LDHLNN V 1)))
              (SBCHLDE (ORA (POPDE W)))))))))))

;; Compile (- A B)
;; P = PC, X = first arg, Y = second arg
(DEFINE CSUB (LAMBDA (P X Y)
  (LET ((Q (CEXP P Y)))
    (LET ((R (PUSHHL Q)))
      (LET ((S (CEXP R X)))
        (LET ((U (POPDE S)))
          (LET ((V (ORA U)))
            (LET ((W (SBCHLDE V)))
              (ADDHLDE (LDHLNN W 1))))))))))

;; Compile a function application
;; P = PC, F = function symbol, A = args list
;; NOTE: Using CAPPLY instead of CAPP to avoid hash collision with CEXP
(DEFINE CAPPLY (LAMBDA (P F A)
  (COND
    ((EQ F '+) (CADD P (CAR A) (CAR (CDR A))))
    ((EQ F '-) (CSUB P (CAR A) (CAR (CDR A))))
    (T P))))

;; Main expression compiler
;; P = PC, E = expression
(DEFINE CEXP (LAMBDA (P E)
  (COND
    ((NUMBERP E) (CNUM P E))
    ((CONSP E) (CAPPLY P (CAR E) (CDR E)))
    (T P))))

;;; Main compiler driver

;; NOTE: Simplified to avoid LET variable lookup issues in nested lambdas
(DEFINE COMPILE (LAMBDA (E)
  (- (HALTZ (CEXP 256 E)) 256)))

;; Test: compile (+ 1 2)
(PRINT '(Compiling...))
(PRINT (LIST 'Generated (COMPILE '(+ 1 2)) 'bytes))
(PRINT '(Dumping...))
(DUMP 256 32)
(PRINT 'Done)
