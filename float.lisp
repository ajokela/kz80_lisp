;;; Pure LISP Floating Point Library
;;; Implements decimal floating point using only integer arithmetic
;;;
;;; Float representation: (SIGN . (EXP . MANT))
;;;   SIGN: 0 = positive, 1 = negative
;;;   EXP:  Exponent (power of 10), biased by 128 (so 128 = 10^0)
;;;   MANT: Mantissa as integer (1000-9999 for normalized, or 0)
;;;
;;; Examples:
;;;   3.14  = (0 . (128 . 314))   ; 314 * 10^(128-128-2) = 3.14
;;;   -42.0 = (1 . (129 . 4200))  ; -4200 * 10^(129-128-2) = -42.0
;;;   0.0   = (0 . (128 . 0))

;;; Constructor
(DEFINE FMAKE (LAMBDA (S E M)
  (CONS S (CONS E M))))

;;; Accessors
(DEFINE FSIGN (LAMBDA (F) (CAR F)))
(DEFINE FEXP  (LAMBDA (F) (CAR (CDR F))))
(DEFINE FMANT (LAMBDA (F) (CDR (CDR F))))

;;; Constants
(DEFINE FZERO (FMAKE 0 128 0))
(DEFINE FONE  (FMAKE 0 128 1000))
(DEFINE FBIAS 128)

;;; Predicates
(DEFINE FZEROP (LAMBDA (F)
  (EQ (FMANT F) 0)))

(DEFINE FNEGP (LAMBDA (F)
  (EQ (FSIGN F) 1)))

(DEFINE FPOSP (LAMBDA (F)
  (COND ((FZEROP F) NIL)
        (T (EQ (FSIGN F) 0)))))

;;; Negate a float
(DEFINE FNEG (LAMBDA (F)
  (COND ((FZEROP F) F)
        ((FNEGP F) (FMAKE 0 (FEXP F) (FMANT F)))
        (T (FMAKE 1 (FEXP F) (FMANT F))))))

;;; Absolute value
(DEFINE FABS (LAMBDA (F)
  (FMAKE 0 (FEXP F) (FMANT F))))

;;; Integer to float
;;; N is a fixnum, returns float representation
(DEFINE FINT (LAMBDA (N)
  (COND ((EQ N 0) FZERO)
        ((< N 0) (FMAKE 1 131 (- 0 N)))
        (T (FMAKE 0 131 N)))))

;;; Float to integer (truncate)
(DEFINE FTRUNC (LAMBDA (F)
  (LET ((E (- (FEXP F) FBIAS)))
    (LET ((M (FMANT F)))
      (LET ((V (COND
                 ((< E 0) 0)
                 ((EQ E 0) (/ M 1000))
                 ((EQ E 1) (/ M 100))
                 ((EQ E 2) (/ M 10))
                 ((EQ E 3) M)
                 (T (* M (COND ((EQ E 4) 10)
                               ((EQ E 5) 100)
                               (T 1000)))))))
        (COND ((FNEGP F) (- 0 V))
              (T V)))))))

;;; Normalize a mantissa (ensure it's in range 1000-9999 or 0)
;;; Returns (EXP-ADJUST . NEW-MANT)
(DEFINE FNORM (LAMBDA (M E)
  (COND ((EQ M 0) (CONS 0 0))
        ((< M 1000)
         (COND ((< M 100)
                (COND ((< M 10)
                       (FNORM (* M 10) (- E 1)))
                      (T (FNORM (* M 10) (- E 1)))))
               (T (FNORM (* M 10) (- E 1)))))
        ((> M 9999)
         (FNORM (/ M 10) (+ E 1)))
        (T (CONS E M)))))

;;; Add two floats with same sign
;;; Returns unnormalized (EXP . MANT)
(DEFINE FADDSAME (LAMBDA (E1 M1 E2 M2)
  (COND ((> E1 E2)
         (LET ((D (- E1 E2)))
           (LET ((M2S (COND ((EQ D 1) (/ M2 10))
                            ((EQ D 2) (/ M2 100))
                            ((EQ D 3) (/ M2 1000))
                            (T 0))))
             (CONS E1 (+ M1 M2S)))))
        ((< E1 E2)
         (LET ((D (- E2 E1)))
           (LET ((M1S (COND ((EQ D 1) (/ M1 10))
                            ((EQ D 2) (/ M1 100))
                            ((EQ D 3) (/ M1 1000))
                            (T 0))))
             (CONS E2 (+ M1S M2)))))
        (T (CONS E1 (+ M1 M2))))))

;;; Subtract mantissas (M1 - M2) with alignment
;;; Assumes M1 >= M2 after alignment
(DEFINE FSUBMANT (LAMBDA (E1 M1 E2 M2)
  (COND ((> E1 E2)
         (LET ((D (- E1 E2)))
           (LET ((M2S (COND ((EQ D 1) (/ M2 10))
                            ((EQ D 2) (/ M2 100))
                            ((EQ D 3) (/ M2 1000))
                            (T 0))))
             (CONS E1 (- M1 M2S)))))
        ((< E1 E2)
         (LET ((D (- E2 E1)))
           (LET ((M1S (COND ((EQ D 1) (/ M1 10))
                            ((EQ D 2) (/ M1 100))
                            ((EQ D 3) (/ M1 1000))
                            (T 0))))
             (CONS E2 (- M1S M2)))))
        (T (CONS E1 (- M1 M2))))))

;;; Compare magnitudes: returns -1, 0, or 1
(DEFINE FCMPMAG (LAMBDA (F1 F2)
  (LET ((E1 (FEXP F1)) (M1 (FMANT F1))
        (E2 (FEXP F2)) (M2 (FMANT F2)))
    (COND ((> E1 E2) 1)
          ((< E1 E2) -1)
          ((> M1 M2) 1)
          ((< M1 M2) -1)
          (T 0)))))

;;; Add two floats
(DEFINE FPLUS (LAMBDA (F1 F2)
  (COND
    ((FZEROP F1) F2)
    ((FZEROP F2) F1)
    ((EQ (FSIGN F1) (FSIGN F2))
     (LET ((R (FADDSAME (FEXP F1) (FMANT F1) (FEXP F2) (FMANT F2))))
       (LET ((N (FNORM (CDR R) (CAR R))))
         (FMAKE (FSIGN F1) (CAR N) (CDR N)))))
    (T (LET ((C (FCMPMAG F1 F2)))
         (COND
           ((EQ C 0) FZERO)
           ((EQ C 1)
            (LET ((R (FSUBMANT (FEXP F1) (FMANT F1) (FEXP F2) (FMANT F2))))
              (LET ((N (FNORM (CDR R) (CAR R))))
                (FMAKE (FSIGN F1) (CAR N) (CDR N)))))
           (T
            (LET ((R (FSUBMANT (FEXP F2) (FMANT F2) (FEXP F1) (FMANT F1))))
              (LET ((N (FNORM (CDR R) (CAR R))))
                (FMAKE (FSIGN F2) (CAR N) (CDR N)))))))))))

;;; Subtract: F1 - F2 = F1 + (-F2)
(DEFINE FMINUS (LAMBDA (F1 F2)
  (FPLUS F1 (FNEG F2))))

;;; Multiply two floats
(DEFINE FTIMES (LAMBDA (F1 F2)
  (COND
    ((FZEROP F1) FZERO)
    ((FZEROP F2) FZERO)
    (T (LET ((S (COND ((EQ (FSIGN F1) (FSIGN F2)) 0) (T 1))))
         (LET ((E (+ (- (FEXP F1) FBIAS) (FEXP F2))))
           (LET ((M (/ (* (FMANT F1) (FMANT F2)) 1000)))
             (LET ((N (FNORM M E)))
               (FMAKE S (CAR N) (CDR N))))))))))

;;; Divide two floats
(DEFINE FDIVIDE (LAMBDA (F1 F2)
  (COND
    ((FZEROP F1) FZERO)
    ((FZEROP F2) NIL)
    (T (LET ((S (COND ((EQ (FSIGN F1) (FSIGN F2)) 0) (T 1))))
         (LET ((E (+ (- (FEXP F1) (FEXP F2)) FBIAS)))
           (LET ((M (/ (* (FMANT F1) 1000) (FMANT F2))))
             (LET ((N (FNORM M E)))
               (FMAKE S (CAR N) (CDR N))))))))))

;;; Print a float (returns the float for chaining)
(DEFINE FPRINT (LAMBDA (F)
  (PROGN
    (COND ((FNEGP F) (PRINT '-)))
    (LET ((E (- (FEXP F) FBIAS))
          (M (FMANT F)))
      (COND
        ((EQ M 0) (PRINT 0))
        ((EQ E 3) (PRINT M))
        ((EQ E 2) (PRINT (/ M 10)))
        ((EQ E 1) (PRINT (/ M 100)))
        ((EQ E 0) (PRINT (/ M 1000)))
        (T (PRINT M))))
    F)))

;;; Done - library loaded
T
