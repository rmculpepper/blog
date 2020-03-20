#lang racket
(provide (all-defined-out))

;; gensym replacement with resettable counter for readability
(define counter (make-parameter 0))
(define (gensym*)
  (counter (add1 (counter)))
  (string->symbol (format "x~a" (counter))))

;; A Probe is (probe (promise-of Neutral))
(struct probe (term-p)
  #:property prop:procedure
  (lambda (self arg)
    (probe (delay `(,(probe-term self) ,(un-eval+* arg))))))

;; probe-term : Probe -> Neutral
(define (probe-term p) (force (probe-term-p p)))

;; un-eval+ : eval(NF) -> NF
(define (un-eval+ p)
  (parameterize ((counter 0))
    (un-eval+* p)))

;; un-eval+* : eval(NF) -> NF
(define (un-eval+* p)
  (if (probe? p)
      (probe-term p)
      (let ([var (gensym*)])
        `(lambda (,var) ,(un-eval+* (p (probe (delay var))))))))

;; ------------------------------------------------------------

(define (((PAIR a) b) f) ((f a) b))
(define (CAR p) (p (lambda (a) (lambda (b) a))))
(define (CDR p) (p (lambda (a) (lambda (b) b))))

(define ((TRUE t) f) t)
(define ((FALSE t) f) f)

(define-syntax-rule (IF b t f)
  (((b (lambda (_) t)) (lambda (_) f)) '***))

(define (ZERO? n) ((n (lambda _ FALSE)) TRUE))

(define ((ZERO f) x) x)
(define (((SUCC n) f) x) ((n f) (f x)))
(define (((SUCC* n) f) x) (f ((n f) x)))

(define (PRED n)
  ;; (f (PAIR n _)) = (PAIR n+1 n), so
  ;; ((n f) (PAIR 0 0)) = (PAIR n n-1) if n>0, (PAIR 0 0) otherwise
  (define (f np) ((PAIR (SUCC (CAR np))) (CAR np)))
  (CDR ((n f) ((PAIR ZERO) ZERO))))

(define (PLUS n) (n SUCC))
(define (((TIMES n) m) s) (n (m s)))

(define ONE (SUCC ZERO))
(define TWO (SUCC ONE))
(define THREE (SUCC TWO))
(define FOUR ((PLUS TWO) TWO))
(define EIGHT ((PLUS FOUR) FOUR))

(define ex1 (lambda (x) (lambda (y) (x (lambda (z) ((z y) x))))))
(define ex2 (lambda (x) (lambda (y) (x (lambda (z) ((z x) y))))))

;; diverges on fixed-point combinators
(define (Y f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))
(define (Z f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))

(define ((mkfact fact) n)
  (IF (ZERO? n) ONE ((TIMES n) (fact (PRED n)))))

(define fact (Z mkfact))

;; ----

(define interesting
  (lambda (f)
    ((lambda (y) f) (f f))))

(define weird
  (lambda (f)
    ((lambda (z) f)
     (f (lambda (y) ((lambda (x) (x x)) (lambda (x) (x x))))))))
