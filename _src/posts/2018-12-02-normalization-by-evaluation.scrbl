;; Date: 2018-12-02T00:21:43
;; Tags: DRAFT
#lang scribble/manual
@(require "util.rkt"
          (for-label racket/base))

@title{normalization by evaluation}

At this year's @hyperlink["https://con.racket-lang.org"]{RacketCon}
@hyperlink["https://github.com/racket/racket/wiki/2018-RacketCon-Office-Hours-Teams-&-Workshops"]{office
hours}, @hyperlink["http://davidchristiansen.dk/"]{David Thrane
Christiansen} talked about ``normalization by evaluation'' as a
prelude to his ``Implementing Dependent Types'' session.

In the design he sketched, the normalizer needed help from the
evaluator. I wondered if it was possible to use a standard (oblivious)
evaluator, such as Racket's normal @racket[eval]. The answer is
yes---kind of.

<!-- more -->

@blogsection{Normal forms}

To be precise, we are working with the following language:

@racketgrammar*[
#:literals (lambda)
[EXP X
     (lambda (X) EXP)
     (EXP EXP)]
[X   @#,(elem "variable name")]
]

A @emph{normal form} (specifically, a β-normal form) is a term that
does not contain any subterms that are β-redexes, where a β-redex has
the shape @racket[((lambda (X) _EXP) _EXP)].

The following are examples of normal forms:
@racketblock[
(lambda (x) x)
(lambda (f) (lambda (x) (f (f (f x)))))
]
The following is @emph{not} a normal form, even though it is a @emph{value}:
@racketblock[
(lambda (x) ((lambda (y) x) (lambda (z) z)))
]
But the term above @emph{has} a normal form (and our normalizer should
be able to find it). The following is not a normal form and does not
have a normal form:
@racketblock[
((lambda (x) (x x)) (lambda (x) (x x)))
]


normalization is not evaluation

@racketgrammar*[
#:literals (lambda)
[NF AF
    (lambda (X) NF)]
[AF X
    (AF NF)]
]


@blogsection{Normalization}

It's straightforward to write a normalizer that takes a term and produces a normal form (if the term has one). Start with an evaluator for call-by-name, then recur into lambda




@racketblock[
((lambda (z) (lambda (y) y))
 ((lambda (x) (x x)) (lambda (x) (x x))))
]

(define (normalize e)
  (match (evaluate e)
    [`(lambda (,x) ,e)
     `(lambda (,x) ,(normalize e))]
    [`(,e1 ,e2)
     `(,(normalize e1) ,(normalize e2))]
    [x x]))

(define (evaluate e)
  (match e
    [`(,e1 ,e2)
     (match (evaluate e1)
       [`(lambda (,x) ,e2)
        (evaluate (subst e2 x e1))]
       [e1*
        `(,e1* ,e2)])]
    [e e]))


note: must be closed expression, only works here where everything is a lambda term!

@racketblock[
(code:comment "normalize : eval(NF) -> NF")
]

the idea of a ``probe'':

@racketblock[

;; A Probe is a distinguishable procedure with an additional operation,
;; probe->expr. Here's one implementation:
(define (probe? p)
  (and (procedure? p) (procedure-arity-includes? p 0)))
(define (make-probe expr)
  (case-lambda
    [() expr]
    [(arg) (make-probe `(,expr ,(loop arg)))]))
(define (probe->expr p) (p))
]

given that, the normalizer:

@racketblock[

;; normalize : eval(NF) -> NF
(define (normalize p)
  (if (probe? p)
      (probe->expr p)
      (let ([var (gensym)])
        `(lambda (,var) ,(normalize (p (make-probe var)))))))
]

Note: normalizer inherits host system's notion of reduction. So in CBV
host, can only find normal forms reachable by CBV reductions! (??)

testing:

@racketblock[

(define ((zero f) x) x)
(define (((succ n) f) x) ((n f) (f x)))
(define (((succ* n) f) x) (f ((n f) x)))

(define (plus n) (n succ))

(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four ((plus two) two))

(define ex1 (lambda (x) (lambda (y) (x (lambda (z) ((z y) x))))))
(define ex2 (lambda (x) (lambda (y) (x (lambda (z) ((z x) y))))))

;; diverges on fixed-point combinators
(define (Y f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))
(define (Z f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v))))))

]
