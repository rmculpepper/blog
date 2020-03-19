;; Date: 2018-12-02T00:21:43
;; Display: draft
#lang scribble/manual
@(require "old/util.rkt"
          (for-label racket/base))

@title{oblivious normalization by evaluation}

At this year's @hyperlink["https://con.racket-lang.org"]{RacketCon}
@hyperlink["https://github.com/racket/racket/wiki/2018-RacketCon-Office-Hours-Teams-&-Workshops"]{office
hours}, @hyperlink["http://davidchristiansen.dk/"]{David Thrane
Christiansen} talked about ``normalization by evaluation'' as a
prelude to his ``Implementing Dependent Types'' session.

In the design he sketched, the normalizer needed help from the
evaluator. I wondered if it was possible to use a standard (oblivious)
evaluator, such as Racket's normal @racket[eval]. The answer is no ---
but kind of.

<!-- more -->

@; ----------------------------------------
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
does not contain any β-redexes. That is, it does not contain any
subterm that the β reduction rule can be applied to:
@racketblock[
((lambda (_X) _EXP₁) _EXP₂) → _EXP₁[_EXP₂ @#,elem{/} _X]  (code:comment "β")
]
where the notation @racket[@#,elem{_}[@#,elem{_} @#,elem{/} @#,elem{_}]] means
capture-avoiding substitution.

The following terms are examples of normal forms:
@racketblock[
(lambda (x) x)
(lambda (f) (lambda (x) (f (f (f x)))))
]
The following is @emph{not} a normal form, even though it is a
@emph{value} (we'll get back to values in a moment):
@racketblock[
(lambda (x) ((lambda (y) y) (lambda (z) (z z))))
]
But the term above @emph{has} a normal form (and our normalizer should
be able to find it):
@racketblock[
(lambda (x) (lambda (z) (z z)))
]
We consider a term to have a normal form if we can convert it into a
normal form by applying the β rule any number of times, anywhere in
the term (even within lambda-abstractions).

The following is not a normal form and does not have a normal form:
@racketblock[
((lambda (x) (x x)) (lambda (x) (x x))) (code:comment "usually called Ω")
]

Normal forms have the following shape, described by the @racket[_NF]
nonterminal:
@racketgrammar*[
#:literals (lambda)
[NF NEU
    (lambda (X) NF)]
[NEU X
     (NEU NF)]
]
A normal form is either a @emph{neutral} term (@racket[_NEU]) or a
lambda-abstraction containing a normal form. A neutral is either a
variable or a neutral applied to a normal form. In other words, a
neutral term is a normal form that is not a lambda abstraction.

@; ----------------------------------------
@blogsection{CBN Evaluation}

If we consistently apply the β rule at the leftmost, outermost place
where it can be applied, except within a @racket[lambda] body, then we
get @emph{call-by-name (CBN) evaluation}.

Here's a simple CBN evaluator:

@racketblock[
(code:comment "evaluate : EXP → EXP")
(define (evaluate e)
  (match e
    [`(,e1 ,e2)
     (match (evaluate e1)
       [`(lambda (,x) ,e2)
        (evaluate (subst e2 x e1))]
       [e1*
        `(,e1* ,e2)])]
    [e e]))
]

Note that the evaluator allows open terms. For example, @racket[(evaluate 'x)]
produces @racket['x]. Also note that the evaluator diverges when given
a term like @racket[Ω]. If the original term is closed, then the
second inner @racket[match] case is impossible, and the evaluator
either returns a lambda-abstraction or it diverges.

Given the CBN evaluator above, it is straightforward to write a
normalizer that takes a term and produces a normal form (if the term
has one). We start by evaluating the term, then we recur into its
immediate subexpressions and normalize them.

@racketblock[
(code:comment "normalize : EXP → NF")
(define (normalize e)
  (match (evaluate e)
    [`(lambda (,x) ,e)
     `(lambda (,x) ,(normalize e))]
    [`(,e1 ,e2)
     `(,(normalize e1) ,(normalize e2))]
    [x x]))
]




@; ----------------------------------------
@blogsection{CBV Evaluation}

A @emph{value} (@racket[_VAL]) is either a variable or a
lambda-abstraction:
@racketgrammar*[
#:literals (lambda)
[VAL X
     (lambda (X) EXP)]
]

Evaluation in Racket, however, is not based on the β rule, but on a
restricted form called βᵥ (``β-value''):
@racketblock[
((lambda (_X) _EXP₁) _VAL₂) → _EXP₁[_VAL₂ @#,elem{/} _X]  (code:comment "βᵥ")
]
.... where value ....

In addition, unlike normalization, where we are allowed to apply the
reduction rule anywhere we like, Racket does not evaluate within the
body of a lambda-abstraction.

Given these two differences, is it possible to build a β-normalizer
out of a βᵥ-evaluator?

No. Not one that uses the evaluator in any meaningful way,
anyway. Consider the following term:
@racketblock[
((lambda (z) (lambda (y) y)) Ω)
]
where @racket[Ω] is that example from earlier of a term without a
normal form. This term has a normal form: @racket[(lambda (y) y)].

If we apply the evaluator to this term, it diverges (that
is, it fails to terminate). If we do case analysis first and apply the
evaluator to the subterms, it diverges on the second. We could do
deeper case analysis, but at that point, we're just building a
normalizer from scratch.

The problem is that while this example has a normal form (reachable in
one β step), that normal form is not reachable through any number of
βᵥ steps --- it gets trapped by @racket[Ω]. That shouldn't be
surprising: βᵥ is weaker than β.

@;{
Side note: β-NF and βᵥ-NF are different. For example,

  ((lambda (x) x) (y y))

is a βᵥ-normal form but not a β-normal form.
}


@; ----------------------------------------
@blogsection{Oblivious normalization and un-evaluation}

Let's refine the question. If there is a normal form reachable through
βᵥ steps, can we find it using a βᵥ evaluator? Racket's @racket[eval]
procedure, for example.

Now we must reckon with another issue: Racket's @racket[eval] takes an
encoding of a term (as an S-expression made of lists and symbols) and
produces a Racket value, and Racket provides no way to convert a
function value back to a term encoding.







There are two different @emph{notions of reduction} that generate different 

((lambda (X) _EXP₁) _VAL₂) → EXP₁[VAL₂/X]  ;; βᵥ


normalization is not evaluation


@blogsection{Normalization}




@racketblock[
((lambda (z) (lambda (y) y))
 ((lambda (x) (x x)) (lambda (x) (x x))))
]


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
