;; Date: 2020-03-20
;; Display: draft
#lang scribble/manual
@(require scribble/example
          (for-syntax racket/base)
          scribble/racket
          "util.rkt"
          (for-label racket/base))

@(define-syntax Quote
   (make-element-id-transformer (lambda _ #'(racket quote))))

@(define (βv) @elem{β@subscript{v}})

@title{oblivious normalization by evaluation}

At @hyperlink["https://con.racket-lang.org/2018/"]{RacketCon 2018's}
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

Note: In this post, whenever I say ``normal form'', I always mean
β-normal form; and whenever I say ``has a normal form'', I mean it has
a β-normal form @emph{reachable through β steps}. That is, even once I
introduce other kinds of steps, the idea of having a normal form is
tied to β; when I talk about whether the normal form is reachable
through other kinds of steps, I'll say so explicitly.

@; ----------------------------------------
@blogsection{CBN Evaluation}

If we consistently apply the β rule at the leftmost, outermost place
that it can be applied, except within a @racket[lambda] body, then we
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
       [e1* (code:comment "open term!")
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

Evaluation in Racket, however, is not based on the β rule, but on a
restricted form called @(βv) (``β-value''):
@racketblock[
((lambda (_X) _EXP₁) _VAL₂) → _EXP₁[_VAL₂ @#,elem{/} _X]  (code:comment @#,(βv))
]
where a @emph{value} (@racket[_VAL]) is either a variable or a
lambda-abstraction:
@racketgrammar*[
#:literals (lambda)
[VAL X
     (lambda (X) EXP)]
]
If we apply @(βv) at the leftmost, outermost place that it can be
applied, except within a @racket[lambda] body, then we get
@emph{call-by-value (CBV) evaluation}. (The terms ``call-by-name''
and ``call-by-value'', as evaluation strategies, are unrelated to the
terms ``call (or pass) by value'' and ``call (or pass) by reference'' used
in the context of languages like C, C++, Java, etc.)

Given these two differences, is it possible to build a β-normalizer
out of a CBV-evaluator?

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
@(βv) steps --- it gets trapped by @racket[Ω]. That shouldn't be
surprising: @(βv) is weaker than β.

@;{
Side note: β-NF and @(βv)-NF are different. For example,

  ((lambda (x) x) (y y))

is a @(βv)-normal form but not a β-normal form.
}


@; ----------------------------------------
@blogsection{Un-evaluation}

@(define eval-v1 (make-base-eval))
@(eval-v1 '(require "../examples/2020/normalize-v1.rkt"))

Let's refine the question. If there is a normal form reachable through
@(βv) steps, can we find it using a CBV evaluator? Racket's @racket[eval]
procedure, for example?

Now we must reckon with another issue: Racket's @racket[eval] takes an
encoding of a term (as an S-expression made of lists and symbols) and
produces a Racket value, and Racket provides no way to convert a
function value back to a term encoding. But perhaps we could build
such an ``un-evaluator'', at least if we're allowed to assume that the
value we're un-evaluating corresponds to a normal form.

So let's try to build a normalizing un-evaluator, with the following
signature:
@racketblock[
(code:comment "un-eval : eval(NF) -> NF")
]
That is, @racket[un-eval] must be given something that is (or is
``equivalent'' to) the result of calling @racket[eval] on some
(closed) normal form, and it will return an equivalent normal form. Of
course, @racket[un-eval] might choose different variable names. For
example, we might have the following:
@examples[#:eval eval-v1 #:label #f
(un-eval (eval '(lambda (z) z)))
(un-eval (lambda (z) z))
]

The only possible value we can get from evaluating a closed normal
form is an ordinary Racket function, and the only thing we can do with
that is to apply it to an argument. So we should apply the function to
some sort of value that will allow us to map out its behavior.

For example, suppose we apply the function to the symbol @racket['a]
and we get back @racket['a]. Then we know that the function
corresponds to the normal form @racket[(lambda (x) x)] --- remember,
we're assuming that the function is the result of @racket[eval]ing
some term in the language above, so we don't consider possibilities
like @racket[(lambda (x) 'a)] or @racket[(lambda (x) (if (symbol? x) x
12))]. But what if the function is like @racket[(lambda (x) (lambda
(y) x))] and doesn't return the symbol immediately? Or worse, what if
the function is like @racket[(lambda (x) (x x))] and tries to apply
its argument? Sending a symbol is not a good strategy.

We need to send a probe that acts like a function, but that we can
recognize and turn into a term encoding. Racket gives us many ways to
create such functions; here's one approach using applicable
structures:
@racketblock[
;; A Probe is (probe S-Expr).
(struct probe (term)
  #:property prop:procedure
  (lambda (self arg)
    (probe `(,(probe-term self) ,(un-eval arg)))))
]

That is, a @racket[probe] instance supports the operation
@racket[probe-term] that returns an S-expression representation of the
term, but it also acts as a procedure of one argument. When applied, a
probe just creates a new probe whose term is an application term where
the operator is the original probe's term and the operand is a term
representing the argument. We convert the argument to a term using
@racket[un-eval], which we are about to define.

The @racket[un-eval] function must handle two cases: ordinary Racket
functions that we want to un-eval, and probes that we have introduced
to explore the first kind of function. Since we can recognize probes
with the @racket[probe?] predicate, we can implement @racket[un-eval]
by case analysis:

@racketblock[
(code:comment "un-eval : eval(NF) -> NF")
(define (un-eval p)
  (if (probe? p)
      (probe-term p)
      (let ([var (gensym)])
        `(lambda (,var) ,(un-eval (p (probe var)))))))
]

That is, to un-eval a probe, we just extract its term using
@racket[probe-term]. To un-eval another kind of function, we first
generate a fresh variable name, then we return a lambda expression
with the fresh variable and a body constructed by probing the given
function.

We can now see how @racket[un-eval] corresponds to the @racket[_NF]
grammar. A probe always carries a neutral term (@racket[_NEU]), and
@racket[un-eval] returns either a neutral term (case 1) or a
lambda-abstraction whose body is a normal form (case 2).

@examples[#:eval eval-v1 #:label #f
(un-eval (lambda (z) z))
(un-eval (lambda (x) (lambda (y) (x (lambda (z) ((z y) x))))))
]

See @ex-link["2020/normalize-v1.rkt"]{this code} for a slightly nicer
implementation and more examples, including Church numerals and a
factorial function defined via the @racket[Z], the call-by-value
fixed-point combinator. For example, we have the following:
@examples[#:eval eval-v1 #:label #f
(un-eval THREE)
(un-eval (fact THREE))
]
Note that @racket[Z], @racket[fact], and recursive functions in
general do not have normal forms, but @racket[(fact THREE)] is just a
Church numeral, and it has the normal form above (the Church encoding
of 6).

Sadly, @racket[un-eval] cannot always find a normal form, even when
one exists. For example, it diverges on the following term:
@racketblock[
((lambda (z) (lambda (y) y))
 ((lambda (x) (x x)) (lambda (x) (x x))))
]
This term has the normal form @racket[(lambda (y) y)], but the normal
form is not reachable via @(βv) steps.

So perhaps we can characterize @racket[un-eval] with the following
statement: if @racket[_EXP] has a β-normal form @racket[_NF] reachable
by a sequence of @(βv) steps, then @racket[(un-eval (eval (Quote _EXP)))]
produces @racket[(Quote _NF)] (modulo α-renaming).

@; ----------------------------------------
@blogsection{Beyond @(βv)}

But wait! Here's an interesting term that takes a function @racket[f],
applies it to itself, and then discards the result and just returns
@racket[f]:
@racketblock[
(define interesting
  (lambda (f)
    ((lambda (y) f) (f f))))
]
or equivalently, with a little clarifying syntactic sugar:
@racketblock[
(define interesting
  (lambda (f)
    (let ([y (f f)])
      f)))
]
This term has the β-normal form @racket[(lambda (f) f)], but the
normal form is not reachable through @(βv) steps.

It's worth emphasizing here that while @racket[interesting] and
@racket[(lambda (f) f)] are equivalent in the λ-calculus (generated by
β), they are @emph{not} equivalent in the λᵥ-calculus (generated by
@(βv)). They must not be, because under CBV evaluation @emph{the two
terms act differently}! For example, @racket[(interesting (lambda (x) x))]
evaluates to @racket[(lambda (x) x)] and
@racket[(interesting (lambda (x) (x x)))] diverges.

But here's a surprise: @racket[un-eval] calculates the β-normal form
for this term:
@examples[#:eval eval-v1 #:label #f
(un-eval interesting)
]
But how?

@(define (βneu) @elem{β@subscript{neu}})

Our probing strategy has turned the inner @racket[(f f)] into a value
at the Racket level, even though it is not a value at the term
level. In general, any (closed) neutral term is represented by a
Racket value (a @racket[probe] instance). So effectively we've added
another notion of reduction to our evaluator:
@racketblock[
((lambda (_X) _EXP₁) _NEU₂) → _EXP₁[_NEU₂ @#,elem{/} _X] (code:comment @#,(βneu))
]

If our purpose were to faithfully follow the restrictions of @(βv)
reduction, this should be alarming. But this whole adventure started
with the question of whether CBV-evaluation can simulate (or at least
approximate) β-normalization, so let's see how far we can push it.

Recall the definition of neutral term (@racket[_NEU]):
@racketgrammar*[
[NEU X (NEU NF)]
]
Probes correspond to neutral terms: a base probe is created to
represent a variable when we create a @racket[lambda] abstraction; and
when we extend a probe through application, we normalize the argument.

What do we lose by eagerly normalizing the argument?

Here's a weird term that takes a function @racket[f], applies it to a
value that doesn't have a normal form, throws away that result, and
then just returns @racket[f].
@racketblock[
(define weird
  (lambda (f)
    ((lambda (z) f)
     (f (lambda (y) ((lambda (x) (x x)) (lambda (x) (x x))))))))
]
This term has the normal form @racket[(lambda (f) f)], but it is not
reachable through @(βv) steps. And indeed, @racket[un-eval] diverges ---
but only because it tries calculating the normal form of the value
passed to @racket[f], even though that result is discarded.

This example suggests a modification to our probe representation:
delay the normalization of arguments until the probe's term is needed
for the body of a lambda-abstraction. Here are the updated definitions:

@(define eval-v2 (make-base-eval))
@(eval-v2 '(require "../examples/2020/normalize-v2.rkt"))

@racketblock[
(code:comment "A Probe is (probe (promise-of Neutral))")
(struct probe (term-p)
  #:property prop:procedure
  (lambda (self arg)
    (probe (delay `(,(probe-term self) ,(un-eval arg))))))

(code:comment "probe-term : Probe -> Neutral")
(define (probe-term p) (force (probe-term-p p)))

(code:comment "un-eval+ : eval(NF) -> NF")
(define (un-eval+ p)
  (if (probe? p)
      (probe-term p)
      (let ([var (gensym*)])
        `(lambda (,var) ,(un-eval (p (probe (delay var))))))))
]

The @ex-link["2020/normalize-v2.rkt"]{new @racket[un-eval+]} agrees
with the old version on the examples where the old one worked:
@examples[#:eval eval-v2 #:label #f
(un-eval+ (lambda (z) z))
(un-eval+ (lambda (x) (lambda (y) (x (lambda (z) ((z y) x))))))
(un-eval+ THREE)
(un-eval+ (fact THREE))
]
And the new version of @racket[un-eval] reports the ``expected''
normal form for @racket[weird]:
@examples[#:eval eval-v2 #:label #f
(un-eval+ weird)
]

@(define (βnval) @elem{β@subscript{nval}})

Now a @racket[probe] instance corresponds to a term like a neutral
term but with value arguments (@racket[_NVAL]):
@racketgrammar*[
[NVAL X (NVAL VAL)]
]
and @racket[un-eval+] effectively implements a new notion of
reduction:
@racketblock[
((lambda (_X) _EXP₁) _NVAL₂) → _EXP₁[_NVAL₂ @#,elem{/} _X] (code:comment @#,(βnval))
]

I conjecture (but haven't proven) that @racket[un-eval+] satisfies the
following property: If @racket[_EXP] has a β-normal form @racket[_NF]
reachable by a sequence of @(βv) and @(βnval) steps, then
@racket[(un-eval (eval (Quote _EXP)))] produces @racket[(Quote _NF)]
(modulo α-renaming).


@(close-eval eval-v1)
@(close-eval eval-v2)


@;{Extensionality

The applicable struct implementation of probes is nice for Racket, but
it's pretty Racket-specific. There are other alternatives: for
example, we rely on the intensional equality of procedures (aka object
identity) and keep a mutable table to map probe procedures to
(promises of) their term representations.

No, I don't think so. I ran into this problem trying a variation of
the probing technique using delimited continuations (inspired by my
misremembering of ideas from SPCF). Without intensional equality, I
couldn't find a base case. (And with intensional equality, why not
just use the current probing approach?)

To make the problem concrete, if we have a term f, how can we tell if
f is the identity function, (lambda (x) x)?

(Well, one way is to pass an "illegal value" like "hello there". But
that's cheating, because not only do we have to catch a possible
exception if f is strict in its argument, but the illegal value is
essentially cover for breaking extensionality. So this is not
progress.)

Well, we can send f a function, and if we get the same function back,
it's the identity function. But how do we tell (extensionally) whether
we got the same function back?

In particular, if z is a "proper" function, how can you distinguish
between the following:

    z
    (lambda (y) (z y))
    (lambda (y) (lambda (x) ((z y) x)))
    (lambda (y) (lambda (x) (lambda (w) (((z y) x) w))))
    ...

These terms are all η-equivalent, and η represents the principle of
extensionality in the pure lambda calculus. So it seems futile to try
purely extensional methods to try to do β-normalization. What about
βη-normalization? I don't know.

}
