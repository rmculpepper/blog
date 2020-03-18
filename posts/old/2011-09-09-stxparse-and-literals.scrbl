;; Date: 2011-09-09T00:00:00
;; Tags: racket, macros
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket/base syntax/parse))

@title{syntax-parse and literals}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(begin
  (define-syntax ==>
    (make-element-id-transformer (lambda _ #'(elem "⇒")))))

In @me-link["/2011/09/macros-and-literals"]{my
last post}, I talked about macros and referential auxiliary
identifiers---what we usually call a macro's ``literals.'' Scheme
macro systems only get it half right, though, because while they
compare identifiers using referential equality (i.e., using the
@racket[free-identifier=?] predicate), they allow literals to refer to
nonexistent bindings. While the comparison is well-defined via the
definition of @racket[free-identifier=?], at a higher level the idea
is nonsensical.

In contrast, @racket[syntax-parse] requires that every literal
refer to some binding. (I'll sometimes refer to this requirement as
the @emph{is-bound} property for short.) This requirement is
problematic in a different way. Specifically, this property cannot be
checked statically (that is, when the syntax-parse expression
containing the literal is compiled).

@(the-jump)

That might strike you as bizarre or unlikely. After all, you can
easily imagine checking that a @racket[syntax-rules] macro, say,
satisfies the @emph{is-bound} property. But in Racket, not every macro
uses @racket[syntax-rules], and—more importantly—not every bit of
syntax-analyzing code is a macro. And both of these facts have to do
with @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{phases}.

In Racket, an identifier might mean one thing in one phase
and another thing in a different phase. Actually, this is true for
every identifier: it is bound in finitely many phases and unbound in
infinitely many more. When an identifier is bound in multiple phases,
it is usually bound to the same thing in each phase, but that is a
consequence of the way we write (tasteful) code, not a rule imposed by
the macro system. Consequently, identifier comparisons must take phase
levels into account, and Racket's version of
@racket[free-identifier=?] takes an optional phase level argument,
which defaults to the value of
@racket[(syntax-local-phase-level)], which usually turns out to be
just what you want.

There are two modes of dealing with syntax in Racket, which I will
label ``eval'' and ``macro.'' In ``eval'' mode, your code is just an
ordinary program that manipulates syntax. Perhaps it takes some code,
expands it, analyzes the result, and instruments it (e.g.,
@racketmodname[errortrace]). Perhaps it constructs code to be executed
(e.g., parts of DrRacket's ``module language'' implementation). In any
case, your program is running at phase 0 and it deals with syntax
which represents another program at phase 0.

In ``macro'' mode, on the other hand, your code is either a macro
definition or a helper to a macro definition. Your code is runs at
phase 1 and deals with syntax representing code at phase 0. Actually,
since your macro might be used in the implementation of some other
macro (possibly itself being used in the implementation a macro, and
so on), your code runs at phase N+1 and deals with syntax representing
code at phase N. You'd better not depend on the particular value of N,
either.

So in ``eval'' mode, you generally want to compare identifiers at phase
0, but in ``macro'' mode, you want to compare identifiers at phase N-1,
where N is the phase of the code currently executing. No problem!
Since ``macro'' code is always at phase 1 or higher, these situations
are distinguishable, and we can choose the right default for each of
them:

@racketblock[
(define (syntax-local-phase-level)
  (cond [(= (phase-of-executing-code) 0) 0]
        [else (sub1 (phase-of-executing-code))]))
]

So what's the problem?

Consider the following bit of code:

@racketmod[#:file "is-lambda.rkt"
racket
(require _???)

(code:comment "is-lambda? : syntax -> boolean")
(code:comment "Determines if this is a lambda form.")
(define (is-lambda? stx)
  (syntax-parse stx #:literals (lambda)
    [(lambda formals . body) #t]
    [_ #f])) 
(provide is-lambda?)
]

What phase will be used for the @racket[lambda] literal
comparison?

Well, if this code is used as a macro helper---that is, this module is
required @racket[for-syntax]---then the phase will be the phase of the
module minus 1, so the module had better have a @racket[(require
(for-template racket))]. On the other hand, if this code is used in
``eval'' mode, the comparison will be done with respect to phase 0, so
we need just @racket[(require racket)]---of course, we have that from
the module's language anyway.

(Actually, it's even worse than that. Racket's
@racket[syntax-local-phase-level] isn't as simple as my definition
above; for example, it seems to always be 0 for the initialization of
a module, even if the module is instantiated at a phase greater than
1.)

So, should what phase(s) should we check that @racket[lambda] is bound
in? Checking only one is not adequate to enforce the @emph{is-bound}
property, but demanding both is unreasonable---adding a require line
is costly at best, semantics-altering at worst. Thus the check must be
done dynamically.

Well, not entirely. It turns out that checking if an identifier is
bound is relatively expensive. And while we don't know what phase(s)
the comparison will actually use at run time, we know it's highly
likely to be either N or N-1, where N is the phase of the module. If
we pre-compute those answers at compile time, we can use them as a
fast-path check that only requires numeric comparison (cheap)... and
if we get a really weird phase level, we can revert to the slow check.

And that, I think, is the best you can do with literals and standard
@racket[free-identifier=?]. On the other hand, what if you had a
cross-phase identifier comparison, one that determines if
@emph{identifierA} refers to the same binding at @emph{phaseA} that
@emph{identifierB} refers to at @emph{phaseB}? Then you could fix the
phase for the literal identifier and check it at compile time. That's
how @racket[syntax-parse]'s literal-sets work.
