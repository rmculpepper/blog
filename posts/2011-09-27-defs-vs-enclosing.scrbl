;; Date: 2011-09-27T00:00:00
;; Tags: racket
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket/base racket/match))

@title{definitions vs enclosing binding forms}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(begin
  (define-syntax ==>
    (make-element-id-transformer (lambda _ #'(elem "⇒"))))
  (define-syntax let-struct
    (make-element-id-transformer (lambda _ #'(racketkeywordfont "let-struct"))))
  (define-syntax letrec-struct
    (make-element-id-transformer (lambda _ #'(racketkeywordfont "letrec-struct")))))

There are two kinds of binding forms in Racket: definitions and
enclosing binding forms. The scope of a binding introduced by an
enclosing binding form is entirely evident: it's one (or more) of the
form's sub-terms. For example, in

@racketblock[
(lambda (_var ...) _body)
]

the scope of the @racket[_var] bindings is @racket[_body]. In
contrast, the scope of a definition is determined by its context: the
enclosing @racket[lambda] body, for example, or the enclosing
module---except that scope is too simple a term for how bindings work
in such contexts. Enclosing binding forms are simpler and cleaner but
weaker; definition forms are more powerful, but have a more
complicated binding structure. Definitions also have the pleasant
property of reducing rightward code drift.

@(the-jump)

Definitions are more powerful than enclosing binding forms because
they can be used to construct more binding structures. Definitions are
composable building blocks for environments; a crucial property of
definitions is that they are complete entities on their own, absent
the expressions (or more generally, forms) that will be used in their
bindings' scope. The expressions in the scope of an enclosing binding
form, on the other hand, are fixed; they're part of the term.

It is easy to construct an enclosing form given a definition form, but
it is difficult to construct a definition form given an enclosing
form. For example, consider the creation of structure types; there is
a @racket[struct] definition form that defines the constructor,
predicate, accessors, etc. Here's how to turn that into an enclosing
binding form:

@racketblock[
(let-struct _name (_field ...) _body)
  ==> (let () (struct _name (_field ...)) (let () _body))
]

We just use @racket[let] to open up a new local definition context,
use @racket[struct] to put its bindings in that context, and place
@racket[_body] in that context---but within its own @racket[let], to
prevent it from perhaps attempting to insert its own conflicting
bindings into the definition context.

If you're finicky, perhaps you've noticed that this is actually more
of a @racket[letrec-struct], since the scope of the introduced names
includes any sub-expressions of the @racket[struct] form---or would,
if our macro supported the same options that @racket[struct] does. We
could give @racket[let-struct] true @racket[let]-scoping instead of
@racket[letrec]-scoping by lifting any sub-expressions out of the
definition context. For example:

@racketblock[
(let-struct _name (_field ...) #:inspector _insp-expr _body)
 ==> (let ([insp _insp-expr])
       (struct _name (_field ...))
       (let () _body))
]

More work, yes, but still feasible. (Alternatively, I conjecture that
we could use marks and rename-transformers in a clever way to hide the
struct names from @racket[_insp-expr] but make them available to
@racket[_body]. See if you can work it out. You might find
@hyperlink["http://www.ccs.neu.edu/scheme/pubs/#gpce05-cof"]{Syntactic
Abstraction in Component Interfaces} helpful. Alternatively, you could
try using internal-definition-contexts; see the implementation of
@racketmodname[racket/splicing], for example.)

What would it take to go the other direction? Here's a first stab at
it:

@racketblock[
(struct _name (_field ...))
  ==> (define-values (_name _name? _name-field ...)
        (let-struct _name (_field ...)
          (values _name _name? _name-field ...)))
]

Bzzzt, wrong: @racket[struct] is supposed to bind @racket[_name] as a
macro that not only acts as the constructor but also records
compile-time information about the struct type that can be used by
other macros like @racket[match], the @racket[struct-out] provide
form, etc.

It's easy to extrude a value from the scope it was created in... at
least, many modern languages have mostly figured it out, although some
still manage to bungle it. But there's no way to extrude a macro from
a local scope to an outer scope. And if there were, we'd have to
rethink what references to local names no longer in scope
meant---that's why I called it extrusion: because it reminds me of the
type extrusion problem.

We could, of course, just re-compute the macro part of the
@racket[struct] expansion. But now we're up to 1.5 implementations of
struct-related macros. Better to do the work once in the definition
form and naively reuse it to create @racket[let-struct] (if we even
care about @racket[let-struct]). Advantage: definitions.

This power comes at a cost, though, namely the @emph{two-pass expansion of
definition contexts}.

(to be continued...)
