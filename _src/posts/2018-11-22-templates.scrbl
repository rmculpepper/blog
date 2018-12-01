;; Date: 2018-11-22T00:00:00
;; Tags: racket, macros
#lang scribble/manual
@(require (for-label racket/base syntax/parse syntax/parse/experimental/template))

@title{new syntax template features}

In Racket 7, the @racket[syntax] form supports two new template subforms:

@itemlist[

@item{@racket[~@] (``splice'') splices the result of its subtemplate (which must
  produce a syntax list) into the enclosing list template, and}

@item{@racket[~?] (``try'') chooses between alternative subtemplates depending
  on whether their pattern variables have "absent" values.}

]

These features originated in @racketmodname[syntax/parse/experimental/template]:
the @racket[template] form was like an extended version of @racket[syntax], and
@racket[?@] and @racket[??] were the splicing forms and the try/else template
forms, respectively. The names were changed to @racket[~@] and @racket[~?] to
avoid name collisions with other libraries. In Racket 7, the old
@racketmodname[syntax/parse/experimental/template] library just exports the new
standard forms under the old names (that is, it exports @racket[syntax] under
the name @racket[template], and so on).

<!-- more -->

@section[#:style 'unnumbered]{Splicing}

The @racket[~@] template form splices its result into the enclosing list
template. It is the template analogue of @racket[syntax-parse]'s @racket[~seq]
form, but it can be used apart from @racket[syntax-parse]. It is useful for
producing syntax with logical groups that don't correspond directly to
parenthesized structure. One example of an unparenthesized logical group is
paired key and value arguments in a call to the @racket[hash] function:

@racketblock[
(define-syntax-rule (zeros-hash key ...)
  (hash (~@ key 0) ...))
(zeros-hash 'a 'b 'c) (code:comment "=> (hash 'a 0 'b 0 'c 0)")
]

Another example is keyword arguments (that is, keyword and expression)
in a function call.

@section[#:style 'unnumbered]{Try/Else}

The @racket[~?] template form produces its first subtemplate's result if that
subtemplate has no ``absent'' pattern variables; otherwise it produces its
second subtemplate (if present) or nothing at all. Absent pattern variables
arise from @racket[syntax-parse]'s @racket[~or] and @racket[~optional] forms,
for example.

Here is an example macro based very loosely on the result of
@racket[define-ffi-definer] from @racketmodname[ffi/unsafe/define]:

@racketblock[
(define-syntax (definer stx)
  (syntax-parse stx
    [(_ name key (~optional (~seq #:make-fail make-fail)))
     #'(define name (lookup key (~? (make-fail 'name) default-fail)))]))
(code:line)
(definer x 'x)
(code:comment "=> (define x (lookup 'x default-fail))")
(definer y 'y #:make-fail make-not-available)
(code:comment "=> (define y (lookup 'y (make-not-available 'y)))")
]


@section[#:style 'unnumbered]{Potential problems and incompatibilities}

There are three ways that the new @racket[syntax] features and implementation
may cause old code to break or misbehave. In the period before the Racket 7
release, we fixed the cases we discovered, but there may be more out there. Here
are descriptions of the potential problems:


@subsection[#:style 'unnumbered]{Nonexistent nested attributes}

If @racket[x] is an attribute bound by @racket[syntax-parse] but @racket[x.y] is
not, then it is now illegal for @racket[x.y] to occur in a syntax template. The
rationale is that @racket[x.y] is probably a mistake, an attempt to reference a
nested attribute of @racket[x] that doesn't actually exist.

@racketblock[
(define-syntax-class binding-pair
  (pattern [name:id rhs:expr]))
(syntax-parse #'[a (+ 1 2)]
  [b:binding-pair
   (list #'b.var    (code:comment "ERROR: b.var is not bound")
         #'b.rhs)]) (code:comment "OK: b.rhs is a nested attr of b")
]

The restriction does not apply to syntax pattern variables bound by
@racket[syntax-case], etc.


@subsection[#:style 'unnumbered]{Using @racket[syntax] and @racket[template] together}

Macros that use @racket[syntax] to produce a @racket[template] expression may
break, because the interpretation of the new template forms will happen at the
outer (@racket[syntax]) level instead of the inner (@racket[template])
level. (The template forms are recognized by binding, so @racket[syntax] will
treat a reference to @racket[?@] from @racket[syntax/parse/experimental/template]
exactly the same as @racket[~@].).

Here's an example loosely based on the @racket[define-ffi-definer] macro:

@racketblock[
(define-syntax (define-definer stx)
  (syntax-case stx ()
    [(_ definer #:default-make-fail default-make-fail)
     #'(begin
         (define dmf-var default-make-fail)
         (define-syntax (definer istx)
           (syntax-case istx ()
             [(_ name value (~optional (~seq #:fail fail)))
              (template
               (define name
                 (lookup value (?? fail (dmf-var 'name)))))])))]))
]

Previously, the @racket[??] would get ignored (that is, treated as a syntax
constant) by @racket[syntax]; it would get noticed and interpreted by the
@racket[template] form in the generated macro. Now it gets interpreted by the
outer macro and since the first subtemplate, @racket[fail], is just a syntax
constant from the @emph{outer} macro's perspective, it always produces the first
subtemplate's result. So the inner macro will fail if used without the
@racket[#:fail] keyword.

One fix is to escape the @racket[??] using ellipsis-escaping:

@racketblock[
.... (lookup value ((... ??) fail (dmf-var 'name))) ....
]

Another fix is to use @racket[quote-syntax] and an auxiliary @racket[with-syntax] binding:

@racketblock[
(define-syntax (define-definer stx)
  (syntax-case stx ()
    [(_ definer #:default-make-fail default-make-fail)
     (with-syntax ([inner-?? (quote-syntax ??)])
     #'(begin
         (define dmf-var default-make-fail)
         (define-syntax (definer istx)
           (syntax-case istx ()
             [(_ name value (~optional (~seq #:fail fail)))
              (template
               (define name
                 (lookup value (inner-?? fail (dmf-var 'name)))))]))))]))
]

A better fix is to avoid having the outer macro generate the inner macro's
entire implementation and use a compile-time helper function instead:

@racketblock[
(begin-for-syntax
  (code:comment "make-definer-transformer : Identifier -> Syntax -> Syntax")
  (define ((make-definer-transformer dmf-var) istx)
    (syntax-case istx ()
      [(_ name value (~optional (~seq #:fail fail)))
       (with-syntax ([dmf-var dmf-var])
         (template
           (define name
             (lookup value (?? fail (dmf-var 'name))))))])))

(define-syntax (define-definer stx)
  (syntax-case stx ()
    [(_ definer #:default-make-fail default-make-fail)
     #'(begin
         (define dmf-var default-make-fail)
         (define-syntax definer
           (make-definer-transformer (quote-syntax dmf-var))))]))
]

@subsection[#:style 'unnumbered]{Strict argument checking in @racket[syntax/loc]}

The second issue is not directly related to the new subforms. The
@racket[syntax/loc] form applies the source location of its first argument to
the syntax produced by the template---but only if the outermost syntax wrapping
layer comes from the template and not a pattern variable. For example, in the
expression

@racketblock[
(with-syntax ([x #'(displayln "hello!")])
  (syntax/loc src-stx x))
]

the location of @racket[src-stx] is @emph{never} transferred to the resulting
syntax object. If the old implementation of @racket[syntax/loc] determined that
the first argument was irrelevant, it discarded it without checking that it was
a syntax object. So, for example, the following misuse ran without error:

@racketblock[
(with-syntax ([x #'(displayln "hello!")])
  (syntax/loc 123 x))
]

In Racket 7, @racket[syntax/loc] is rewritten to handle the new
subforms---consider, for example, the template @racket[(~? x (+ 1 2))]; the
source location of the second template should be overridden, but not the
first. But the new implementation always checks the first argument. So some
programs that previously got away with bad source arguments will raise errors in
Racket 7.
