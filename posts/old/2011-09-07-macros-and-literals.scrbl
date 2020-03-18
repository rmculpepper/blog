;; Date: 2011-09-07T00:00:00
;; Tags: racket, macros
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-label racket/base racket/class racket/unit syntax/parse))

@title{macros and literals}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Macros often have associated auxiliary identifiers (sometimes called
@emph{keywords} or @emph{reserved words}, although both terms are
problematic in Racket). For example, @racket[cond] has @racket[else];
@racket[class] has @racket[public], @racket[private], etc;
@racket[unit] has @racket[import] and @racket[export].

The fundamental question is what constitutes a use of an auxiliary
identifier, and there are two reasonable answers: @emph{symbolic
equality} and @emph{referential equality}. By symbolic equality I
mean, for example, that any identifier written with exactly the
letters @tt{else} is accepted as an @racket[else] auxiliary form. By
referential equality I mean any identifier that @emph{refers to}
(using the standard notions of binding, environments, etc) the binding
identified as the @racket[else] binding.

@(the-jump)

The advantage of symbolic equality is its simplicity. The problem is
that it conflicts with other kinds of terms. The most common use of
auxiliary identifiers is to distinguish special sub-forms that require
special interpretation from standard forms such as expressions or
definitions. For example, the first sub-term of a @racket[cond] clause
is either the auxiliary @racket[else] or an expression; a class-body
form is either a @racket[public] form, a @racket[private] form, ...,
or a definition or expression. But a variable can be named
@racket[else], and a function can be named @racket[private]... except
that with symbolic auxiliaries, in some contexts a reference to such a
variable or function will be given a drastically different
interpretation, just because of its @emph{name}.

We like to think that names don't matter, as long as they're used
consistently and don't collide with other names in use. And,
crucially, our notion of @emph{collision} is based on
@emph{binding}. The problem with symbolic auxiliaries is that they are
ghosts; they don't collide with other bindings, but they still change
the interpretation of an identifier in some contexts (but not others).

The alternative is referential auxiliaries, in which the special
interpretation of the auxiliary identifier is tied to a binding. The
virtue of this approach is that it relies on the standard mechanisms
of scoping. An identifier cannot refer to @emph{both} a variable
binding and the @racket[else] auxiliary in the same scope. Having a
dedicated binding means the notion of @racket[else] has an existence
more concrete than just ``a special interpretation that @racket[cond]
gives to identifiers spelled a certain way''; one consequence is that
@racket[else] can be documented using Racket's binding-based
documentation system.

Auxiliary bindings collide like (and with) ordinary bindings, and
these collisions can be resolved using standard namespace management
tools such as import renaming (@racket[rename-in]). Alas, there is
no way to protect a referential auxiliary identifier from being
shadowed, but at least with it behaves consistently: shadow
@racket[else] and it no longer acts like @racket[else], but
after all, shadow @racket[lambda] and it no longer acts like
@racket[lambda]. I do wish there were a way to mark certain names
unshadowable, though; I think it would be a net win for usability.

(Note that all of these arguments in favor of referential auxiliary
identifiers also apply to syntax-parameters as a superior alternative
to unhygienic binding. See
@me-link["/2006/04/macros-parameters-binding-and-reference"]{my old blog
post on the subject}.)

Scheme uses referential equality for macro ``literals lists'' (used to
recognize auxiliary identifiers). Unfortunately, it allows literals
that don't refer to any binding, creating another kind of ghostly
identifier interpretation: it collides with nothing but can be
overridden by any other binding that comes along.

When I created @racket[syntax-parse], I made it an error to
specify a literal identifier that had no corresponding binding. This
has its own difficulties, however, which I will describe in a separate
post.
