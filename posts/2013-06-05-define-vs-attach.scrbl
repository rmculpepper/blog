;; Date: 2013-06-05T00:00:00
;; Tags: racket
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-label racket syntax/id-table racket/struct-info))

@title{define vs attach}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Racket's macro system gives macros two ways of associating
@emph{static} (or @emph{compile-time}) information with a particular
name. One way is to @emph{define} the name as the static information;
the other is to @emph{attach} the information to the already-defined
name.

@(the-jump)

Much of the power of Racket macros comes from their ability to create
and manipulate novel kinds of static information @cite{MTWT}. For
example, a struct definition associates the struct name with a static
record containing the names (identifiers) of the struct's
functions. One client macros is @racket[match], which uses this
information to transform a struct pattern into code that uses the
struct's predicate and accessors to recognize and destructure
instances.

@declare-keyword[point]
@racketblock[
(struct point (x y))

(match v
  [(point x y) ....]
  ....)
==>
(if (point? v)
    (let ([x (point-x v)] [y (point-y v)]) ....)
    (match v ....))
]

Another client is @racket[struct-out]:

@racketblock[
(struct point (x y))

(provide (struct-out point))
==>
(provide point point? point-x point-y struct:point)
]

Structs provide an interface to their static information
(@racketmodname[racket/struct-info]), so programmers can create
additional client macros.

@blogsection{The @emph{define} approach}

Racket enables this kind of association through an extension to
@racket[define-syntax] that allows the right-hand side to be any
(compile-time) value, not just a macro transformer. In fact, in Racket
any name bound via @racket[define-syntax] to a function is treated as
a macro. Likewise, a struct type is just a name bound via
@racket[define-syntax] to a @seclink/reference["structinfo"]{static
struct record}. The code that @racket[struct] produces to define
@racket[point] looks something like this:

@racketblock[
(struct point (x y))
==>
(begin
 (define-values (make-point point? point-x point-y) ....)
 (define-syntax point
    (make-struct-info .... #'make-point #'point?
                           (list #'point-x #'point-y)
                      ...)))
]

The code is similar for @tech/reference{signatures} and
@tech/reference{match expanders} and
@seclink/reference["for"]{sequence syntaxes} and so on. Client macros
fetch static information using the @racket[syntax-local-value]
function.

The @emph{define} approach seems sensible as long as a name has a
single kind of information associated with it, but what if we want a
name to carry multiple kinds of static information?  For example,
suppose we have an ORM library with a macro that defines struct types
that also carry information about their persistent storage. Or what if
we want a name to carry static information but also act as a value?
For example, a struct name carries the static information discussed
above, but it also acts as a constructor---a function.

I will refer to an independent kind of information attachable to a
name as a @deftech{sense} of information or of meaning.  Note that a
name having multiple senses of information is different from what some
languages call ``multiple namespaces.'' Racket has a single
``namespace'' in that sense: all of the information attached to a name
has the same scope. Consider the following code:

@racketblock[
(struct point (x y))
(lambda (point) ....)
]

The @racket[lambda] expression's binding of @racket[point] shadows not
only the constructor meaning of @racket[point] but also its
struct-name meaning (used by @racket[match] and @racket[struct-out]).

The way a name can carry multiple senses of information is if its
(compile-time) value is appropriate for representing each kind of
information. Whether that is possible depends on the representations
chosen for each sense. If one sense of information is represented as a
vector and another as a list, then there is no way for one name to
encompass both. But if both kinds of information are represented via
@seclink/reference["struct-generics"]{generic interfaces} (or, at a
lower level, @seclink/reference["structprops"]{struct type
properties}), then we can simply create a new struct type that
implements all of the required interfaces.

In particular, a name's behavior as an expression is controlled by the
@racket[prop:procedure] interface. So a struct name, for example, has
a compile-time value (created by @racket[make-struct-info]) that
implements both a specialized struct-type information interface and
the @racket[prop:procedure] interface.  The first interface serves
clients such as @racket[match] and @racket[struct-out], and the second
interface allows struct names to be used as constructors---by
expanding into references to the @emph{real} constructor function.

@blogsection{The @emph{attach} approach}

The problem with the definition approach, however, is that it relies
on all senses of a name to be known and available when the name is
defined. No more can be added later. Consider again the example of
@racket[match] patterns. There are a number of built-in pattern
forms---like @racket[and], @racket[or], and @racket[list]---that share
their names with bindings from @racketmodname[racket/base]. The
@racket[match] form wants to @emph{attach} new sense of meaning---as
pattern forms---to those existing bindings.

If the set of names to give new meaning to is small, fixed, and known
when the client macro is defined---like in the case of
@racket[match]---then it can be done by simple hard-coded identifier
comparisons in the
@me-link["/2011/09/macros-and-literals.html"]{literals list} of a
macro. Otherwise, the new sense of meaning can be represented by an
@emph{identifier-keyed dictionary}. (Like macro literals, identifier
dictionaries should use @racket[free-identifier=?] for key
comparison. The @racketmodname[syntax/id-table] library provides an
efficient hash-based implementation, @seclink[#:doc '(lib
"syntax/scribblings/syntax.scrbl") "idtable"]{free-identifier
tables}.)

In general, the code to @emph{attach} information to the
@racket[_sense] sense of an identifier @racket[_name] will look
something like the following:

@racketblock[
(begin-for-syntax
 (sense-attach! #'name sense-information))
]

where @racket[sense-attach!] is backed by a free-identifier table. The
@racket[begin-for-syntax] form is necessary to update the information
in the correct phase and to make the information ``persistent'' by
performing it every time the enclosing module is visited, not just
once when the enclosing module is compiled @cite{YWIW}.

Using an identifier table relaxes the constraints imposed by the
definition-based mechanism: new meanings can be attached to a name
after its definition, and a name can carry many senses of meaning
without conflicts---just define a new table. 

Typed Racket @cite{LL}, for example, uses the @emph{attach} approach
for its type environment---the mapping of names to their types. In
other words, typed variables have a sense of information which is
their type. When Typed Racket compiles a typed module, it gathers the
names defined in the module together with the types given to (or
inferred for) them and inserts compile-time code into the module to
update Typed Racket's global type environment table when the typed
module is @tech/reference{visit}ed---that is, during the compilation
of any module that depends on it.

For Typed Racket, one benefit of @emph{attach} over @emph{define} is
that the information survives macro expansion. Typed Racket works by
fully expanding a module body, then analyzing (type checking) and
transforming the code (to protect exports from untyped contexts, for
example). Fully expanded code contains no macros or other syntax
bindings, though, so typed variables cannot be represented as
expression macros, and if they were bound to some other kinds of
static information, they wouldn't be usable as variables. Storing type
information in a separate static table solves the problem.

There are drawbacks to the @emph{attach} approach, though. It buys
flexibility in the form of (limited) mutation, and that has some
standard costs. The @emph{attach} approach separates the definition of
a name from the point where some kind of static information is added
to the name, and there are no inherent constraints on when that
information can be added---or how many times. If the ability to attach
information of some sense is exposed to the programmer, for example,
they might use it in two different modules to attach different values
to the same name. If a third module then imports both of those two
modules, then what information should that name be considered to
carry?

A bad answer would be to pick an arbitrary answer (perhaps the
``latest'' information attached for that sense). That corresponds to
blindly updating the table, no matter if it already contained an entry
for the name in question. A better answer would be to raise an error
if multiple sites try to attach information to the same name. That
would be a ``fail early'' approach to conflicts. Another answer might
be to change the state associated with the name to some
``incompatible'' indicator and only signal an error if that sense of
the name is actually used in the program---that is, shift the error
from update-time to lookup-time. 

In many cases, the best approach is to avoid the problem by strictly
limiting the ways in which information can be attached to names. For
example, a library might internally represent information via a
compile-time table, so that it can attach meaning to a few existing
names, but only export a form that follows a @emph{define}-like
discipline. 

@declare-keyword[define-sense]
@racketblock[
(define-sense name ....sense-info....)
==>
(begin (define-syntax name
         (lambda (stx) ....meaning-as-expr....))
       (begin-for-syntax
        (sense-attach! #'name ....sense-info....)))
]

Or it might mark the general @emph{attach} facility as for
power users or extension builders only.

Both @emph{define} and @emph{attach} approaches work with local
bindings too, although the code produced for @emph{attach} must be
adapted: local bindings have no need for cross-module persistence, and
@racket[begin-for-syntax] cannot be used except at module level
anyway. The attaching form should produce the following code instead:

@racketblock[
(define-syntaxes ()
  (begin
   (sense-attach! #'name ....sense-info....)
   (values)))
]

In fact, this is the essense of what @racket[begin-for-syntax] means
for expressions anyway, so the same code can be used for both module
contexts and local (definition) contexts.


@;{
One should aim to ensure that the availability of the information
coincides with the scope of the name.

What about local bindings?
}

@; ----

@;{For another example, Typed Racket records the type of every name
defined in a typed module so that the type-checker can check
occurrences of that name---whether they occur in the same module or in
typed client modules.  }

@bibliography[
@bib-entry[#:key "LL"
           #:title "Languages as libraries"
           #:author (string-append "Sam Tobin-Hochstadt and Vincent St-Amour and Ryan Culpepper"
                                   " and Matthew Flatt and Matthias Felleisen")
           #:location "Programming Language Design and Implementation (PLDI)"
           #:date "2011"
           ;; #:pages 132--141
           #:url "http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf"]
@bib-entry[#:key "MTWT"
           #:title "Macros that work together"
           #:author "Matthew Flatt, Ryan Culpepper, David Darais, and Robert Bruce Findler"
           #:location @elem{@italic{Journal of Functional Programming} @bold{22}}
           #:date "2012"
           #:url "http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=8573399"]
@bib-entry[#:key "YWIW"
           #:title "Composable and compilable macros: you want it when?"
           #:author "Matthew Flatt"
           #:location "International Conference on Functional Programming (ICFP)"
           #:date "2002"
           ;; #:pages "72--83"
           #:url "http://www.cs.utah.edu/plt/publications/macromod.pdf"]
]
