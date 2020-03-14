;; Date: 2011-10-16T00:00:00
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket/base racket/class racket/unit syntax/parse
                     racket/promise racket/runtime-path
                     macro-debugger/analysis/show-dependencies))

@title{lazy module loading}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(declare-keyword forever lazy-require hypothetical-lazy-require-macro)

The Racket module system is good at managing dependencies. When you
@racket[require] a module, you ensure that that module is initialized
before your code runs, and when the other module changes, the compiler
will notice and recompile your module too. Racket even stratifies
dependencies according to @seclink["stx-phases" #:doc Guide]{phase
levels} so you can use some modules in your macro implementations and
other modules in your run-time code and the expander/compiler/linker
knows
@hyperlink["http://www.cs.utah.edu/plt/publications/macromod.pdf"]{what
you want when}. It keeps track and makes sure that everything is
loaded and available when it's supposed to be.

But sometimes you want to manage dependencies yourself. This post
is about how to @emph{lazily load} the implementations of functions
and---with a bit of care---even macros.

@(the-jump) @; -- have to manually replace this in generated HTML

@blogsection{Why?}

One reason to lazily load a module is if it depends on foreign
libraries, for example, and you don't to try to load them if the user
doesn't actually use them. My @racketmodname[db] library does this;
there's no point complaining about a missing
@hyperlink["http://www.sqlite.org"]{SQLite} library if the user wants
to connect to a @hyperlink["http://www.postgresql.org"]{PostgreSQL}
server. Of course, it's possible to put the library loading inside a
@racket[lambda] or a @racket[unit], but you need to keep propagating
the delaying mechanism up until you either deal with it or dump it in
the user's lap.

Another reason is to select an implementation at run time. Racket's
GUI system works this way, for example. There are three
implementations (using system-specific foreign libraries) and
@racketmodname[racket/gui] picks the appropriate one at run time.

Yet another reason is to simply avoid loading code that is unlikely to
be used. When a module is loaded, all of its normal dependencies
(those introduced by @racket[require], whatever the phase) are also
loaded---including the code for any macros and their compile-time
support. To be clear, the modules are only @emph{instantiated} if
they're needed for execution or compilation, but their bytecode
is loaded into memory regardless.

Consider what that means for a language such as Typed Racket: loading
any typed program would also load the type-checker and optimizer---if
Typed Racket played by the normal rules. In fact, Typed Racket lazily
loads most of its compile-time components precisely so that compiled
typed programs won't depend on them. (Credit goes to
@hyperlink["http://www.ccs.neu.edu/home/samth"]{Sam TH} for showing me
the lazy-loading trick for macros. As I recall, I was initiallly
aghast at the idea until he explained the performance implications
of the normal loading strategy.)

@blogsection{Lazy Loading for Functions}

In principle, lazily loading a module is easy. Although Racket
presents a pleasant illusion of a static module graph, it's really all
built on top of dynamic pieces. The primary relevant tool is
@racket[dynamic-require]. Instead of using @racket[dynamic-require]
directly, though, let's define a @racket[lazy-require] form that
creates function proxy definitions that automatically call
@racket[dynamic-require] when necessary. Here's a first cut:

@racketblock[
(lazy-require
  [_mod (_function-id ...)])
==>
(begin (define _function-id
         (let ([p (delay (dynamic-require '_mod '_function-id))])
           (lambda args
             (apply (force p) args))))
       ...)
]

That's the basic idea. But there are several major problems with this
first cut:
@itemlist[

@item{@racket[dynamic-require] uses the @tech/reference{current
namespace}, which might not have the same @tech/reference{module
registry} the enclosing module was loaded in (see
also @secref/guide["mk-namespace"]).}

@item{If @racket[_mod] is a relative @tech/guide{module path},
@racket[dynamic-require] will try to resolve it relative to the
current directory rather than the location of the enclosing module.}

@item{It breaks @tt{raco exe}, since there's no indication to
the compiler that @racket[_mod] should be included in the executable.}
]

The solution to the first problem is to use
@racket[define-namespace-anchor] and
@racket[namespace-anchor->namespace]. The second and third problems
are both solved by using @racket[define-runtime-module-path-index].

@racketblock[
(lazy-require
  [_mod (_function-id ...)])
==> 
(begin (define-namespace-anchor anchor)
       (define-runtime-module-path-index mpi '_mod)
       (define _function-id
         (let ([p (delay
                   (parameterize ((current-namespace
                                   (namespace-anchor->namespace anchor)))
                     (dynamic-require mpi '_function-id)))])
           (lambda args
             (apply (force p) args))))
       ...)
]

There are still some little problems left. This version doesn't work
for functions with keyword arguments. Solution:
@racket[make-keyword-procedure] and @racket[keyword-apply]. And the
promise created by @racket[delay] is not reentrant; if multiple
threads try to force a function's loading simultaneously, it'll raise
an error. Solution: @racket[delay/sync].

It would also be nice if @racket[lazy-require] worked for values other
than functions. One way to make that work is to bind each name as an
identifier macro instead of creating a proxy function.


@blogsection{Lazy Loading for Macros}

So far, the @racket[lazy-require] macro only lets us access value
exports. Can we lazily require a macro?

The short answer is no.

There are two reasons why it won't work, one shallow and one deep. The
shallow reason is that @racket[dynamic-require] can only be used with
value exports. We'd need something else---let's call it
@racket[dynamic-require-syntax-local-value]. Let's pretend that
@racket[dynamic-require-syntax-local-value] would give us the
compile-time value of an export bound as syntax---for example, given a
macro name it would fetch the macro transformer. That would give us
just enough rope to hang ourselves, because the other reason it won't
work has to do with how Racket handles inter-module references---in
other words, linking.

Consider the following modules:

@racketmod[
#:file "private/run-forever.rkt"
racket
(define (run-forever thunk)
  (thunk)
  (run-forever thunk))
(provide run-forever)
]

@racketmod[
#:file "forever.rkt"
racket
(require "private/run-forever.rkt")
(define-syntax-rule (forever e)
  (run-forever (lambda () e)))
(provide forever)
]

@racketmod[
#:file "five-yo.rkt"
racket
(require "forever.rkt")
(forever
  (begin (read-line)
         (display "Why?\n")))
]

When the use of @racket[forever] in @racketmodname["five-yo.rkt"] is
expanded, it leaves behind a reference to @racket[run-forever]. (In
this context, I'll call that a @deftech{residual reference}, which
implies a @deftech{residual dependency} on the module defining it.)
How should that reference be represented in bytecode? One possibility
is ``the variable named @racket[run-forever] defined in
@racketmodname["/abs/path/to/private/run-forever.rkt"].''  But if we
hard-wired full filesystem paths into bytecode, we wouldn't be able to
compile it on one machine and install it on another, which would make
distributing Racket and Racket programs difficult. So instead,
the source of the reference is represented as a @tech/reference{module
path index}: a chain of relative module references usually ending in
@racket[(module-path-index-join #f #f)], which means ``the enclosing
module,'' roughly. The source of @racket[run-forever] is represented as

@racketblock[
(module-path-index-join "private/run-forever.rkt"
  (module-path-index-join "forever.rkt"
    (module-path-index-join #f #f)))
]

@;{ **** linking done at load-time or instantiate-time? **** }

When the compiled form of @racketmodname["five-yo.rkt"] is loaded,
it first loads its immediate dependencies
(@racketmodname["forever.rkt"]) and they load their immediate
dependencies (@racketmodname["private/run-forever.rkt"]) and so
on. Then it resolves the module path index for each reference's source
and links the reference to that actual module declaration. The
collection root and project root can change, as long as all relative
paths are preserved. (Otherwise, the Racket linker raises an error.)

What if we were to load @racketmodname["forever.rkt"] lazily?

@racketmod[
#:file "five-yo.rkt"
racket
(hypothetical-lazy-require-macro
  ["forever.rkt" (forever)])
(forever
  (begin (read-line)
         (display "Why?\n")))
]

By linking dynamically, we disrupt the chain of relative module paths;
@racket[dynamic-require] (and its hypothetical extension) have no tie
to the module currently being expanded---which, after all, may or may
not be the module that contains the @racket[dynamic-require]
call. Instead, the residual reference to @racket[run-forever] would
have a source that looks something like this:

@racketblock[
(module-path-index-join "private/run-forever.rkt"
  (make-resolved-module-path "/abs/path/to/forever.rkt"))
]

which Racket would refuse to write to a @bold{zo}-file.

So what @emph{does} work?

Follow this recipe to lazily load transformation dependencies:

@itemlist[
#:style 'ordered

@item{Group your code into @emph{interface}, @emph{transformation},
and @emph{residual} modules.}

@item{Add a ``bridge'' transformation module to get around
@racket[dynamic-require]'s phase limitations.}

@item{Use absolute module paths in all requires between the different
kinds of modules.}

@item{Change the interface-to-transformation requires to use
@racket[lazy-require], still with absolute module paths.}

]

I'll elaborate on each of those steps. Each step, if done correctly,
produces a working program/library; the final step just flips on lazy
loading of transformation code.

@bold{Group your code into @emph{interface}, @emph{transformation},
and @emph{residual} modules.} The interface modules are those required
by clients, and the residual modules are the ones that satisfy
residual dependencies in the code your macros produce. The
transformation modules are the ones that are only necessary during
transformation. The fact that they ``disappear'' from a compiled
module's dependencies is the win of the whole approach. If there's not
much in your transformation modules, then stop. Don't bother. It's not
worth it.

Here's our @racket[forever] macro reorganized. We'll go
bottom-up. Here's the single residual module:

@racketmod[
#:file "private/run-forever.rkt"
racket
(define (run-forever thunk)
  (thunk)
  (run-forever thunk))
(provide run-forever)
]

If we had previously defined @racket[run-forever] in the same module
as the @racket[forever] macro, we would need to separate them at this
stage.

There are many ways to separate out the transformation part, but the
easiest is to take the existing macro(s) and move them to new
private transformation module(s):

@racketmod[
#:file "private/trans-forever.rkt"
racket
(require "run-forever.rkt")
(define-syntax-rule (forever e)
  (run-forever (lambda () e)))
(provide forever)
]

Now we add an interface module:

@racketmod[
#:file "forever.rkt"
racket
(require (only-in "private/run-forever.rkt")
         (prefix-in t: "private/trans-forever.rkt"))
(define-syntax-rule (forever e)
  (t:forever e))
(provide forever)
]

The interface module(s) must require the residual module(s) directly
to make sure they get loaded, because the dependency through the
transformation modules will disappear when we get to the final
step. Since the interface module doesn't need any of its exports, we
use @racket[only-in] and ask for none of the bindings; that still
introduces a dependency. We also define a new @racket[forever] macro
that expands into the other one. We could have simply reprovided
@racket[forever] for now, but the indirection macro will become useful
later.

When separating your modules, don't confuse @emph{transformation} with
@emph{compile-time} and @emph{residual} with @emph{run-time}. They do
often coincide---in Typed Racket, for example, the type checker and
optimizer are compile-time and @emph{transformation}
components. However, Typed Racket also inserts code into a compiled
typed module, to be executed when the module is visited, to add its
bindings to a global type environment; thus the type environment code
is @emph{compile-time} but @emph{residual}.

@bold{Add a ``bridge'' transformation module} to get around
@racket[dynamic-require]'s phase limitations. Specifically,
@racket[dynamic-require] can only get @emph{value} bindings provided
from @emph{phase 0} of a module. So we add the following:

@racketmod[
#:file "private/bridge-forever.rkt"
racket
(require (for-template "trans-forever.rkt"))
(define (get-forever-id) #'forever)
(provide get-forever-id)
]

We need to provide some sort of handle for the @racket[forever] macro, 
but we need to provide it at phase 0---with the knowledge that we'll
be required at phase 1 (@racket[for-syntax]). For the phases to work
out, that means we must require the macro's module at phase -1
(@racket[for-template]). 

How, then, do we represent a handle for the macro? One possibility is
to just provide its transformer; but that would require
@racketmodname["private/trans-forever.rkt"] to have defined it as a
separate phase-1 function and provided it @racket[for-syntax]. The
alternative is to use an identifier that acts as a reference to the
macro.

We also change the interface module to use the additional identifier
indirection:

@racketmod[
#:file "forever.rkt"
racket
(require (only-in "private/run-forever.rkt")
         (for-syntax "private/bridge-forever.rkt"))
(define-syntax (forever stx)
  (syntax-case stx ()
    [(forever e)
     (with-syntax ([real-forever (get-forever-id)])
       #'(real-forever e))]))
(provide forever)
]

In other words, we lazily load the @emph{name} of the ``real'' macro,
relying on the fact that if we actually use the name, we've also
loaded the macro implementation itself (because
@racketmodname["bridge-forever.rkt"] has a strict dependency on
@racketmodname["trans-forever.rkt"]).

@bold{Use absolute module paths in all requires between the different
kinds of modules.} This is necessary to avoid unmarshallable module
path indexes later. Easiest is to put the code in a collection
library.

(If you're following along, just run @commandline{raco link --name
somelib .} in the directory with the interface module---now that
directory is a collection library directory! Remember to unlink it
when you're done---use the @tt{-r} flag.)

@racketmod[
#:file "private/run-forever.rkt"
racket
(code:comment "no changes")
(define (run-forever thunk)
  (thunk)
  (run-forever thunk))
(provide run-forever)
]

@racketmod[
#:file "private/trans-forever.rkt"
racket
(code:comment "abs. mod. path")
(require somelib/private/run-forever)
(define-syntax-rule (forever e)
  (run-forever (lambda () e)))
(provide forever)
]

@racketmod[
#:file "private/bridge-forever.rkt"
racket
(code:comment "no changes")
(require (for-template "trans-forever.rkt"))
(define (get-forever-id) #'forever)
(provide get-forever-id)
]

@racketmod[
#:file "forever.rkt"
racket
(code:comment "abs. mod. path")
(require (only-in somelib/private/run-forever)
         (for-syntax somelib/private/bridge-forever))
(define-syntax (forever stx)
  (syntax-case stx ()
    [(forever e)
     (with-syntax ([real-forever (get-forever-id)])
       #'(real-forever e))]))
(provide forever)
]

Note that @racketmodname["bridge-forever.rkt"] can @racket[require]
@racketmodname["trans-forever.rkt"] using a relative module path
because they're both transformation modules.

@bold{Change the interface-to-transformation requires to use
@racket[lazy-require], still with absolute module paths.} We'll need
to put the @racket[lazy-require] within a @racket[begin-for-syntax]
block.

@racketmod[
#:file "forever.rkt"
racket
(require (only-in somelib/private/run-forever)
         (for-syntax unstable/lazy-require))
(begin-for-syntax
 (lazy-require
  [somelib/private/bridge-forever 
   (get-forever-id)]))
(define-syntax (forever stx)
  (syntax-case stx ()
    [(forever e)
     (with-syntax ([real-forever (get-forever-id)])
       #'(real-forever e))]))
(provide forever)
]

That's it! Now the real implementation of @racket[forever] (that is,
@racketmodname["private/trans-forever.rkt"]) is only loaded when a use
of @racket[forever] needs to be expanded.

We can even test this:

@interaction[
(eval:alts
 (require macro-debugger/analysis/show-dependencies)
 (void))
(eval:alts
 (show-dependencies '(file "five-yo.rkt")
                    #:exclude '(racket))
 (for ([dep '(somelib/private/run-forever "forever.rkt")])
   (printf "~s\n" dep)))
]

That is, the only modules that @racketmodname["five-yo.rkt"] depends on
(not including @racketmodname[racket] and its dependencies) are
@racket[somelib/private/run-forever] and
@racketmodname["forever.rkt"].

The recipe above works for plain old macros, but not other kinds of
static bindings like @tech/reference{syntax parameters}, struct names,
or unit @tech/reference{signatures}. Those you'll just have to put in
the residual modules or not use lazy loading at all.


@blogsection{Is It Worthwhile?}

So, is lazy loading a good idea?

Occasionally. Lazy loading, aside from the case of avoiding foreign
library dependencies, is an optimization technique, and as such it's
good to have in a narrow band of cases. In particular, lazy loading is
useful for large blocks of code that are unlikely to be executed in a
typical run of a program (not just ``executed infrequently''). For
macros, the bar is higher because of the difficulty of separating
transformer code from residual code, and the danger of getting it
wrong. Lazy transformer loading is useful in cases like analyzers and
optimizers. The rest of the time, you're better off trusting Racket to
do the right thing.

Don't use lazy loading for code that can be split off into separate
modules just as easily. For example, @racketmodname[racket/contract]
is a moderately large library, but the core features are available
from @racket[racket/contract/base], which is a smaller library.
