;; Date: 2006-04-08T00:00:00
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket/base racket/class racket/stxparam))

@title{macros, parameters; binding, and reference}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(begin
  (define-syntax define-generator
    (make-element-id-transformer
     (lambda _ #'(racketkeywordfont "define-generator"))))
  (define-syntax define-dynamic-generator
    (make-element-id-transformer
     (lambda _ #'(racketkeywordfont "define-dynamic-generator"))))
  (define-syntax break
    (make-element-id-transformer (lambda _ #'(racketkeywordfont "yield"))))
  (define-syntax yield
    (make-element-id-transformer (lambda _ #'(racketkeywordfont "yield")))))

Danny Yoo had an interesting question on the plt-scheme mailing list
recently. At first it seemed like your standard non-hygienic, ``I want
this to mean something in here'' macro question. I used to group
macros into three levels of ``hygienicness'': the hygienic ones, the
ones that are morally hygienic in that the names they introduce are
based on their input, and the totally non-hygienic ones that have a
fixed name that they stick into the program. An example of the middle
set is @racket[define-struct], and an example of the third set is a
loop construct that binds the name @racket[break] in its body.

The third class used to offend me from a purist's (semi-purist?)
perspective. But it's a very reasonable thing to want to do. Consider
the @racket[class] macro and the names it uses to do interesting
things: @racket[super], @racket[public], @racket[field],
@racket[init], and so on. It depends on those particular names.

@(the-jump)

Danny Yoo was writing a generator library. He had a
@racket[define-generator] form, used like this:

@racketblock[
(define-generator (_name . _args) . _body)
]

and he wanted @racket[yield] to have a particular meaning inside of
the generator body. He had used the usual non-hygienic technique of
creating the right @racket[yield] identifier using
@racket[datum->syntax], but he was asking for other ideas.

Dave commented that non-hygienic macros typically do not play nicely
together; that is, it can be hard to write other macros that expand
into them, because you have to think about what version of the code
you want to bind the variable in, and you can't always tell... it's a
mess. Dave recommended creating two versions of the macro: a
non-hygienic front-end that forged the @racket[yield] identifier, and
a hygienic back-end that did the actual implementation. People who
wanted to further abstract over generator definitions could use the
hygienic version.

But there's another way to look at it, and that's what I replied to
the mailing list. It got me thinking about the similarities between
macros and normal programming, and the different techniques we use.

@emph{My answer begins here:}

Another way you can do this that avoids the problems Dave mentioned is
to use a ``syntax parameter.'' It's like a parameter, but for
macros. Here's how:

Consider a related problem: what if we wanted @racket[yield] to work
in the @emph{dynamic extent} of a call to a generator, rather than
just lexically in the generator's body? Here's how you'd do it:

@racketblock[
(define current-yielder
  (make-parameter
    (lambda (value) (error 'yield "not in a generator"))))
(define (yield value) ((current-yielder) value))
]

Then @racket[define-dynamic-generator] wraps the generator function
with something that sets the @racket[current-yielder] parameter.

@racketblock[
(define-syntax define-dynamic-generator
  (syntax-rules ()
    [(define-generator (name . args) . body)
     (define (name . args)
       (let ([name
              (lambda (yielder)
                (parameterize ([current-yielder yielder])
                  . body))])
         (make-generator name)))]))
]

(As an aside, this version passes your test suite, too.)

Using parameters, you can change an issue of @emph{binding} into an
issue of @emph{common reference} to a side-channel of communication
like a parameter or a box.

Syntax parameters work the same way. You agree on a keyword:

@racketblock[
(require racket/stxparam)

(define-syntax-parameter yield
  (lambda (stx)
    (raise-syntax-error #f "used outside of a generator" stx)))
]

Then you when you want some code to be able to use yield, like the
body of your generator function, you use @racket[syntax-parameterize]:

@racketblock[
(define-syntax define-generator
  (syntax-rules ()
    [(define-generator (name . args) . body)
     (define (name . args)
       (let ([name
              (lambda (yielder)
                (syntax-parameterize
                     ([yield
                       (syntax-rules ()
                         [(yield value)
                          (yielder value)])])
                  . body))])
         (make-generator name)))]))
]

This use of @racket[syntax-parameterize] changes the meaning of
@racket[yield] in the body to just call the supplied @racket[yielder]
function. If you want to be able to use @racket[yield] as an
expression by itself, rather than requiring it to be called, you can
use @racket[syntax-id-rules] to define an identifier macro instead.

By using @racket[syntax-parameterize], @racket[define-generator]
doesn't @emph{bind} @racket[yield]; it @emph{refers to}
@racket[yield]. Any macro that expands into a generator definition
will still be producing a reference to @racket[yield], and references
don't suffer the same interactions with hygiene that bindings do.
