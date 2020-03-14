#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket))

@title{Chaperones and Contracts for ADTs}
@author{Ryan}

Chaperones provide a nice mechanism for implementing 

@(the-jump)

I think a mutable stack is a better example. That was the example I used when I was talking with Vincent later that afternoon, and I think it illustrates the problems better.

  (struct stack ([elts #:mutable])

with operations

  is-empty? : stack -> boolean
  push! : stack any -> void
  pop! : stack -> any

Now we want a stack/c contract form such that when (stack/c elt-ctc) is applied to a stack S it produces S' such that

1) if push! is given a value not satisfying elt-ctc, a contract error occurs blaming the client

2) if pop! would return a value not satisfying elt-ctc, a contract error occurs blaming the provider

This example is just complicated enough to demonstrate why the naive approach of just applying the right chaperone/impersonator to the representation doesn't work.

What happens if we just apply the projection of

  (struct/c stack (listof elt-ctc))

to a stack? I can think of two problems: 1) it checks too much, and 2) it wraps too much.

Consider this example:

  (define s (new-stack))
  (push! s sub1)
  (define/contract s*
    (stack/c (-> exact-positive-integer? exact-positive-integer?))
    s)

  (push! s* add1)
  (pop! s*)
  ((pop! s) -12)  ;; failure A

  (push! s* add1)
  (for ([i 10]) (push! s* add1) (pop! s*))
  ((pop! s*) 0) ;; problem B

Failure A: when push! updates the *list* of elements, it applies the projection for (listof (-> pos-int? pos-int?)) to the whole list. So when an element is popped from the original, unrestricted stack, it has a projection applied to it even though it was not pushed! via s* and was not pop!ed via s*.

The second problem is more an implementation issue, but I think it's still worth noticing. Projections are idempotent in semantics but not in performance cost. The function applied at point B has had 22 projections applied to it. That's a time and space cost out of proportion to the number of times the value crossed the contract boundary (or at least that the programmer expected was the contract boundary).

And so it seems that chaperones and impersonators for ADTs *must* cooperate with the ADT's operations.

---

Option 1: bake contracts into structure/representation (ala data/*)
  BAD: not chaperone-of

Option 2: use chaperone-properties, conspire with operations
  GOOD: chaperone-of works
  BAD: uglifies operations

Option 3: "We are all generics now!": define ADT operations as (private!) generics
  GOOD: chaperone-of works
  OKAY: operations a bit odd, but not too bad
  BAD?: generic dispatch may be slower than predicate dispatch (Option 2)
        OTOH, (2) requires chaperone-property check, already kind of slow?
