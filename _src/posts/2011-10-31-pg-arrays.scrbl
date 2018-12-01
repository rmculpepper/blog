;; Date: 2011-10-31T00:00:00
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket db db/util/postgresql))

@title{in praise of PostgreSQL arrays}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@declare-keyword[]

I just added support for
@hyperlink["http://www.postgresql.org/docs/current/static/arrays.html"]{PostgreSQL
arrays} to the @racketmodname[db] library. While there are some uses
of arrays that are iffy from a database design standpoint, there's one
use that weighs overwhelmingly in their favor: avoiding dynamic
generation of SQL @tt{IN} comparisons.

@(the-jump)

Everyone knows not to ``parameterize'' SQL code by smashing strings
together, @hyperlink["http://xkcd.com/327/"]{right?}

That is, don't do this:

@racketblock[
(define (get-history name)
  (query-rows the-db 
    (format "SELECT url FROM fb.global_web_tracker WHERE name='~a'"
            name)))
]

Instead, do this:

@racketblock[
(define (get-history name)
  (query-rows the-db
    "SELECT url FROM fb.global_web_tracker WHERE name=$1"
    name))
]

But what if we want to search multiple names simultaneously?

If we know the names statically, we can use @tt{IN}:

@inset-block{
@verbatim{SELECT url FROM fb.global_web_tracker
          WHERE name IN ('Alice', 'Bob')}
}

And if the names aren't fixed and known in advance? Looks like a job
for string smashing ... (but don't!)

A clean, but awful, alternative is to create a temporary table, insert
each of the strings into it, perform the query with a sub-select or
join on the temporary table, then drop the temporary table. This
works, and it avoids the possibility of SQL injection. But isn't the
point of a declarative language (like SQL, ostensibly) to be able to
talk about information without having to first tell the system where
to put it?

In PostgreSQL, the array solution is clean and simple, although
@hyperlink["http://www.postgresql.org/docs/8.2/static/functions-comparisons.html#AEN14122"]{the
syntax} is a bit peculiar:

@inset-block{
@verbatim{SELECT url FROM fb.global_web_tracker
          WHERE name = ANY ($1)}
}

The parameter @tt{$1} here must by an array, probably with a type like
@tt{TEXT[]}, assuming @tt{name} is @tt{TEXT}. The parentheses around
@tt{$1} are significant.

Here's what the code looks like using the recently-updated
@racketmodname[db] library:

@racketblock[
(define (get-history names)
  (query-rows the-db
    "SELECT url FROM fb.global_web_tracker WHERE name = ANY ($1)"
    names))
(get-history (list "Alice" "Bob"))
]

In general, array values are actually represented with a dedicated
structure type, @racket[pg-array], that accommodates multi-dimensional
arrays, non-standard starting indexes, etc. But lists are
automatically converted, for the sake of simplicity.

Very preliminary research suggests that arrays might be mentioned in
one of the later SQL standards. MySQL and SQLite support them,
though. Oracle and DB2 seem to have some sort of support, but getting
to it through ODBC would be tricky, if it's even possible. So for now,
advantage PostgreSQL.
