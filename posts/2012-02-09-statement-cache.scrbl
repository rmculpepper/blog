;; Date: 2012-02-09T00:00:00
;; Tags: racket, db
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-label racket db))

@title{unprepared queries vs statement caching}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

Racket's @racketmodname[db] library supports parameterized queries
where the SQL is given as a string (that is, @deftech{unprepared}):

@racketblock[
(query-list c "SELECT field1 FROM table WHERE field2 = ?" 17)
]

This is handled by an implicit call to @racket[prepare], which turns
the string into a prepared statement; the prepared statement is then
executed with the arguments.

The problem with this approach is that it involves two trips to the
server: one to prepare and one to execute. One trip would be
better. In this post I discuss two techniques for eliminating
extraneous server trips. I've recently added one of them to Racket;
the other turns out to be problematic.

@(the-jump)

@(begin
  (define pg-url
    "http://www.postgresql.org/docs/current/static/protocol-flow.html#PROTOCOL-FLOW-EXT-QUERY")
  (define my-url
    "http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol")
  (define (pgmsg text)
   (hyperlink pg-url (tt text)))
  (define (mymsg text)
   (hyperlink (string-append my-url "#" text) (tt text))))

The first approach, which I call ``unprepared query,'' is to avoid
preparing the statement altogether. PostgreSQL has a way of sending
both query string and arguments together in the same communication
without needing to hear back from the server. It's done by sending
@pgmsg{Parse}, @pgmsg{Bind}, and @pgmsg{Execute} messages in a
pipeline before sending @pgmsg{Sync}, which triggers a server
response. In MySQL it's only possible (if I remember correctly) if
there are no arguments, in which case one can use @mymsg{COM_QUERY}
instead of @mymsg{COM_PREPARE} and @mymsg{COM_EXECUTE}.

The main problem with unprepared queries for PostgreSQL is that Racket
no longer sees the statement's argument and result types. So if the
arguments are the wrong type, the server catches the error instead of
Racket. That's bad for two reasons: the error you get is different
from the error you get in the prepared case (and worse, IMHO), and if
you're in a transaction, it invalidates the transaction (all errors
invalidate a transaction in PostgreSQL). The @racketmodname[db]
library also needs the result types so it can choose between the text
and binary result encodings; for most types it speaks binary, but a
few (eg @tt{TIMESTAMP}, @tt{NUMERIC}) have complicated enough binary
forms that it's easier to just parse the text form. So unprepared
queries for PostgreSQL are out. For MySQL there's a similar issue:
@mymsg{COM_QUERY} message produces text results whereas the
@mymsg{COM_EXECUTE} message binary results. And again, it doesn't work
for queries with parameters.

For both systems it would be fine to use unprepared queries for
statements that neither take arguments nor return result rows. The
@racketmodname[db] library does just that for MySQL connections, but
for a different reason: only some types of statements can be
prepared. So unless it's a @tt{SELECT} or @tt{SHOW} statement (which
return rows) or it has parameters, a statement is executed
unprepared. But in PostgreSQL, every statement is preparable, and the
statements that are safe to execute unprepared are the ones that
aren't likely to be executed too often, like @tt{CREATE TABLE}.

The alternative, given that we usually want the prepared statement
information for better error behavior, is to cache implicit call to
@racket[prepare]. The first query still involves two trips, but
subsequent queries only take one. Only DML statements are cached:
@tt{SELECT}, @tt{INSERT}, etc.

Just one hitch---changes to the database schema may invalidate the
information associated with prepared statements. A connection inspects
each statement it executes to see if it is a potentially
schema-changing statement such as @tt{ALTER TABLE}; if so, it
invalidates the statement cache. We also invalidate the cache on
transactional statements; a @tt{ROLLBACK TO SAVEPOINT} can undo a
previous schema change in PostgreSQL.

But a schema change could also originate from another connection, so
by default we only enable the statement cache inside of a
transaction. The transaction isolates us from surprise
externally-originating schema changes (on PostgreSQL, anyway---getting
this right on MySQL is probably hopeless). The programmer should
probably be able to tune how careful the statement cache is, but I
haven't added that yet.
