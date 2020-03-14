;; Date: 2012-03-16T00:00:00
#lang scribble/manual
@(require scribble/eval
          scribble/racket
          "util.rkt"
          (for-syntax racket/base)
          (for-label racket
                     ;; A small lie: FFI's -> id has no docs, so use contract ->
                     ;; to avoid unsightly red underline.
                     (except-in ffi/unsafe ->)))

@title{avoid flushing your wire protocol's performance down the pipes}
@;@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

When implementing a wire protocol, one occasionally needs to know something
about the wires. This blog post is the story of how the placement of a call to
@racket[flush-output] caused a @emph{factor-of-20} variation in performance.

@(the-jump)

@nested[#:style 'inset]{
@emph{(If you're an experienced network programmer, everything I'm about to
say may be old hat, basic knowledge. It's new to me, though, so I thought I'd
share.)}
}

As I @hyperlink["/2012/02/unprepared-queries-vs-statement-caching.html"]{previously
mentioned}, the Racket @racketmodname[db] library always prepares statements before
executing them, even if the statement is supplied directly as a SQL string, and
even if no query parameters are given.

Prepared statements involve server-side resources. Normally, prepared statements
(on the server side) are closed when the corresponding prepared statement object
(on the Racket side) becomes unreachable; a finalizer issues the commands to
release the server-side resource. A busy program, however, can create prepared
statements far faster than the garbage collector can finalize them, so it makes
sense to add an optimization: a prepared statement created for the execution of
a SQL string is closed immediately after execution. (When the statement cache is
used, it's more complicated but fundamentally similar---a set of statements is
closed at once when the cache is flushed.)

This ``close-on-exec'' optimization works great for PostgreSQL, SQLite, and ODBC
connections, but it caused the execution of the test suite to slow down
dramatically---@emph{by a factor of roughly 20}---for MySQL
connections. ``Maybe closing prepared statements is just really slow in MySQL,'' I
thought. ``I'll look into it again right after I implement a few more Really
Cool Features, like nested transaction support.'' After all, the lack of
close-on-exec isn't an issue for short-lived connections---where ``short-lived''
means less than several thousand statements. It's not a problem if the
connection gets occasional breaks long enough for the garbage collector to catch
up. It's even okay if the same statement is executed (possibly with different
parameters) many times within a transaction, because of the statement cache. But
it's a problem for very busy connections that miss (or don't use) the statement
cache, and basic stress tests cause MySQL connections to quickly hit the
prepared statement limit and die.

It turns out that the problem with ``close-on-exec'' for MySQL connections is
due to a bad interaction between the structure of the MySQL protocol and TCP's
anti-congestion tricks.

Preparing a statement involves sending a message to the server and getting
several messages back: parameter and result descriptions, etc. Executing a
statement involves sending a message to the server and getting several messages
back: data rows and a status message. Closing a statement involves sending a
message to the server... and not getting any messages back.

A @emph{20x slowdown}, for this.

The problem is that the ``no-reply'' close message triggers
@hyperlink["http://developers.slashdot.org/comments.pl?sid=174457&cid=14515105"]{an
unfortunate interaction} between two features of TCP:
@hyperlink["http://en.wikipedia.org/wiki/Nagle%27s_algorithm"]{Nagle's
algorithm} and
@hyperlink["http://en.wikipedia.org/wiki/TCP_delayed_acknowledgment"]{delayed
ACKs}, two heuristic techniques for coalescing tiny packets into larger
packets. Executing a ``close-on-exec'' statement involves the following steps:
@itemlist[#:style 'ordered
@item{send a prepare message}
@item{receive some messages}
@item{send an execute message}
@item{receive some messages}
@item{send a close message}
]
When two ``close-on-exec'' statements are executed in quick succession, we get a
deadly @emph{write-write-read} sequence at the TCP level. Racket sends the close
message of the first cycle as one TCP packet; the server doesn't send any
messages back, so the ACK is delayed for some fraction of a second. Racket tries
to send the prepare message, but the TCP stack buffers it instead, waiting to
see if Racket wants to send any more data before the ACK for the previous packet
arrives---that's Nagle's algorithm. Now Racket is waiting on the server's reply
to a message that hasn't even been sent yet and won't be sent until the ACK for
the close message's packet finally arrives.

One solution, the one used by the MySQL client according to
@hyperlink["http://bugs.mysql.com/bug.php?id=5787"]{the bug discussion that
helped me figure out the problem}, is to just disable Nagle's algorithm by
setting the @tt{TCP_NODELAY} socket option. That's not too hard to do in Racket
using the FFI. Here's the code:

@racketblock[
(define IPPROTO_TCP 6)
(define TCP_NODELAY 1)

(define setsockopt_tcp_nodelay
  (get-ffi-obj "setsockopt" #f
    (_fun (socket enabled?) ::
          (socket : _int)
          (_int = IPPROTO_TCP)
          (_int = TCP_NODELAY)
          (enabled-ptr : (_ptr i _int) 
                       = (if enabled? 1 0))
          (_int = (compiler-sizeof 'int))
          -> (result : _int)
          -> (if (zero? result)
                 (void)
                 (error 'set-tcp-nodelay! "failed")))))

(define scheme_get_port_socket
  (get-ffi-obj "scheme_get_port_socket" #f
    (_fun (port) ::
          (port : _racket)
          (socket : (_ptr o _intptr))
          -> (result : _int)
          -> (and (positive? result) socket))))

(code:comment "set-tcp-nodelay! : tcp-port boolean -> void")
(define (set-tcp-nodelay! port enabled?)
  (let ([socket (scheme_get_port_socket port)])
    (setsockopt_tcp_nodelay socket enabled?)))
]

Calling @racket[set-tcp-nodelay!] on MySQL connections' TCP ports reduces the
real running time of the @racketmodname[db] test suite from 54 seconds to 2.8
seconds. Success!

But there's an even simpler solution that doesn't involve fussing with socket
options. There's no great urgency to the close statement message. So instead of
@emph{sending} it at the end of the query cycle, we just @emph{buffer} it
without flushing output. That way it gets sent at the start of the next query
cycle in the same packet as the prepare message. The prepare message causes the
server to send data back immediately, eliminating the ACK delay. This solution
performs just as well as the @tt{TCP_NODELAY} solution, and the changes to the
code are minimal.
