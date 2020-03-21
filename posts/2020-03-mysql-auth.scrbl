;; Date: 2020-03-21
;; Slug: mysql-mariadb-authentication-complaints
;; Display: draft
#lang scribble/manual
@(require scribble/example
          "util.rkt"
          (for-label racket/base))

@(define mydoc (make-hyperlinker "https://dev.mysql.com/doc/refman/8.0/en/"))
@(define mardoc (make-hyperlinker "https://mariadb.com/kb/en/"))

@title{some complaints about authentication in MySQL and MariaDB}

I have some complaints about the authentication mechanisms in
@hyperlink["https://mysql.com"]{MySQL} and
@hyperlink["https://mariadb.org/"]{MariaDB}.

<!-- more -->

Specifically, I have a small issue with MySQL's
@mydoc["caching-sha2-pluggable-authentication.html"]{@tt{caching_sha2_password}}
authentication plugin, and I have a big issue with MariaDB's
@mardoc["authentication-plugin-ed25519/"]{ed25519} plugin.

I've implemented most of the client-side @tt{caching_sha2_password}
mechanism for Racket's @racketmodname[db] library, which has a pure
Racket implementation of the MySQL wire protocol. I haven't
implemented the @tt{ed25519} mechanism at all.


@;============================================================
@blogsection{MySQL: @tt{caching_sha2_password} Authentication}

MySQL has supported multiple
@mydoc["pluggable-authentication.html"]{authentication plugins} since
version 5.5. It added the
@mydoc["caching-sha2-pluggable-authentication.html"]{@tt{caching_sha2_password}}
authentication mechanism in version 8.0 and made it the default
authentication mechanism in 8.0.4.

The authentication mechanism has two variants: a ``fast path'' and a
``slow path''; which variant is used for a given connection attempt
depends on the state of the server. I'll give a slightly simplified
summary of the two variants, but more details can be found in the
@hyperlink["https://dev.mysql.com/doc/dev/mysql-server/latest/page_caching_sha2_authentication_exchanges.html"]{MySQL Client/Server Protocol documentation}. See also
@hyperlink["https://mysqlserverteam.com/mysql-8-0-4-new-default-authentication-plugin-caching_sha2_password/"]{this overview from the MySQL Server team}.

The fast path occurs when ...

A successful authentication looks like this:

@nested[#:style 'code-inset
@verbatim{
  Server → Client: nonce (20 bytes)
  Client → Server: scramble(nonce, password)
  Server → Client: OK
}]

The @tt{scramble} function is defined thus (@tt{⊕} means XOR, @tt{||}
means concatentation):

@nested[#:style 'code-inset
@verbatim{
scramble(nonce, password) =
  sha256(password) ⊕ sha256( sha256(sha256(password)) || nonce )
}]

The server stores @tt{sha256(sha256(password))}, so it can use a fresh
nonce each time. Verification looks like this:

@itemlist[

@item{The server knows @tt{sha256(sha256(password))} and @tt{nonce}, but it
does not know @tt{password} or @tt{sha256(password)}.}

@item{The server computes @tt{rhs = sha256( sha256(sha256(password)) || nonce )}.
It can do that, because it has both components.}

@item{When the server receives a scramble response from the client, it
computes @tt{lhs = client-scramble ⊕ rhs}. If the client sent a
correct response, then @tt{lhs} @emph{should} be
@tt{sha256(password)}. The server does not know @tt{sha256(password)},
but it can compute @emph{sha256(lhs)} and check whether that matches
@tt{sha256(sha256(password))}, which it does know.}

]


@; ============================================================
@blogsection{MariaDB: @tt{ed25519} Authentication}



https://mariadb.com/kb/en/authentication-plugin-ed25519/
