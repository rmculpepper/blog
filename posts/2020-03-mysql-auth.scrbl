;; Date: 2020-03-21
;; Slug: mysql-mariadb-authentication-complaints
;; Display: draft
#lang scribble/manual
@(require scribble/example
          scribble/core
          scribble/html-properties
          racket/format
          "util.rkt"
          (for-label racket/base))

@(define mydoc (make-hyperlinker "https://dev.mysql.com/doc/refman/8.0/en/"))
@(define mardoc (make-hyperlinker "https://mariadb.com/kb/en/"))

@(define (caching_sha2_password)
   @mydoc["caching-sha2-pluggable-authentication.html"]{@tt{caching_sha2_password}})
@(define (ed25519) @mardoc["authentication-plugin-ed25519/"]{ed25519})

@title{authentication in MySQL and MariaDB}

Last summer I updated Racket's @racketmodname[db] library to support
@hyperlink["https://mysql.com"]{MySQL}'s new
@tt{caching_sha2_password} authentication method. While I was at it, I
looked at @hyperlink["https://mariadb.org/"]{MariaDB} authentication
and discovered that they also have a new authentication method,
@tt{ed25519}. I considered implementing it, but when I started looking
into the details, it seemed bad. Surprisingly bad. Maybe I
misunderstood something --- I decided to put it on the shelf and come
back to it later.

It's later now, and it still seems to be as bad as I originally
thought.

<!-- more -->

This post goes over the details of both MySQL's
@tt{caching_sha2_password} and MariaDB's @tt{ed25519} authentication,
and it analyzes the security of both protocols.


@;============================================================
@blogsection{Password-based Authentication}

The goal of password-based security mechanisms is to mitigate the
relatively low entropy of human-generated passwords. A uniform
distribution of passwords of length 8 from the 95 printable ASCII
characters has an entropy of 8 × log@subscript{2}(95) = @(~r
#:precision 2 (* 8 (/ (log 95) (log 2)))) --- but humans do not choose
passwords from uniform distributions. Even if they did, that number is
miniscule compared to a typical security level target of 128 bits.

A cryptographic system has ``@italic{N} bits of security'' ---
@hyperlink["https://wickr.com/the-bit-security-of-cryptographic-primitives-2/"]{very
roughly} --- if an attacker should expect to do about
2@superscript{@italic{N}} @emph{somethings} to break it in some way,
or perhaps do less work for a correspondingly lower probability of
success. The @emph{something} depends on the system and the type of
break desired; for example, SHA-256 has 256 bits of security against
pre-image attacks but 128 bits of security against collision
attacks. Good password-based cryptography makes each @emph{something}
costly enough that even typical password security levels are expensive
to attack.

Let's introduce two attackers: Brutus and, uh, Tabula.

Brutus has plenty of determination (that is, computational power) but
does no preparation. He does, however, have an algorithm for
enumerating all possible passwords. If your password appears
@emph{N}th in Brutus's enumeration, then its security level is about
log@subscript{2}(n) bits @emph{with respect to Brutus}.

Tabula, on the other hand, believes in preparation. She has a list of
a billion popular passwords, which may or may not be the same as the
first billion passwords on Brutus's list. Furthermore, Tabula is
willing to spend both time and storage to pre-compute various
functions of the passwords and storage to keep them around for fast
lookup. If your password is in her list, then it has at most 30 bits
of security with respect to Tabula.

@;{
Bad outcomes:
- attacker discovers the password
- attacker discovers a password-equivalent
}

I'll use Brutus and Tabula to illustrate attacks against the
authentication protocols.


@;============================================================
@blogsection{MySQL: @tt{caching_sha2_password} Authentication}

@;{
Other references:
- https://dev.mysql.com/worklog/task/?id=9271
}

MySQL has supported multiple
@mydoc["pluggable-authentication.html"]{authentication plugins} since
version 5.5. It added the @(caching_sha2_password) authentication
mechanism in version 8.0 and made it the default authentication
mechanism in 8.0.4.

The authentication mechanism has two variants: a ``fast path'' and a
``slow path''; which variant is used for a given connection attempt
depends on the state of the server. I'll focus on the fast path; more
details can be found in the
@hyperlink["https://dev.mysql.com/doc/dev/mysql-server/latest/page_caching_sha2_authentication_exchanges.html"]{MySQL Client/Server Protocol documentation}. See also
@hyperlink["https://mysqlserverteam.com/mysql-8-0-4-new-default-authentication-plugin-caching_sha2_password/"]{this overview from the MySQL Server team}.

A successful fast-path authentication looks like this:

@nested[#:style 'code-inset
@verbatim{
  Server → Client: nonce (20 bytes)
  Client → Server: scramble(nonce, password)
}]

The @tt{scramble} function is defined thus (@tt{⊕} means XOR, @tt{||}
means concatentation):

@nested[#:style 'code-inset
@verbatim{
scramble(nonce, password) =
  sha256(password) ⊕ sha256( sha256(sha256(password)) || nonce )
}]

The server stores @tt{sha256(sha256(password))}, and it generates a
fresh nonce each time. Verification looks like this:

@itemlist[

@item{The server knows @tt{sha256(sha256(password))} and @tt{nonce}, but it
does not know @tt{password} or @tt{sha256(password)}.}

@item{The server computes @tt{rhs = sha256( sha256(sha256(password)) || nonce )}.
It can do that, because it has both components.}

@item{When the server receives a response from the client, it
computes @tt{lhs = response ⊕ rhs}. If the client sent a
correct response, then @tt{lhs} @emph{should} be
@tt{sha256(password)}. The server does not know @tt{sha256(password)},
but it can compute @tt{sha256(lhs)} and check whether that matches
@tt{sha256(sha256(password))}, which it does know.}

]

In the slow path, if the connection is over a Unix domain socket or
over TCP with TLS, the client just sends the password to the
server. If the connection is over TCP without TLS, the client encrypts
the password with the server's RSA public key, which it had better
already know. (The client can request the server's public key, but the
server responds with the raw public key, not a certificate, so the
client should not trust its authenticity. IMHO, that part of the
protocol is pointless and dangerous.) Once the server has the
password, it verifies it by computing the
@hyperlink["https://www.akkadia.org/drepper/SHA-crypt.txt"]{crypt-sha256}
salted password hash. If the password is correct, it also computes
@tt{sha256(sha256(password))} and stores it in an in-memory cache so
future authentication attempts can use the fast path.

What can our attackers do with this protocol?

@bold{Attack: Eavesdrop on a fast-path authentication.} Suppose Brutus
and Tabula watch one successful fast-path authentication. That is,
they learn @tt{user} and a matching pair of @tt{nonce} and
@tt{scramble(nonce, password)} for that user's @tt{password}. Let's
assume the nonce hasn't been used before --- it shouldn't be; that's
the point of a nonce.

Brutus can guess check passwords at a cost of about 4 @tt{sha256}
calls per password. (I'm assuming the concatenation and XORing are
negligible.) Tabula, on the other hand, can pre-compute @tt{sha256(_)}
and @tt{sha256(sha256(_))} for each password in her list, and so the
online cost is only 1 additional @tt{sha256} operation per
password. (That last operation cannot be pre-computed or reused
because of the nonce.)

@(define speed:sha256-52/sec 4.5e6)

For comparison, @exec{openssl speed -evp sha256 -seconds 1} tells me
that on my desktop, on a single core, OpenSSL can do about 4.5 million
@tt{sha256} operations on 52-byte inputs (the longest input consists
of 32 bytes of @tt{sha256} output plus 20 bytes for the nonce,
assuming no unusually long passwords).

@bold{Attack: Eavesdrop on a slow-path authentication.} If the client
is using TLS and using it correctly (including verifying the server's
certificate), the attackers should get no usable information unless
they are able to break TLS in some way. If the client is using TCP
without TLS and uses an unauthenticated RSA key supplied by the
server, then the client is susceptible to a MITM (active) attack.

@bold{Attack: Steal the server's authentication cache from memory.}
Suppose our attackers steal the server's mapping of @tt{user} to
@tt{sha256(sha256(password))}. That would be catastrophic: Brutus
could guess passwords at a cost of only 2 @tt{sha256} operations per
guess; Tabula could just look in her pre-computed
@tt{sha256(sha256(_))} tables for matches. That's why MySQL doesn't
store it on disk; it keeps this mapping in memory as a cache and
populates it from successful slow-path authentications.

@bold{Attack: Steal MySQL's ``password file'' (table) from disk.}
MySQL does not persistently store a user's password or even the double-@tt{sha256}
hash of the password. Instead, it stores the result of
@hyperlink["https://www.akkadia.org/drepper/SHA-crypt.txt"]{crypt-sha256}, which is
@emph{designed to be slow}. The process is an instance of a general technique called
@hyperlink["https://en.wikipedia.org/wiki/Key_stretching"]{@emph{key stretching}},
and it's @hyperlink["https://codahale.com/how-to-safely-store-a-password/"]{absolutely
necessary for password storage}. (This particular algorithm was developed in
@hyperlink["https://akkadia.org/drepper/sha-crypt.html"]{2007}, long before
@hyperlink["https://password-hashing.net/#argon2"]{Argon2}, but not before
@hyperlink["https://www.usenix.org/legacy/events/usenix99/provos.html"]{bcrypt} (1999)
and @hyperlink["https://tools.ietf.org/html/rfc2898#section-5.2"]{PBKDF2} (2000).
The linked page explains why the developer didn't use bcrypt but
didn't explain why they didn't use PBKDF2.)
The @hyperlink["https://mysqlserverteam.com/mysql-8-0-4-new-default-authentication-plugin-caching_sha2_password/"]{MySQL
announcement} claims that they use 5000 rounds, which corresponds to just over 5000
@tt{sha256} operations.

Brutus can guess passwords at a cost of 5000 @tt{sha256} operations
per password @emph{per password file entry}. Each stored password is
processed with a different @emph{salt}, so work done guessing one
password entry can't be reused for another. These salts also mean that
Tabula has no advantage over Brutus. Much better. (But 5000 is
probably too low today, based on @hyperlink["https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html#pbkdf2"]{recommendations for PBKDF2}, an algorithm
with a comparable work factor.)

@bold{Summary:} The persistent password storage uses the right kind of
technique. It might not use the best algorithm, and the iteration
count might be too low, but at least it's not just keeping a simple
salted-hash password file like
@hyperlink["https://codahale.com/how-to-safely-store-a-password/"]{all those}
@hyperlink["https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html"]{password-hashing pages}
@hyperlink["https://www.tarsnap.com/scrypt.html"]{warn about}.
On the other hand, performing a fast-path authentication over TCP
without TLS is like leaking one line of one of those bad, awful,
insecure password files to anyone watching --- except it takes
2--4 hash calculations to check a guess instead of 1.

If you connect to MySQL over TCP, @emph{use TLS and verify the
server's certificate}.


@; ============================================================
@blogsection{MariaDB: @tt{ed25519} Authentication}

The page about MariaDB's @mardoc["authentication-plugin-ed25519/"]{ed25519}
authentication plugin says these two things:

@html:blockquote[#:cite "https://mariadb.com/kb/en/authentication-plugin-ed25519/"]{
The ed25519 authentication plugin uses Elliptic Curve Digital
Signature Algorithm (ECDSA) to securely store users' passwords and to
authenticate users.
}

That statement is false: the passwords are not stored securely.

@html:blockquote[#:cite "https://mariadb.com/kb/en/authentication-plugin-ed25519/"]{
The ed25519 algorithm is the same one that is used by OpenSSH.
}

That statement is highly misleading: SSH keys are generated by
cryptographically-secure pseudo-random number generators
(CSPRNGs). The security level of an Ed25519 SSH key is around 112 bits
(security level is not the same as key length). The quoted statement
is also technically false: MariaDB makes non-standard modifications to
the EdDSA algorithms.

Let's look at the details.

The @tt{ed25519} plugin uses a non-standard variant of
@hyperlink["https://tools.ietf.org/html/rfc8032"]{@tt{EdDSA}} where the signing
and public-key derivation algorithms
@hyperlink["https://github.com/ottok/mariadb-10.1/tree/master/plugin/auth_ed25519/README"]{treat
the password as the secret key}.
This ``works'' because the first step of both algorithms is to hash
the secret key with @tt{sha512}; the hash is then used to derive the
secret scalar and an ingredient to the signing algorithm's synthetic
IV. But cryptographic wisdom or foolishness aside, that means that the
authentication method cannot be independently implemented using the
standard EdDSA interfaces of cryptography libraries.

MariaDB stores the corresponding public key in its password table. A
successful authentication looks like this:

@nested[#:style 'code-inset
@verbatim{
  Server → Client: nonce (20 bytes)
  Client → Server: nonstd-Ed25519-Sign(password, nonce)
}]

The server uses the standard Ed25519 verification algorithm with the
user's stored public key to check whether the client's response is a
valid signature of @tt{nonce}.

Let's see what Brutus and Tabula can do with this authentication mechanism.

@bold{Attack: Eavesdrop on an authentication.} The attackers learn
@tt{user} and a matching pair of @tt{nonce} and
@tt{nonstd-Ed25519-Sign(password, nonce)}. Brutus has two options for
checking a password guess: @itemlist[

@item{Calculate @tt{nonstd-Ed25519-Sign(password-guess, nonce)} and see
if the signature matches the client's response. (Unlike some signature
algorithms, EdDSA is deterministic: signing the same message twice
with the same secret key produces the same signature.) The cost is
dominated by the curve arithmetic: signing requires a single
scalar--point multiplication.}

@item{Calculate @tt{pk = nonstd-Ed25519-PublicKey(password-guess)} and
then check if @tt{Ed25519-Verify(pk, response)} succeeds. Calculating
the public key takes 1 scalar--point multiplication, and verifying a
signature takes an additional two scalar--point multiplicatations and
a point addition.}

]
So calculating a signature and seeing if it equals the observed
signature seems like the cheaper option for Brutus.

The benchmark results from @exec{openssl speed -seconds 1 ed25519}
roughly agree with the relative cost analysis above: my desktop can do
about 37500 signatures per second and about 11500 verifications per
second, using a single core.

@;{(I conjecture that signature is even faster than expected because
it only uses multiplication of a scalar by the fixed base point, and
that operation might be specially optimized.)}

@(define speed:ed25519-sign/sec 37500)
@;{
LD_LIBRARY_PATH=/opt/openssl-1.1.1a/lib /opt/openssl-1.1.1a/bin/openssl speed -seconds 1 ed25519
Doing 253 bits sign   Ed25519's for 1s: 37719 253 bits Ed25519 signs  in 1.00s 
Doing 253 bits verify Ed25519's for 1s: 11481 253 bits Ed25519 verify in 1.00s
}

Can Tabula do better? Not significantly. For signing, the single
scalar--point multiplication depends on the nonce, so it cannot be
precomputed. For verification, she can precompute the public key,
saving one scalar--point multiplication, but verification is still
substantially slower than signing, so there's no point.

@bold{Attack: Steal MariaDB's ``password file'' (table) from disk.}
MariaDB stores the public key (produced by the nonstandard version of
@tt{Ed25519-PublicKey}) corresponding to the user's
@tt{password}.

Brutus can check a password guess at a cost of 1 scalar--point
multiplication per guess --- but since there is no salt, he can check
each guess against the entire password file for the same amount of
work.

Tabula can precompute the public key of each password on her
list. Each pre-computation is more expensive than a simple hash
function like SHA256 --- about two orders of magnitude slower. But if
she gets a password file she can simply scan it for known
passwords. And her precomputed list is reusable for any MariaDB
password file in the world.

On my desktop, @ex-link["2020/auth"]{this code} turns a million
passwords into @tt{ed25519} public keys in about 31 seconds. (That's
slightly slower than the openssl signing benchmark above.) For a rough
comparison, in about 32 seconds I can process the same million
passwords with PBKDF2-HMAC-SHA256 with a fixed salt and a work factor
@emph{60 iterations}. Keep in mind that
@hyperlink["https://tools.ietf.org/html/rfc2898#section-4.2"]{RFC 2898
(PKCS #5)} recommended a minimum of 1000 iterations in the year 2000.
The 2017 version of the
@hyperlink["https://pages.nist.gov/800-63-3/sp800-63b.html#memsecret"]{NIST
Digital Identity Guidelines (SP 800-63B)} recommends ``as large as
verification server performance will allow, typically at least 10,000
iterations'', but that number is probably on the low side. And of
course the salt should be random.

@bold{Summary:} MariaDB's @tt{ed25519} authentication is like a
cardboard door with a steel lock: it uses modern cryptographic
primitives but fails to mitigate the weakness of human-selected
passwords.

I'm not the first to
@hyperlink["https://security.stackexchange.com/questions/218046/how-does-mariadbs-ed25519-auth-scheme-work"]{notice
these problems}, and the designer of the authentication plugin
@hyperlink["https://mariadb.org/history-of-mysql-mariadb-authentication-protocols/"]{seems
aware of them but unconcerned}.


@; ============================================================
@blogsection{Conclusion}

Let's put the analysis in context.

When authentication is performed within a secure channel, even a
protocol like ``What is your password? --- Here it is.'' is secure
against eavesdropping-based attacks. Connections over TCP should use
TLS and use it correctly, including verifying the server
certificate. That will protect against eavesdropping as well as other
attacks I haven't discussed in detail, like MITM.

On the other hand, TLS won't protect against 



@blogsection{Other Comments}

The numbers 

@; ============================================================

@;{
Password lists:
- https://github.com/danielmiessler/SecLists
- ~1e9 https://labs.nettitude.com/tools/rocktastic/ (1 billion)
  - blog post: https://labs.nettitude.com/blog/rocktastic/

https://en.wikipedia.org/wiki/Alice_and_Bob
https://research.checkpoint.com/2019/cryptographic-attacks-a-guide-for-the-perplexed/

https://stackoverflow.com/questions/11298184/about-how-fast-can-you-brute-force-pbkdf2
https://www.usenix.org/system/files/conference/woot16/woot16-paper-ruddick.pdf - an attack on PBKDF2
https://eprint.iacr.org/2016/273.pdf - On the Weaknesses of PBKDF2
https://crypto.stackexchange.com/questions/18173/how-long-does-it-take-to-crack-pbkdf2

http://databaseblog.myname.nl/2018/02/how-cachingsha2password-leaks-passwords.html

https://www.nccgroup.trust/us/about-us/newsroom-and-events/blog/2007/july/enough-with-the-rainbow-tables-what-you-need-to-know-about-secure-password-schemes/ -- republished from elsewhere???
https://www.nccgroup.trust/us/about-us/newsroom-and-events/blog/2015/march/enough-with-the-salts-updates-on-secure-password-schemes/

Comment: timings from my desktop don't necessarily reflect GPU times,
etc. Also, trivially parallelizable.

TODO: taking PBKDF2 spec as baseline, calculate year for which each
auth mechanism is "adequate"
}
