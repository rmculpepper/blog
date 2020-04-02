#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         net/base64)
(provide (all-defined-out))

;; This needs the "client_ed25519.so" shared library from the
;; libmariadb3 client package.

(define-ffi-definer define-auth (ffi-lib "client_ed25519.so"))

(define-auth crypto_sign_keypair
  (_fun (passwd [pk (make-bytes 32)]) ::
        (pk : _pointer)
        (passwd : _pointer)
        (_ullong = (bytes-length passwd))
        -> (r : _int) -> (and (zero? r) pk)))

(define-auth crypto_sign
  (_fun (passwd msg [signed (make-bytes 32)]) ::
        (signed : _pointer)
        (msg : _pointer) (_ullong = (bytes-length msg))
        (passwd : _pointer) (_ullong = (bytes-length passwd))
        -> (r : _int) -> (and (zero? r) signed)))

(define (password->pk passwd [scratch (make-bytes 32)])
  (define r (base64-encode (crypto_sign_keypair passwd scratch) #""))
  (regexp-replace #rx#"[=]+$" r #""))

;; ----------------------------------------
;; Main

;; Reads passwords one line at time from stdin, converts to public
;; key, and writes (base64-encoded) to stdout.

(module+ main
  (define scratch (make-bytes 32))
  (let loop ()
    (define passwd (read-bytes-line))
    (when (bytes? passwd)
      (define pk (password->pk passwd scratch))
      (write-bytes pk)
      (write-bytes #"\n")
      (loop))))
