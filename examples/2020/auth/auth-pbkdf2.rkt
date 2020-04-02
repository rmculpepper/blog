#lang racket/base
(require racket/match
         crypto
         crypto/libcrypto
         net/base64)

(define kdfi (get-kdf '(pbkdf2 hmac sha256) libcrypto-factory))

(define (pbkdf2-hmac-sha256 password salt iters)
  (define r (kdf kdfi password salt `((iterations ,iters) (key-size 32))))
  (regexp-replace #rx#"[=]+$" (base64-encode r #"") #""))

;; salt is up to 16 characters

;; ========================================
;; Main

;; Reads passwords one line at time from stdin, converts to public
;; key, and writes (base64-encoded) to stdout.

(module+ main
  (define-values (iters salt)
    (match (current-command-line-arguments)
      [(vector (app string->number (? exact-positive-integer? iters)) salt)
       (values iters (string->bytes/utf-8 salt))]
      [else (error "expected iters, salt")]))
  (let loop ()
    (define passwd (read-bytes-line))
    (when (bytes? passwd)
      (define c (pbkdf2-hmac-sha256 passwd salt iters))
      (write-bytes c)
      (write-bytes #"\n")
      (loop))))
