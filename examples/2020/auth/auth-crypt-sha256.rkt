#lang racket/base
(require racket/match
         ffi/unsafe
         ffi/unsafe/define
         net/base64)

(define-ffi-definer define-c (ffi-lib "libcrypt"))

(define-c crypt
  (_fun (password : _bytes/nul-terminated)
        (salt : _bytes/nul-terminated)
        -> _bytes/nul-terminated))

(define (crypt-sha256 password salt)
  (crypt password (bytes-append #"$5$" salt)))

;; salt is up to 16 characters

;; ========================================
;; Main

;; Reads passwords one line at time from stdin, converts to public
;; key, and writes (base64-encoded) to stdout.

(module+ main
  (define salt
    (match (current-command-line-arguments)
      [(vector salt)
       (string->bytes/utf-8 salt)]
      [else (error "no salt given")]))

  (let loop ()
    (define passwd (read-bytes-line))
    (when (bytes? passwd)
      (define c (crypt-sha256 passwd salt))
      (write-bytes c)
      (write-bytes #"\n")
      (loop))))
