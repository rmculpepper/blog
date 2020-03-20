#lang racket/base
(require scribble/manual
         scribble/racket
         scribble/core
         scribble/html-properties
         scribble/decode
         (for-syntax racket/base))
(provide (all-defined-out))

(define (blogsection . preflow)
  (apply section #:style 'unnumbered preflow))

(define (the-jump) "<!-- more -->")

;; Intra-blog links

(define (me-link url . pre-flow)
  (apply hyperlink (string-append "/blog" url) pre-flow))

(define ex-link-base
  "https://raw.githubusercontent.com/rmculpepper/blog/master/examples")

(define (ex-link suffix . pre-flow)
  (apply hyperlink (format "~a/~a" ex-link-base suffix) pre-flow))

;; Reference and Guide links

;; Formatting

(define-syntax-rule (define-declare-X declare-X formatter)
  (... (define-syntax-rule (declare-X id ...)
         (begin (define-syntax id
                  (make-element-id-transformer
                   (lambda _ #'(formatter (symbol->string 'id)))))
                ...))))

(define-declare-X declare-keyword racketkeywordfont)

;; Symbols

(define-syntax ==>
  (make-element-id-transformer (lambda _ #'(elem "⇒"))))
