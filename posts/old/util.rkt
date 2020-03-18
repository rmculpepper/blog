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

;; Reference and Guide links

(define Guide '(lib "scribblings/guide/guide.scrbl"))
(define Reference '(lib "scribblings/reference/reference.scrbl"))

(define (tech/guide #:key [key #f] . preflow)
  (apply tech #:doc Guide #:key key preflow))

(define (secref/guide . key)
  (apply secref #:doc Guide key))

(define (tech/reference #:key [key #f] . preflow)
  (apply tech #:doc Reference #:key key preflow))

(define (secref/reference . key)
  (apply secref #:doc Reference key))

(define (seclink/reference key . pre-content)
  (apply seclink #:doc Reference key pre-content))

(define (inset-block . preflow)
  (apply nested #:style "leftindent" preflow))

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
