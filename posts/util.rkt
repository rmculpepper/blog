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

(define ((make-hyperlinker prefix) suffix . pre-flow)
  (apply hyperlink (string-append prefix suffix) pre-flow))

(define me-link (make-hyperlinker "/blog"))

(define ex-link-base
  #;"https://raw.githubusercontent.com/rmculpepper/blog/master/examples"
  "https://github.com/rmculpepper/blog/blob/master/examples")

(define ex-link (make-hyperlinker ex-link-base))

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

;; ============================================================
;; HTML

(define (html:blockquote #:cite [cite-href #f] . pre-flow)
  (define s (style #f (list (alt-tag "blockquote")
                            (attributes (if cite-href `((cite . ,cite-href)) '())))))
  (apply para #:style s pre-flow))

(define (html:footer . pre-flow)
  (define s (style #f (list (alt-tag "footer"))))
  (apply para #:style s pre-flow))
