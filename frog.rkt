#lang racket/base
(require racket/match
         frog/params
         frog/enhance-body
         threading)
(provide (all-defined-out))

;; Called early when Frog launches. Use this to set parameters defined
;; in frog/params.
(define (init)
  (current-scheme/host "https://rmculpepper.github.io/blog")
  (current-title "(blog-of ryanc@racket-lang.org)")
  (current-author "Ryan Culpepper")
  (current-index-full? #f))

;; Called once per post and non-post page, on the contents.
(define (enhance-body xs)
  ;; Here we pass the xexprs through a series of functions.
  (~> xs
      (syntax-highlight #:python-executable "python"
                        #:line-numbers? #f
                        #:css-class "source")
      (auto-embed-tweets #:parents? #t)
      (add-racket-doc-links #:code? #t #:prose? #f)))

;; Called from `raco frog --clean`.
(define (clean)
  (void))
