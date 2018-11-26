#lang frog/config
(require racket/match)

;; Called early when Frog launches. Use this to set parameters defined
;; in frog/params.
(define (init)
  (current-scheme/host "https://rmculpepper.github.io")
  (current-title "One Racketeer")
  (current-author "Ryan Culpepper")
  (current-index-full? #f))

;; Called once per post and non-post page, on the contents.
(define/contract (enhance-body xs)
  (-> (listof xexpr/c) (listof xexpr/c))
  ;; Here we pass the xexprs through a series of functions.
  (~> xs
      (downgrade-header-elements)
      (syntax-highlight #:python-executable "python"
                        #:line-numbers? #f
                        #:css-class "source")
      (auto-embed-tweets #:parents? #t)
      (add-racket-doc-links #:code? #t #:prose? #f)))

(define (downgrade-header-elements xs)
  (define (do-tag tag)
    (case tag
      [(h1) 'h2]
      [(h2) 'h3]
      [(h3) 'h4]
      [(h4) 'h5]
      [(h5 h6) 'h6]
      [else tag]))
  (define (do-node x)
    (match x
      [(list* (? symbol? tag) (? list? attrs) elems)
       (list* (do-tag tag) attrs (do-list elems))]
      [(list* (? symbol? tag) elems)
       (list* (do-tag tag) (do-list elems))]
      [_ x]))
  (define (do-list xs)
    (map do-node xs))
  (do-list xs))

;; Called from `raco frog --clean`.
(define (clean)
  (void))
