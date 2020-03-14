#lang racket/base
(require racket/runtime-path
         jeremiah/config
         net/url)

(require jeremiah/templates/purecss/load)
;;(require jeremiah/templates/bootstrap4/load)

(define-runtime-path here ".")
(base-dir here)
;;(base-url (string->url "https://rmculpepper.github.io/blog/"))
(base-url (string->url "https://rmculpepper.github.io/"))

(site-author "Ryan Culpepper")
;;(site-title "(blog-of ryanc@racket-lang.org)")
(site-title "One Racketeer")

(tag-uri-entity "rmculpepper.github.io,2020")
(tag-uri-prefix "blog")
