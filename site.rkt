#lang at-exp racket/base
(require racket/runtime-path
         racket/class
         racket/list
         racket/date
         jeremiah/config
         net/url)

(require jeremiah/templates/purecss/load)
;;(require jeremiah/templates/bootstrap4/load)

(define-runtime-path here ".")
(base-dir here)
(base-url (string->url "https://rmculpepper.github.io/blog"))

(site-author "Ryan Culpepper")
;;(site-title "(blog-of ryanc@racket-lang.org)")
(site-title "One Racketeer")

(tag-uri-entity "rmculpepper.github.io,2020")
(tag-uri-prefix "blog")

(extra-html
 (lambda (location page)
   (case location
     [(after-tagline)
      @list{<h2 class="site-alt-tagline">
              (blog-of ryanc@"@"racket-lang.org)
            </h2>}]
     [(end-content)
      (define copyright-year
        (or (and (send page is-page-type? 'post) (send page get-year))
            (let* ([posts (send (the-site) get-posts)]
                   [post-years (filter string? (map (lambda (p) (send p get-year)) posts))]
                   [now-year (number->string (date-year (current-date)))]
                   [max-year (argmax string->number (cons now-year post-years))]
                   [min-year (argmin string->number (cons now-year post-years))])
              (if (equal? min-year max-year) min-year (format "~a&ndash;~a" min-year max-year)))))
      @list{<div class="page-copyright">
              <div class="page-copyright-icon">
                <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
                  <img alt="Creative Commons License" style="border-width:0"
                       src="@(send (the-site) link)/img/cc4-by-sa-88x31.png" /></a>
              </div>
              <div class="page-copyright-text">
                Copyright @|copyright-year| Ryan Culpepper.
                This work is licensed under a
                <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
                Creative Commons Attribution-ShareAlike 4.0 International License</a>.
              </div>
            </div>}])))
