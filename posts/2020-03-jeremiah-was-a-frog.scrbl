;; Date: 2020-03-17
;; Tags: racket
#lang scribble/manual
@title{Jeremiah was a Frog}
@(require (for-label racket/base racket/runtime-path web-server/templates))

@(define (Frog) @hyperlink["https://github.com/greghendershott/frog"]{Frog})

I'm rebooting my blog, previously
@hyperlink["https://macrologist.blogspot.com/"]{hosted on Blogspot}. I finished
a conversion to @(Frog) some time ago --- there weren't that many posts to
convert, and I was already using Scribble and my now-defunct tool
@hyperlink["https://blog.racket-lang.org/2012/04/scribble-your-blogs.html"]{Scriblogify}.
But there was just one thing I wanted to fix about Frog's Scribble rendering
first. And then there was another thing, and so on, and in the end I wound up
with something different enough to give it a new name.

<!-- more -->

@hyperlink["https://github.com/rmculpepper/racket-jeremiah"]{Jeremiah} is a
static blog generator that evolved out of a heavily modified version of
@(Frog). During this evolution, the code lost several of the virtues of the
original, such as tests, documentation, and features. It's not
finished. It's not compatible with Frog. It's not stable --- there are still
frequent breaking changes.

Here are some of the differences from Frog:
@itemlist[

@item{Scribble metadata is written in comments before the @litchar{#lang}
line. The title can also be extracted from the page content. For example,
the metadata from this post is declared as follows:

@codeblock|{
;; Date: 2020-03-17
;; Tags: racket
#lang scribble/manual
@title{Jeremiah was a Frog}
....
}|}

@item{If Frog's attitude is ``here, let me get that for you'', Jeremiah's
attitude is ``that looks like something you can handle yourself''. Here
are a few examples of this philosophy:

@itemlist[

@item{Jeremiah provides hooks for producing HTML, but you must give it
functions, not template files. You're welcome to implement those functions
using @racket[include-template], of course, but Jeremiah doesn't want to
know. (Actually, Jeremiah comes with a default template that uses
@racket[include-template] and provides sub-hooks to make HTML generation
easier.)}

@item{A site configuration file is just an ordinary Racket module that
gets @racket[dynamic-require]d for side-effect at the beginning of
processing. You must require @racketmodname[jeremiah/config] and set
the necessary parameters. Jeremiah currently doesn't even set the
@racket[current-directory] to the configuration file's directory,
because you can use @racket[define-runtime-path] to get absolute paths
from module-relative paths.}

@item{The command-line interface doesn't have a @exec{--verbose} flag;
instead, Jeremiah logs to the @tt{jeremiah} topic; the @tt{info} level
corresponds to verbose output, and @tt{debug} is very verbose.}

]}

@item{Jeremiah uses a different directory layout. Static sources are copied
to the output directory on each build, so cleaning up can be done with
@tt{rm -rf}. The layout is not currently configurable.}

@item{Jeremiah currently writes each post to its own directory. This
makes it slightly easier to handle Scribble posts that generate
auxiliary files like images.}

@item{Jeremiah uses Racket classes and objects pervasively in its
implementation and in the interface it exposes for HTML generation. For
example, the main page generator, instead of getting an environment with
lots of variables, gets the following: @tt{Page} object (which might be a
@tt{Post} or an @tt{IndexPage}) and a @tt{Site} object (and the latter is a
parameter, not an argument). From these objects you can extract titles,
dates, various links, HTML content, etc.}

@item{Jeremiah currently has none of Frog's widgets and body
enhancers. I'll probably adapt them case-by-case as I need them.}

@item{Jeremiah generates only Atom feeds (no RSS), and it uses the
@hyperlink["https://taguri.org/"]{tag URI scheme} for generating feed
and post identifiers.}

]

Despite the changes, Jeremiah owes a lot to Frog. It's easier to make
changes --- even significant changes --- to a design and
implementation that works than to create one from scratch. Jeremiah
also keeps parts of Frog's structure and some of the trickier
implementation bits with minimal changes. So many thanks to Greg
Hendershott and the other contributors to Frog!

@(require "private/fish.rkt")
@centered[
(parameterize ((current-pseudo-random-generator
                (make-pseudo-random-generator)))
  (random-seed 2020)
  (with-handlers (#;[exn:fail? (lambda _ @elem{"logo failed"})])
    (racket-logo-fish-school)))
@para{Joy to the fishes!}
]
