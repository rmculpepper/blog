#lang racket/base
(require racket/class
         racket/match
         racket/draw
         pict)
(provide (all-defined-out))

;; ============================================================
;; Utils

(define (clip v vmin vmax)
  (cond [(< v vmin) vmin]
        [(> v vmax) vmax]
        [else v]))

(define (color->rgba c) ;; note: components are not pre-multiplied by alpha
  (values (send c red) (send c green) (send c blue) (send c alpha)))

(define (make-transparent) (make-object color% 0 0 0 0.0))

;; ============================================================
;; Colormaps

;; type UCoord = Real in [0,1]
;; type Coord = NNReal

;; type Colormap = (colormap PosReal PosReal (Coord Coord -> Color))
(struct colormap (width height get-color))

;; colormap-color : Colormap Coord Coord -> Color
(define (colormap-color cm x y)
  ((colormap-get-color cm) x y))

;; colormap-color-u : Colormap UCoord UCoord -> Color
(define (colormap-color-u cm ux uy)
  (match-define (colormap w h get-color) cm)
  (get-color (inexact->exact (floor (* ux w)))
             (inexact->exact (floor (* uy h)))))

;; bitmap->colormap : Bitmap -> Colormap
(define (bitmap->colormap b)
  (define bdc (new bitmap-dc% (bitmap b)))
  (define w (send b get-width))
  (define h (send b get-height))
  (define (get-color x y)
    (define c (new color%))
    (send bdc get-pixel (clip x 0 (sub1 w)) (clip y 0 (sub1 h)) c)
    c)
  (colormap w h get-color))

;; pict->colormap : Pict -> Colormap
(define (pict->colormap p)
  (bitmap->colormap (pict->bitmap p)))

;; map-colormap : Colormap (Color -> Color) -> Colormap
(define (map-colormap cm f)
  (define (get-color x y)
    (f (colormap-color cm x y)))
  (colormap (colormap-width cm) (colormap-height cm) get-color))

;; value-theshold-colormap : Colormap Real Real -> Colormap
(define (value-threshold-colormap cm vmin vmax)
  (define (f c)
    (define-values (r g b a) (color->rgba c))
    (define v (max r g b))
    (if (or (> v vmax) (< v vmin)) (make-transparent) c))
  (map-colormap cm f))

;; randomize-colormap : Colormap (Listof Color) -> Colormap
(define (randomize-colormap cm colors)
  (define colorv (list->vector colors))
  (define (random-color) (vector-ref colorv (random (vector-length colorv))))
  (define (f c)
    (define-values (r* b* g* a*) (color->rgba (random-color)))
    (make-object color% r* b* g* (* (send c alpha) a*)))
  (map-colormap cm f))

;; ============================================================
;; Sampling points

;; type UPoint = (cons UCoord UCoord)

;; By default, points are drawn in order returned.

;; sample-uniform-points : Nat -> (Listof UPoint)
(define (sample-uniform-upoints n)
  (for/list ([_i (in-range n)]) (cons (random) (random))))

;; sample-stratified-upoints : Nat -> (Listof UPoint)
(define (sample-stratified-upoints nrows ncols)
  ;; Divides cm into nrows rows and ncols columns, samples one point
  ;; from each point in the resulting grid.
  (for*/list ([row (in-range nrows)] [col (in-range ncols)])
    (cons (/ (+ row (random)) nrows) (/ (+ col (random)) ncols))))

;; ============================================================
;; Sampling fishes

;; fish-scale : (Parameterof PosReal)
(define fish-scale (make-parameter 1))

;; fish-dir : (Parameterof (U 'left 'right))
(define fish-dir (make-parameter 'left))

;; scale-fish : Pict -> Pict
(define (scale-fish p) (scale p (fish-scale)))

;; random-fish : Color -> Pict
(define (random-fish c)
  (define-values (w h)
    (case (random 4)
      [(0) (values 20 15)]
      [(1) (values 40 30)]
      [(2) (values 25 25)]
      [(3) (values 25 20)]))
  (let-values ([(r g b a) (color->rgba c)])
    (define p (standard-fish w h #:color c #:eye-color "gray" #:direction (fish-dir)))
    (scale-fish (if (< a 1) (cellophane p a) p))))

;; ============================================================

(define (fish-school cm upoints)
  (define w (colormap-width cm))
  (define h (colormap-height cm))
  (for/fold ([base (blank w h)]) ([upoint (in-list upoints)])
    (match-define (cons ux uy) upoint)
    (define p (random-fish (colormap-color-u cm ux uy)))
    (define pw (pict-width p))
    (define ph (pict-height p))
    (pin-over base (- (* w ux) (/ pw 2)) (- (* h uy) (/ ph 2)) p)))

(define (pict->fish-school p upoints)
  (define cm (pict->colormap p))
  (fish-school cm upoints))

(define (racket-logo-fish-school)
  (parameterize ((fish-scale 1/5))
    (define b (read-bitmap (collection-file-path "drracket.png" "drracket")))
    (define cm (pict->colormap (inset (bitmap b) 10)))
    (scale (fish-school cm (sample-stratified-upoints 20 20)) 5)))
