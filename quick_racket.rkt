#lang slideshow
;; https://docs.racket-lang.org/quick/

(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
  (filled-rectangle n n))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

; let allows us to bind multiple values.
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

; Equivalent code using `define`.
(define (checker1 p1 p2)
  (define p12 (hc-append p1 p2))
  (define p21 (hc-append p2 p1))
  (vc-append p12 p21))
  

(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))

(checker1 (colorize (square 10) "red")
          (colorize (square 10) "black"))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(checkerboard (square 10))
(checkerboard (circle 10))


;; 6: Functions are Values
"6: Functions are Values"

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))


(series circle)
(series square)

(series (lambda (size) (checkerboard (square size))))

;; 7: Lexical Scope
"7: Lexical Scope"

; Here, `mk` refers to the argument of `rgb-series`.
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(rgb-series circle)
(rgb-series square)


;; 8: Lists
"8: Lists"

(list "red" "green" "blue")
(list 'a 'b 'c)

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 4))

(apply vc-append (rainbow (square 5)))


; 11: Objects
"11: Objects"

(require racket/class
         racket/gui/base)

(define f (new frame% [label "My Art"]
               [width 300]
               [height 300]
               [alignment '(center center)]))

(send f show #t)


