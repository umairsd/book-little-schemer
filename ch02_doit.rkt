#lang racket
(require test-engine/racket-tests)

(define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(define member? 
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
    




(define l '(Jack Sprat could eat no chicken fat))
(check-expect (lat? l) #t)
(check-expect (member? 'could l) #t)
(check-expect (member? 'Sprat l) #t)
(check-expect (member? 'c l) #f)

;(define lat '(mashed potatoes and meat gravy))
(check-expect (member? 'liver '()) #f)

(test)
