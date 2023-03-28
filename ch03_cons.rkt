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

; My own implementation.
(define rember1
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember1 a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember a (cdr lat))))))))
                  


(define lat '(lamb chops and mint jelly))


(check-expect (rember 'mint lat) '(jelly))
(check-expect (rember1 'mint lat) '(lamb chops and jelly))

(test)
