#lang racket
(require rackunit)

;; Previously defined functions.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member? 
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

;;------------------





(define pick
  (lambda (n lat)
    (cond
      ((null? lat) #f)
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(module+ test
  (check-equal? (pick 3 '(a b c d e)) 'c)
  (check-equal? (pick 1 '(a b c d e)) 'a)
  (check-equal? (pick 5 '(a b c d e)) 'e))



; My version.
(define keep-looking1
  (lambda (a n lat)
    (cond
      ((null? lat) #f)
      ((eq? a n) #t)
      ((number? n) (keep-looking1 a (pick n lat) lat))
      (else #f))))

(module+ test
  (check-true (keep-looking1 'caviar 6 '(6 2 4 caviar 5 7 3)))
  (check-false (keep-looking1 'caviar 6 '(6 2 grits caviar 5 7 3)))
  )


; sorn means "symbol or number"
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((null? lat) #f)
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(module+ test
  (check-true (keep-looking 'caviar 6 '(6 2 4 caviar 5 7 3)))
  (check-false (keep-looking 'caviar 6 '(6 2 grits caviar 5 7 3))))



(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(module+ test
  (check-true (looking 'caviar '(6 2 4 caviar 5 7 3)))
  (check-false (looking 'caviar '(6 2 grits caviar 5 7 3))))











