#lang racket
(require rackunit)

;; Used to redefine Racket functions in terms of the original functions
(require (rename-in racket
                    [set? racket-set?]))
(provide (all-defined-out))


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


; Removes all occurrences of a given value from the list.
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))

;;------------------


; removes a member of a list, with the given equality check.
(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-f1 test? a (cdr l)))))))

(module+ test
  (check-equal? (rember-f1 = 5 '(6 2 5 3)) '(6 2 3))
  (check-equal? (rember-f1 eq? 'jelly '(jelly beans are good))
                '(beans are good))
  (check-equal? (rember-f1 equal?
                           '(pop corn)
                           '(lemonade (pop corn) and (cake)))
                '(lemonade and (cake))))


; When passed an argument `a`, returns a function.
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(module+ test
  (check-true ((eq?-c 'salad) 'salad))
  (check-false ((eq?-c 'salad) 'salado)))


; Use define to name the function returned by `eq-c?`.
(define eq-salad? (eq?-c 'salad))
(module+ test
  (check-true (eq-salad? 'salad))
  (check-false (eq-salad? 'salado)))



; Re-write rember-f as a function of one argument `test?`
; that returns an argument like rember with `eq?` replaced
; by test?
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a (cdr l))))))))

(module+ test
  (check-equal? ((rember-f =) 5 '(6 2 5 3)) '(6 2 3))
  (check-equal? ((rember-f eq?) 'jelly '(jelly beans are good))
                '(beans are good))
  (check-equal? ((rember-f equal?)
                 '(pop corn)
                 '(lemonade (pop corn) and (cake)))
                '(lemonade and (cake))))





