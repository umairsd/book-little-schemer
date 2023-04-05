#lang racket
(require rackunit)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Returns true if its two arguments are the same atom.
; Uses = for numbers, and eq? for all other atoms.
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))


;;------------------

;; Determines if the arithmetic expression aexp contains only numbers besides +, ×, and ↑
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ↑))
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      (else #f))))

(module+ test
  (check-true (numbered? '(4 + 4)))
  (check-true (numbered? 4))
  (check-true (numbered? '(4 x 4)))
  (check-true (numbered? '(4 x 1)))
  (check-true (numbered? '(4 ↑ 4)))
  (check-true (numbered? '(1 ↑ 5)))
  (check-false (numbered? '(4 a 4)))
  (check-false (numbered? '(4 b 4)))
  (check-false (numbered? '4b4))
  (check-false (numbered? '(4 4))))


; Gets the first subexpression of an arithmetic expression, of the form:
; (+ 4 3), or (+ (+ 4 1) (* 2 7))
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(module+ test
  (check-equal? (1st-sub-exp '(+ 4 3)) 4)
  (check-equal? (1st-sub-exp '(+ (+ 2 1) (x 4 5))) '(+ 2 1)))


; Gets the first subexpression of an arithmetic expression, of the form:
; (+ 4 3), or (+ (+ 4 1) (* 2 7))
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(module+ test
  (check-equal? (2nd-sub-exp '(+ 4 3)) 3)
  (check-equal? (2nd-sub-exp '(+ (+ 2 1) (x 4 5))) '(x 4 5)))


; Gets the first part of the arithmetic expression, which is the operator.
; The aexp is of the form (+ 4 3), (+ (x 4 5) 1), etc.
(define operator
  (lambda (aexp)
    (car aexp)))


; The value of an arithmetic expression.
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x))
       (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      (else (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(module+ test
  (check-eq? (value 4) 4)
  (check-eq? (value '(+ 4 3)) 7)
  (check-eq? (value '(+ 4 (+ 3 1))) 8)
  (check-eq? (value '(+ 4 (x 3 5))) 19)
  (check-eq? (value '(+ (x 3 5) 4)) 19)
  (check-eq? (value '(a 3 4)) 81)
  )



; Alternate respresentation of numbers.
; 0 = ()
; 1 = (())
; 2 = (() ())
; 3 = (() () ())

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

