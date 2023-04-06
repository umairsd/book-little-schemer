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


(define myset?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (myset? (cdr lat))))))

(module+ test
  (check-true (myset? '(apples peaches plums)))
  (check-false (myset? '(apples apples peaches)))
  (check-true (myset? '()))
  (check-false (myset? '(apple 3 pear 4 9 apple 3 4))))


; Convert a lat into a set.
; The order of elements in the resulting set is in the order of their
; last occurrence in `lat`.
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (makeset (cdr lat))) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(module+ test
  (define lat '(apple peach pear peach plum apple lemon peach))
  (check-equal? (makeset lat) '(pear plum apple lemon peach))
  (check-equal? (makeset '(apple 3 pear 4 9 apple 3 4))
                '(pear 9 apple 3 4)))


; makeset using multirember
; The order of elements in the resulting set is in the order of their
; first occurrence in `lat`.
(define makeset1
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
                  (multirember (car lat) (makeset1 (cdr lat))))))))

(module+ test
  (define lat1 '(apple peach pear peach plum apple lemon peach))
  (check-equal? (makeset1 lat) '(apple peach pear plum lemon))
  (check-equal? (makeset1 '(apple 3 pear 4 9 apple 3 4))
                '(apple 3 pear 4 9)))



; Check is set1 is a subset of set2.
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(module+ test
  (define set1 '(5 chicken wings))
  (define set2 '(5 hamburgers
                   2 pieces fried chicken and
                   light duckling wings))
  (check-true (subset? set1 set2))
  (check-false (subset? set2 set1))
  (check-false (subset? '(4 pounds of horseradish)
                        '(four pounds chicken of horseradish))))


(define addtoset
  (lambda (a lat)
    (cond
      ((member? a lat) lat)
      (else (cons a lat)))))

(module+ test
  (check-equal? (addtoset 'a '(b c)) '(a b c))
  (check-equal? (addtoset 'a '()) '(a))
  )