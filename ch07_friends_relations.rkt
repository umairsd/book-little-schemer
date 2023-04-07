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


; Define subset? using and.
(define subset1?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset1? (cdr set1) set2))))))

(module+ test
  (define set1a '(5 chicken wings))
  (define set2a '(5 hamburgers
                    2 pieces fried chicken and
                    light duckling wings))
  (check-true (subset? set1a set2a))
  (check-false (subset? set2a set1a))
  (check-false (subset? '(4 pounds of horseradish)
                        '(four pounds chicken of horseradish))))


; Checks if two sets are equal.
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(module+ test
  (check-true (eqset? '(6 large chickens with wings)
                      '(6 chickens with large wings))))


; Checks if the interesection of two sets is non-empty.
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(module+ test
  (check-true (intersect? '(stewed tomatoes and macaroni)
                          '(macaroni and cheese)))
  (check-false (intersect? '(stewed tomatoes and macaroni)
                           '(cheese pizza))))
  

; Intresection of two sets.
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(module+ test
  (check-equal? (intersect '(stewed tomatoes and macaroni)
                           '(macaroni and cheese))
                '(and macaroni)))


; Union of two sets.
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(module+ test
  (check-equal? (union '(stewed tomatoes and macaroni casserole)
                       '(macaroni and cheese))
                '(stewed tomatoes casserole macaroni and cheese)))


; intersection of all sets in a list of sets, assuming that the list
; is not empty.
(define intersectall
  (lambda (l-set)
    (cond
      ; ((null? l-set) (quote ())) ;; Assumes that list is not empty.
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(module+ test
  (check-equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
                '(a))
  (check-equal? (intersectall '((6 pears and)
                                (3 peaches and 6 peppers)
                                (8 pears and 6 plums)
                                (and 6 prunes with some apples)))
                '(6 and)))


(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (car l)) #f)
      ((null? (cdr l)) #f)
      (else (null? (cdr (cdr l)))))))

(module+ test
  (check-true (a-pair? '(a b)))
  (check-true (a-pair? '(a (b))))
  (check-false (a-pair? '(a)))
  (check-false (a-pair? '(a b c))))


(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cdr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))


(define addtoset
  (lambda (a lat)
    (cond
      ((member? a lat) lat)
      (else (cons a lat)))))

(module+ test
  (check-equal? (addtoset 'a '(b c)) '(a b c))
  (check-equal? (addtoset 'a '()) '(a))
  )