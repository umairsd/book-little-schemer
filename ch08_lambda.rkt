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


; Re-write insertL as a function that takes a comparison function
; `test?` as an argument.
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) old)
         (cons new (cons old (cdr lat))))
        (else (cons (car lat)
                    ((insertL-f test?) new old (cdr lat))))))))

(module+ test
  (define l1 '(lamb chops and mint jelly))
    
  (check-equal? ((insertL-f eq?) 'xy 'jelly l1)
                '(lamb chops and mint xy jelly))

  (check-equal? ((insertL-f equal?) 'flavored 'jelly l1)
                '(lamb chops and mint flavored jelly))

  ; `x` is not inserted, as `eq?` doesn't work for list of S-expressions
  (check-equal? ((insertL-f eq?) 'x '(a b) '(a (a b) c d))
                '(a (a b) c d))
    
  (check-equal? ((insertL-f equal?) 'x '(a b) '(a (a b) c d))
                '(a x (a b) c d)))


; Re-write insertR as a function that takes a comparison function
; `test?` as an argument.
(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((test? (car lat) old)
         (cons old (cons new (cdr lat))))
        (else (cons (car lat)
                    ((insertR-f test?) new old (cdr lat))))))))
(module+ test
  (define l2 '(lamb chops and mint jelly))
    
  (check-equal? ((insertR-f eq?) 'xy 'jelly l2)
                '(lamb chops and mint jelly xy))

  (check-equal? ((insertR-f equal?) 'flavored 'jelly l2)
                '(lamb chops and mint jelly flavored))

  ; `x` is not inserted, as `eq?` doesn't work for list of S-expressions
  (check-equal? ((insertR-f eq?) 'x '(a b) '(a (a b) c d))
                '(a (a b) c d))
    
  (check-equal? ((insertR-f equal?) 'x '(a b) '(a (a b) c d))
                '(a (a b) x c d)))



(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))


; Inserts either at the left or the right.
(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
        ((null? lat) (quote ()))
        ((equal? (car lat) old) (seq new old (cdr lat)))
        (else (cons (car lat)
                    ((insert-g seq) new old (cdr lat))))))))

(module+ test
  (check-equal? ((insert-g seqR) 'x '(a b) '(a (a b) c d))
                '(a (a b) x c d))
  (check-equal? ((insert-g seqL) 'x '(a b) '(a (a b) c d))
                '(a x (a b) c d)))


; Define insertL in terms of insert-g.
(define insertL1
  (insert-g seqL))

(module+ test
  (check-equal? (insertL1 'x 'c '(a b c d))
                '(a b x c d)))


; Define insertR in terms of insert-g
(define insertR1
  (insert-g seqR))


; Define insertL in terms of insert-g, without passing in
; a function `seqL`.
(define insertL2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(module+ test
  (check-equal? (insertL2 'x 'c '(a b c d))
                '(a b x c d)))


#|
; Replaces the first occurrence of old with new.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old (cdr lat)))))))
|#

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst1
  (insert-g seqS))

(module+ test
  (define l3 '(lamb chops and mint jelly))
  (check-equal? (subst1 'pudding 'jelly l3) '(lamb chops and mint pudding))
  (check-equal? (subst1 'beef 'lamb l3) '(beef chops and mint jelly)))

  