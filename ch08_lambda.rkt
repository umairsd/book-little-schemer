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


; Define rember using `insert-g`.
(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(module+ test
  (check-equal? (rember 'mint '(lamb chops and mint jelly))
                '(lamb chops and jelly)))



#|
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
      ((eq? (operator nexp) (quote +)) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x)) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
      (else (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(module+ test
  (check-eq? (value 4) 4)
  (check-eq? (value '(+ 4 3)) 7)
  (check-eq? (value '(+ 4 (+ 3 1))) 8)
  (check-eq? (value '(+ 4 (x 3 5))) 19)
  (check-eq? (value '(+ (x 3 5) 4)) 19)
  (check-eq? (value '(a 3 4)) 81)
  )
|#


; copied from chapter 6.
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

; copied from chapter 6.
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

; copied from chapter 6.
(define operator
  (lambda (aexp)
    (car aexp)))


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) +)
      ((eq? x (quote *)) *)
      (else expt))))


; Re-write `value` by abstracting the common functionality into
; a separate function.
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))


(module+ test
  (check-eq? (value 4) 4)
  (check-eq? (value '(+ 4 3)) 7)
  (check-eq? (value '(+ 4 (+ 3 1))) 8)
  (check-eq? (value '(+ 4 (* 3 5))) 19)
  (check-eq? (value '(+ (* 3 5) 4)) 19)
  (check-eq? (value '(a 3 4)) 81))




#|
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat)
                          (multirember a (cdr lat)))))))))
|#


(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))

(module+ test
  (define ls0 '(lamb and chops and gravy and mint and jelly))
  (check-equal? ((multirember-f eq?) 'and ls0)
                '(lamb chops gravy mint jelly))
  ; eq? won't work for nested s-expressions.
  (check-equal? ((multirember-f eq?) '(c d) '(a b c (c d) (d (e f)) (c d)))
                '(a b c (c d) (d (e f)) (c d)))
  (check-equal? ((multirember-f equal?) '(c d) '(a b c (c d) (d (e f)) (c d)))
                '(a b c (d (e f)))))



; multiremeber such that it takes a function as a test without
; a specific value to compare against.
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote ()))
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat)
                  (multiremberT test? (cdr lat)))))))

(module+ test
  (define ls1 '(lamb and chops and gravy and mint and jelly))  
  (check-equal? (multiremberT (lambda (x) (eq? x 'and))
                              ls1)
                '(lamb chops gravy mint jelly))
  ; eq? won't work for nested s-expressions.
  (define ls2 '(a b c (c d) (d (e f)) (c d)))
  (check-equal? (multiremberT (lambda (x) (eq? x '(c d)))
                              ls2)
                ls2)
  
  (check-equal? (multiremberT (lambda (x) (equal? x '(c d)))
                              ls2)
                '(a b c (d (e f)))))


; It looks at every atom of the `lat` to see whether it is `eq?`
; to `a`. Those atoms that are not, are collected in one list `ls1`.
; the others for which the answer is true are collected in a second
; list `ls2`. Finally it determines the value of `f ls1 ls2`
(define multirember&co
  (lambda (a lat col)
    (cond
      ; (1) Base case. Collect empty lists.
      ((null? lat) (col (quote ()) (quote ())))
      ; (2) The head of the list is equal to `a`. Collect it in `seen`.
      ((eq? a (car lat))
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen)))))
      ; (3) The head of the list is not equal to `a`. Collect it in `newlat`.
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat) seen)))))))

(module+ test
  (check-equal? (multirember&co 'tuna '(strawberries tuna and swordfish) list)
                '((strawberries and swordfish) (tuna)))
  (check-equal? (multirember&co 'b '(a b c b d e b) list)
                '((a c d e) (b b b)))
  )


(define a-friend
  (lambda (x y)
    (null? y)))

(module+ test
  (check-true (multirember&co 'tuna '() a-friend))
  (check-false (multirember&co 'tuna '(tuna) a-friend))
  (check-false (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend))
  (check-true (multirember&co 'tuna '(strawberries and swordfish) a-friend))
  )