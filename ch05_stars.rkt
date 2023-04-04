#lang racket
(require test-engine/racket-tests)

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

; Recursively remove a from all elements of the list `l`
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (rember* a (cdr l)))
                         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))


; Recursively add `new` to the left of every occurrence of `old`
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons (car l)
                (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))


(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))


(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new
                                  (cons (car l)
                                        (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))


; Checks if the given atom exists in the list.
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l)) (member* a (cdr l))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))


; Finds the leftmost atom in a non-empty list of S-expressions
; that does not contain the empty list.
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
    

; Checks if two lists are equal, using `eqan?`
(define eqlist1?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist1? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else (and (eqlist1? (car l1) (car l2))
                 (eqlist1? (cdr l1) (cdr l2)))))))


; Checks if two S-expressions are the same.
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))


; Re-write eqlist? in terms of equal?.
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))


; s is any S-expression, and l is a list of S-expressions.
; We need to ask 3-questions, per the First Commandment.
(define rember1
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((equal? (car l) s) (cdr l))
         (else (cons (car l)
                     (rember1 s (cdr l))))))
      (else (cond
              ((equal? (car l) s) (cdr l))
              (else (cons (car l) (rember1 s (cdr l)))))))))


; Simplified.
; s is any S-expression, and l is a list of S-expressions.
; We need to ask 3-questions, per the First Commandment.
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))



;; ------------------------------
;; Tests
(define l1 '((coffee) cup ((tea) cup) (and (hick)) cup))
(check-expect (rember* 'cup l1) '((coffee) ((tea)) (and (hick))))

(define l2 '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(check-expect (rember* 'sauce l2) '(((tomato)) ((bean)) (and ((flying)))))

(define l3 '((how much (wood))
             could
             ((a (wood) chuck))
             (((chuck)))
             (if (a) ((wood chuck)))
             could chuck wood))
(define expectedl3 '((how much (wood))
                     could
                     ((a (wood) chuck roast))
                     (((chuck roast)))
                     (if (a) ((wood chuck roast)))
                     could chuck roast wood))

(check-expect (insertR* 'roast 'chuck l3) expectedl3)

(check-expect (insertR* 'full 'cup l1)
              '((coffee) cup full ((tea) cup full) (and (hick)) cup full))

(check-expect (insertR* 'full 'cup '(coffee cup)) '(coffee cup full))

(check-expect (occur* 'wood l3) 4)
(check-expect (occur* 'could l3) 2)
(check-expect (occur* 'chuck l3) 4)
(check-expect (occur* 'cup l1) 3)


(define l4 '((banana)
             (split ((((banana ice)))
                     (cream (banana))
                     sherbet))
             (banana)
             (bread)
             (banana brandy)
             banana pudding))
(define expectedl4 '((orange)
                     (split ((((orange ice)))
                             (cream (orange))
                             sherbet))
                     (orange)
                     (bread)
                     (orange brandy)
                     orange pudding))
(check-expect (subst* 'orange 'banana l4) expectedl4)
(check-expect (subst* 'orange 'banana '(banana (banana pudding))) '(orange (orange pudding)))

(define expectedl4-1 '((yellow banana)
                       (split ((((yellow banana ice)))
                               (cream (yellow banana))
                               sherbet))
                       (yellow banana)
                       (bread)
                       (yellow banana brandy)
                       yellow banana pudding))
(check-expect (insertL* 'yellow 'banana l4) expectedl4-1)
(check-expect (insertL* 'yellow 'banana '(banana pudding))
              '(yellow banana pudding))

(check-expect (member* 'split l4) #t)
(check-expect (member* 'abc l4) #f)
(check-expect (member* 'banana l4) #t)

(check-expect (leftmost '((potato) (chips ((with) fish) (chips)))) 'potato)
(check-expect (leftmost '(((hot) (tuna (and))) cheese)) 'hot)
(check-error (leftmost '(((() four)) 17 (seventeen))))
(check-error (leftmost (quote ())))

(check-expect (eqlist1? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-expect (eqlist1? '(strawberry ice cream) '(strawberry cream ice)) #f)
(check-expect (eqlist1? '(banana ((split))) '((banana) (split))) #f)
(check-expect (eqlist1? '(beef ((sausage)) (and (soda)))
                        '(beef ((salami)) (and (soda)))) #f)

(check-expect (eqlist1? '(beef ((sausage)) (and (soda)))
                        '(beef ((sausage)) (and (soda)))) #t)


(check-expect (eqlist? '(strawberry ice cream) '(strawberry ice cream)) #t)
(check-expect (eqlist? '(strawberry ice cream) '(strawberry cream ice)) #f)
(check-expect (eqlist? '(banana ((split))) '((banana) (split))) #f)
(check-expect (eqlist? '(beef ((sausage)) (and (soda)))
                       '(beef ((salami)) (and (soda)))) #f)

(check-expect (eqlist? '(beef ((sausage)) (and (soda)))
                       '(beef ((sausage)) (and (soda)))) #t)



(define lat '(lamb chops and mint jelly))
(check-expect (rember 'mint lat) '(lamb chops and jelly))

(define l5 '((banana)
             (split ((((banana ice)))
                     (cream (banana))
                     sherbet))
             (banana)
             (bread)
             (banana brandy)
             banana pudding))

(define expectedl5-1 '((split ((((banana ice)))
                               (cream (banana))
                               sherbet))
                       (banana)
                       (bread)
                       (banana brandy)
                       banana pudding))

(check-expect (rember1 '(banana) l5) expectedl5-1)
(check-expect (rember '(banana) l5) expectedl5-1)

(test)