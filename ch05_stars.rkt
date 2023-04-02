#lang racket
(require test-engine/racket-tests)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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


(test)