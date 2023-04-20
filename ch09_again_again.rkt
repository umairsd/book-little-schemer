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

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (car l)) #f)
      ((null? (cdr l)) #f)
      (else (null? (cdr (cdr l)))))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

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


; The function shift takes a pair whose first component
; is a pair and builds a pair by shifting the second part
; of the first component into the second component.
(define shift
  (lambda (fp)
    (build (first (first fp)) (build (second (first fp)) (second fp))))) 
  
(module+ test
  (check-equal? (shift '((a b) c)) '(a (b c)))
  (check-equal? (shift '((a b) (c d))) '(a (b (c d)))))



(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))


(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (length* (first pora))
               (length* (second pora)))))))

(module+ test
  (check-equal? (length* '((a b) c)) 3)
  (check-equal? (length* '((a b) (c d))) 4))


(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* 2 (weight* (first pora)))
               (weight* (second pora)))))))

(module+ test
  (check-equal? (weight* '((a b) c)) 7)
  (check-equal? (weight* '(a (b c))) 5)
  (check-equal? (weight* '((a b) (c d))) 9))


; Swap the components of pairs when the first component is a pair.
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))


(module+ test
  (check-equal? (shuffle '(a (b c)))
                '(a (b c))))


(define one?
  (lambda (n)
    (eq? n 1)))

; Collatz function.
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      (else
       (cond
         ((even? n) (C (quotient n 2)))
         (else (C (add1 (* 3 n)))))))))
              

; Ackermann function.
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))




(define eternity
  (lambda (x)
    (eternity x)))


(define l2
  (lambda (l) ; length <= 2
    (cond
      ((null? l) 0)
      (else
       (add1
        ((lambda (l) ; length <= 1 
           (cond
             ((null? l) 0)
             (else
              (add1
               ((lambda (l)  ; length0
                  (cond
                    ((null? l) 0)
                    (else (add1 (eternity (cdr l))))))
                (cdr l))))))
         (cdr l))))))
  )
             
(module+ test
  (check-equal? (l2 '()) 0)
  (check-equal? (l2 '(a)) 1)
  (check-equal? (l2 '(a b)) 2)
  )


; Defines length0, such that we pass in a function (eternity)
((lambda (g-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
        (add1 (g-length (cdr l)))))))
 eternity)


((lambda (f)   ; length<=1
   (lambda (l)
     (cond
       ((null? l) 0)
       (else
        (add1 (f (cdr l)))))))
 ((lambda (g)  ; length0
    (lambda (l)
      (cond
        ((null? l) 0)
        (else
         (add1 (g (cdr l)))))))
  eternity
  )
 )



((lambda (h)   ; length<=2 (takes a function)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (h (cdr l)))))))
 ((lambda (g)  ; length<=1 (argument to length<=2)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  ((lambda (f) ; length0 (argument to length<=1)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (f (cdr l)))))))
   eternity
   )
  )
 )
 
 