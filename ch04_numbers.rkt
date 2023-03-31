#lang racket
(require test-engine/racket-tests)

(define +o
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (add1 (+o a (sub1 b)))))))


(define -o
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (sub1 (-o a (sub1 b)))))))


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))


(define *o
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (*o n (sub1 m)))))))


(define *oo
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (*oo n (cond
                          ((> m 0) (sub1 m))
                          ((< m 0) (add1 m)))))))))

; A version that adds two lists together. If the lengths are
; mismatched, it returns the remaining elements of the longer
; list.
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) (quote()))
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))


(define lt
  (lambda (n m)
    (cond
      ((and (zero? m) (zero? n)) #f)
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt (sub1 n) (sub1 m))))))


(define gt
  (lambda (n m)
    (cond
      ((and (zero? m) (zero? n)) #f)
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))


(define eq
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (eq (sub1 n) (sub1 m))))))

; Equality in terms of lt and gt.
(define eq1
  (lambda (n m)
    (and (not (lt n m)) (not (gt n m)))))

(define eq2
  (lambda (n m)
    (cond
      ((lt n m) #f)
      ((gt n m) #f)
      (else #t))))


(define **
  (lambda (x n)
    (cond
      ((zero? n) 1)
      (else (* x (** x (sub1 n)))))))


; Integer division
(define quot
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quot (- n m) m))))))


(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))



(define pick
  (lambda (n lat)
    (cond
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))


(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


; a lat obtained by removing all the numbers.
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))))


; a lat obtained by removing all non-numbers
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))


; counts the number of times a appears in lat.
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? (car lat) a) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))


(define one?
  (lambda (n)
    (zero? (sub1 n))))


;; Tests
(check-expect (+o 4 13) 17)

(check-expect (-o 13 7) 6)
(check-expect (-o 7 9) -2)

(check-expect (addtup '(1 2 3 4 5 6)) 21)

(check-expect (*o 7 3) 21)
(check-expect (*o -7 3) -21)


(check-expect (tup+ '(1 2 3 4) '(1 1 1 1)) '(2 3 4 5))
(check-expect (tup+ '(1 2 3 4 5) '(1 1 1 1)) '(2 3 4 5 5))

(check-expect (lt 4 5) #t)
(check-expect (lt 5 4) #f)
(check-expect (lt 0 4) #t)
(check-expect (lt 4 0) #f)
(check-expect (lt 4 4) #f)

(check-expect (gt 4 5) #f)
(check-expect (gt 5 4) #t)
(check-expect (gt 0 4) #f)
(check-expect (gt 4 0) #t)
(check-expect (gt 4 4) #f)

(check-expect (eq 4 5) #f)
(check-expect (eq 5 4) #f)
(check-expect (eq 0 4) #f)
(check-expect (eq 4 0) #f)
(check-expect (eq 4 4) #t)

(check-expect (eq1 4 5) #f)
(check-expect (eq1 5 4) #f)
(check-expect (eq1 0 4) #f)
(check-expect (eq1 4 0) #f)
(check-expect (eq1 4 4) #t)

(check-expect (eq2 4 5) #f)
(check-expect (eq2 5 4) #f)
(check-expect (eq2 0 4) #f)
(check-expect (eq2 4 0) #f)
(check-expect (eq2 4 4) #t)

(check-expect (** 1 1) 1)
(check-expect (** 2 3) 8)
(check-expect (** 5 3) 125)

(check-expect (quot 15 4) 3)
(check-expect (quot 15 3) 5)
(check-expect (quot 15 2) 7)

(check-expect (len '(a b c d)) 4)

(define lat '(lasagna spaghetti ravioli macaroni meatball))
(check-expect (pick 4 lat) 'macaroni)
(check-expect (pick 1 lat) 'lasagna)

(check-expect (rempick 3 lat) '(lasagna spaghetti macaroni meatball))

(check-expect (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))
(check-expect (no-nums '(1 2 3 4)) '())

(check-expect (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))
(check-expect (all-nums '(1 2 3 4)) '(1 2 3 4))

(check-expect (eqan? 3 3) #t)
(check-expect (eqan? 3 4) #f)
(check-expect (eqan? 'ab 'ab) #t)
(check-expect (eqan? 'ab 'ac) #f)
(check-expect (eqan? 'f 5) #f)

(check-expect (occur 4 '(1 4 5 4 1 3 4)) 3)
(check-expect (occur 9 '(1 4 5 4 1 3 4)) 0)

(check-expect (one? 1) #t)
(check-expect (one? 2) #f)
(check-expect (one? 0) #f)

(test)