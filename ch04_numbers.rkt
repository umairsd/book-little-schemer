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


(test)