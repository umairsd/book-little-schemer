#lang racket
(require test-engine/racket-tests)

(define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(define member? 
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

; My own implementation...which is what the book came up with. 
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

; Initial implementation in the book, to highlight the need for cons.
(define rember1
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember1 a (cdr lat))))))))


(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))


(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))


;; Tests

(define lat '(lamb chops and mint jelly))
(check-expect (rember1 'mint lat) '(jelly))
(check-expect (rember 'mint lat) '(lamb chops and jelly))


(define lls '((apple peach pumpkin)
              (plum pear cherry)
              (grape raisin pea)
              (bean carrot eggplant)))
(check-expect (firsts lls) '(apple plum grape bean))


(check-expect (insertR 'pudding 'jelly lat) '(lamb chops and mint jelly pudding))
(check-expect (insertR 'yummy 'lamb lat) '(lamb yummy chops and mint jelly))
(check-expect (insertR 'sweet 'and lat) '(lamb chops and sweet mint jelly))
(check-expect (insertR 'sweet 'bbbbb lat) lat)

(check-expect (insertL 'flavored 'jelly lat) '(lamb chops and mint flavored jelly))
(check-expect (insertL 'yummy 'lamb lat) '(yummy lamb chops and mint jelly))
(check-expect (insertL 'sweet 'mint lat) '(lamb chops and sweet mint jelly))
(check-expect (insertL 'sweet 'bbbbb lat) lat)

(check-expect (subst 'pudding 'jelly lat) '(lamb chops and mint pudding))
(check-expect (subst 'beef 'lamb lat) '(beef chops and mint jelly))
(check-expect (subst 'sweet 'mint lat) '(lamb chops and sweet jelly))
(check-expect (subst 'sweet 'bbbbb lat) lat)

(test)
