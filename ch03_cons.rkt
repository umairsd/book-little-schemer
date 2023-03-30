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

; Replaces the first occurrence of old with new.
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))


; Replaces either the first occurrence of o1 or o2 with new.
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))


; Removes all occurrences of a given value from the list.
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) a) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))


; Insert multiple new values to the right of
; all old values.
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons old
                     (cons new
                           (multiinsertR new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertR new old (cdr lat)))))))))


; Insert multiple new values to the left of
; all old values.
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new (cons old
                               (multiinsertL new old (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertL new old (cdr lat)))))))))


; Replaces all the occurrences of old with new.
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new
                     (multisubst new old (cdr lat))))
              (else (cons (car lat)
                          (multisubst new old (cdr lat)))))))))


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

(check-expect (subst2 'ice 'chops 'ice lat) '(lamb ice and mint jelly))
(check-expect (subst2 'ice 'ice 'chops lat) '(lamb ice and mint jelly))


(check-expect (multirember 'and '(lamb and chops and gravy and mint and jelly))
              '(lamb chops gravy mint jelly))
(define lat1 '(coffee cup tea cup and hick cup))
(check-expect (multirember 'cup lat1) '(coffee tea and hick))


(define lat2 '(lamb chops chops mint chops))
(check-expect (multiinsertR 'xyz 'chops lat2)
              '(lamb chops xyz chops xyz mint chops xyz))
(check-expect (multiinsertR 'xyz 'ABC lat2) lat2)


(check-expect (multiinsertL 'xyz 'chops lat2)
              '(lamb xyz chops xyz chops mint xyz chops))
(define lat3 '(chips and fish or fish and fried))
(check-expect (multiinsertL 'fried 'fish lat3)
              '(chips and fried fish or fried fish and fried))


(check-expect (multisubst 'shrimp 'fish lat3)
              '(chips and shrimp or shrimp and fried))


(test)
