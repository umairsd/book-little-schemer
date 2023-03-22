#lang racket
(require test-engine/racket-tests)

(define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

;; Chapter 1
(check-expect (atom? 'atom) #t)
; A list
(check-expect (atom? '(atom?)) #f) 
; Atom
(check-expect (atom? (car '(Harry had a heap of apples))) #t)
(check-expect (atom? '(atom? 'turkey 'b)) #f)
(check-expect (atom? "atom") #t)
(check-expect (atom? '(a b c)) #f)
(check-expect (atom? "another") #t)
; An S-expression
(check-expect (atom? '('('x 'y) 'z)) #f)
; Empty list
(check-expect (atom? '()) #f)
; car is like `head`
(check-expect (car '(a b c)) 'a)

; cdr is like `tail`
(check-expect (cdr '(a b c)) '(b c))
(check-expect (car (cdr '('(b) '(x y) '('(c))))) ''(x y))

; cons
(check-expect (cons 'peanut '(butter and jelly)) '(peanut butter and jelly))
(check-expect (cons '(a b (c)) '()) '((a b (c))))
(check-expect (cons 'a (car '((b) c d))) '(a b))

; null
(check-expect (null? (quote ())) #t)
(check-expect (null? '(a b c)) #f)

; eq
(check-expect (eq? 'Harry 'Harry) #t)
(define a1 'Harry)
(define a2 'Harry)
(check-expect (eq? a1 a2) #t)

(define l '(beans beans we need jelly beans))
(check-expect (eq? (car l) (car (cdr l))) #t)

;; Run all the tests.
(test)
