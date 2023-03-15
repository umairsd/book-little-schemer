#lang racket
(require test-engine/racket-tests)

(define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(check-expect (atom? "atom") #t)
(check-expect (atom? '(a b c)) #f)
(check-expect (atom? "another") #t)
(test)
