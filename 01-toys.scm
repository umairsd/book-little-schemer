#lang scheme

; define this at the beginning of each file.
; TODO: Determine how to import this.
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? (quote ()))


