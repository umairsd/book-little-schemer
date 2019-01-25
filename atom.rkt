; Define  `atom?` as described in the preface of the book, "The Little Schemer"
; This will be imported in other racket files, instead of copy pasted.

; NOTE: This files does not have a `#lang racket` statement at the top, as adding that
; statement creates a module, and thus we will be attempting to import a module within a module
; if we use this in other files, and likely isn't what we want.

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))