# book-little-schemer
Notes and snippets from reading the book - The Little Schemer

I will be using Racket to follow along, and will be using the DrRacket IDE. A few notes on Racket vs Scheme.


- The book calls out defining the `atom?` function as below. I've defined a keyboard macro that allows me to paste this in (when needed).


```racket
(define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
```

- Second thing to keep in mind is that string literals have to be in double quotes.
- Finally, the list needs to be prefixed with a single quote. So `(list 1 2 3)` becomes `'(1 2 3)`.