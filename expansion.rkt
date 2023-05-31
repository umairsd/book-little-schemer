#lang racket

(define f
  (lambda (newlat4 seen4) ; col-4
    ((lambda (newlat3 seen3) ; col-3
       ((lambda (newlat2 seen2) ; col-2
          ((lambda (newlat1 seen1); col-1
             (col-0 (cons 'a newlat1) seen1)) ; arguments to col-0
          
           newlat2 (cons 'b seen2))) ; arguments to col-1
        
        (cons 'c newlat3) seen3)) ; arguments to col-2
      
     newlat4 (cons 'b seen4))) ; arguments to col-3
  ) ; y


(define x
  (lambda (n m)
    (list n m)))

(define z
  (lambda (odds4 evens4)
    ((lambda (odds3 evens3)
       ((lambda (odds2 evens2)
          ((lambda (odds1 evens1)
             (col-0 (cons 1 odds1) evens1))
           odds2 (cons 2 evens2)))
        (cons 3 odds3) evens3))
     odds4 (cons 4 evens4)))
  )


(define col-0 list)


((lambda (newlat4 seen4) ; col-4
   ((lambda (newlat3 seen3) ; col-3
      ((lambda (newlat2 seen2) ; col-2
         ((lambda (newlat1 seen1); col-1
            
            (col-0 (cons 'a newlat1) seen1)) ; arguments to col-0
          newlat2 (cons 'b seen2))) ; arguments to col-1
       (cons 'c newlat3) seen3)) ; arguments to col-2
    newlat4 (cons 'b seen4))) ; arguments to col-3

 '() '()
 )
