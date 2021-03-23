#lang sicp

(define (filter predicates in)
  (if(null? predicates)
     (list in)
     (if((car predicates) in)
        (filter (cdr predicates) in)
        '())))
(define (filter-all predicates lo hi)
  (if(> lo hi)
     '()
     (append
      (filter predicates lo)
      (filter-all predicates (+ 1 lo) hi))))
;(filter-all (list positive? prime?) -4 10)
(define (even? x) (= (remainder x 2) 0))
(define (odd? x)  (not (even? x)))
(define (true x) #t)
(define (false x) #f)
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (multiple-of-hundred? x) (= (remainder x 100) 0))
(filter-all (list even? negative?) -4 4)