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