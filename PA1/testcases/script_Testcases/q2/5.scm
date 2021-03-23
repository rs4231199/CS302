#lang sicp

(define (cddr_modified in)
  (if(null? (cdr in))
     '()
     (cddr in)))
(define (cddddr_modified in)
  (cond
    ([null? (cdr in)] '())
    ([null? (cddr in)] '())
    ([null? (cdddr in)] '())
    (else (cddddr in))))
(define (fewest-moves-helper in cnt)
  (if(null? in)
     cnt
     (if(equal? 1 (car in))
        (min (fewest-moves-helper (cdr in) (+ 1 cnt))
             (fewest-moves-helper (cddr_modified in) (+ 1 cnt)))
        (min (fewest-moves-helper (cdr in) (+ 1 cnt))
             (fewest-moves-helper (cddddr_modified in) (+ 1 cnt))))))
(define fewest-moves (lambda (in) (fewest-moves-helper in 0)))
;(fewest-moves '(1 0 1 1 1 0 1 1 0 1 0 1 1 0 0 1 1 1))
;(fewest-moves '(0 0 0 0 0))
(fewest-moves '(0 1))