#lang racket

(define make-monitored
  (lambda (f)
    (define count 0)
    (lambda (in)
      (cond
        [(equal? 'how-many-calls? in)
         count]
        [(equal? 'reset-count in)
         (set! count 0)]
        [else
         (begin (set! count (+ 1 count))
                (f in))]))))

(define square
  (lambda (x)
    (* x x)))

(define s (make-monitored square))
(s 100)
(s 100)
(s 'how-many-calls?)

(s 'reset-count)
(s 'how-many-calls?)
