#lang sicp
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (memo-proc (lambda () exp)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())

(define (stream-null? s) (eq? s the-empty-stream))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (enumerate-interval low high)
  (if (> low high)
      'nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (square x) (* x x))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;=======================ends here====================



; 2 2 3 10
;(define (fu n)
;             (if (equal? n 1)
;                 (cons-stream 3 (fu (+ 1 n)))
;                 (cons-stream (stream-ref (pooh 1) (- n 2)) (fu (+ 1 n)))))

;(3 4 4 2 2 2 2 3 3 3)
;(4 4 2 2 2 2 3 3 3 3)

(define (fu n)
  (cons-stream (stream-ref Pooh (- n 2)) (fu (+ 1 n))))

(define (pooh n)
  (cons-stream (stream-ref Fu (- n 2)) (pooh (+ 1 n))))

(define Fu (cons-stream 2 (fu 2)))
(define Pooh (cons-stream 4 (pooh 2)))

(define (print s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (print (stream-cdr s) (- n 1)))))
(print Fu 30) ;(3 2 2 2 2 2 2 4 4 4)
;(print Pooh 10) ;(4 4 4 3 3 3 3 3 3 3)
