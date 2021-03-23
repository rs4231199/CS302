#lang racket
(define precision 0.001)
(define (cube x)
  (* x x x))
(define (cuberoot x y)
  (define next (/ (+ (/ x (* y y)) (* 2.0 y)) 3.0))
  (if (> (abs (- (cube next) x)) precision) (cuberoot x next) next) 
)
(cuberoot 100.0 1.0)

