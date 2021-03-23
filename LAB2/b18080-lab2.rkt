#lang racket
(define zero (lambda () '()))
(define (succ x) (lambda () x))
(define (pred x) (x))
; numbers
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
; operators
(define (is-zero? x) (null? (x)))
(define (is-equal? x y)
  (cond ((is-zero? x) (is-zero? y))
        ((is-zero? y) (is-zero? x))
        (else (is-equal? (pred x) (pred y)))))

; 1. zero is not equal to one, but is equal to zero
(printf "1. \n")
(is-equal? zero one)
(is-equal? zero zero)
; 2. four is equal to succ(succ(succ(succ(zero))))
(printf "2. \n")
(is-equal? four (succ(succ(succ(succ(zero))))))
; 3. The predecessor of the successor of two is two
(printf "3. \n")
(is-equal? two (pred (succ two)))

; add-church
(printf "add-church \n")
(define (add-church x y)
  (if (is-zero? y)
      x
      (add-church (succ x) (pred y))))
(is-equal? four (add-church one three))
(is-equal? (add-church one two) (add-church one three))
; subtract-church, x must be >= y
(printf "subtract-church \n")
(define (subtract-church x y)
  (if (is-zero? y)
      x
      (subtract-church (pred x) (pred y))))
(is-equal? two (subtract-church four two))
; multiply-church
(printf "multiply-church \n")
(define (multiply-church x y)
  (if (is-zero? y)
      zero
      (add-church x (multiply-church x (pred y)))))
(is-equal? (multiply-church two two) four)
(is-equal? (multiply-church one three) three)
(is-equal? (multiply-church zero three) zero)



