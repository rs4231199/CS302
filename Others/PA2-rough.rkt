#lang sicp

(define-syntax delay
  (syntax-rules ()
    ((delay expr) (lambda () expr))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (force promise) (promise))

(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '())

(define (stream-null? s) (eq? s the-empty-stream))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))




;=======================ends here====================

(define (get-from s n)
  (define (get-favorite s n c1 c2 c3 c4)
    (if (<= n 1)
        (let ()
          (cond
            ((equal? (stream-car s) 1) (set! c1 (inc c1)))
            ((equal? (stream-car s) 2) (set! c2 (inc c2)))
            ((equal? (stream-car s) 3) (set! c3 (inc c3)))
            ((equal? (stream-car s) 4) (set! c4 (inc c4))))
          (define favorite 4)
          (define its-count 0)
          (if(> c4 its-count) (begin (set! favorite 4) (set! its-count c4)))
          (if(> c3 its-count) (begin (set! favorite 3) (set! its-count c3)))
          (if(> c2 its-count) (begin (set! favorite 2) (set! its-count c2)))
          (if(> c1 its-count) (begin (set! favorite 1) (set! its-count c1)))
          favorite)
        (let ()
          (cond
            ((equal? (stream-car s) 1) (set! c1 (inc c1)))
            ((equal? (stream-car s) 2) (set! c2 (inc c2)))
            ((equal? (stream-car s) 3) (set! c3 (inc c3)))
            ((equal? (stream-car s) 4) (set! c4 (inc c4))))
          (get-favorite (stream-cdr s) (- n 1) c1 c2 c3 c4))))

  (define (beats-favorite favorite)
    (cond
      ((equal? favorite 1) 4)
      ((equal? favorite 2) 3)
      ((equal? favorite 3) 4)
      ((equal? favorite 4) 2)))
  
  (beats-favorite (get-favorite s n 0 0 0 0)))

; 2 2 3 10
;(define (fu n)
;             (if (equal? n 1)
;                 (cons-stream 3 (fu (+ 1 n)))
;                 (cons-stream (stream-ref (pooh 1) (- n 2)) (fu (+ 1 n)))))

;(3 4 4 2 2 2 2 3 3 3)
;(4 4 2 2 2 2 3 3 3 3)

(define (fu n)
  (cond ((= n 1)
         (cons-stream 3 (fu (+ 1 n))))
        (else
         (cons-stream (get-from (pooh 1) (- n 1)) (fu (+ 1 n))))))

(define (pooh n)
  (cond ((= n 1)
         (cons-stream 4 (pooh (+ 1 n))))
        (else 
         (cons-stream (get-from (fu 1) (- n 1)) (pooh (+ 1 n))))))

;(stream-ref (fu 1) 1)

(define (print s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (print (stream-cdr s) (- n 1)))))
(print (fu 1) 10) ;(3 2 2 2 2 2 2 4 4 4)
(print (pooh 1) 10) ;(4 4 4 3 3 3 3 3 3 3)
