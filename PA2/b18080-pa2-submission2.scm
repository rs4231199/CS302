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


; Hobbit | Dwarf | Elf | Orc
; O > H
; H > D
; D > E
; E > O
; D > O
; H > E

; Fu and Pooh
(define Fu the-empty-stream)
(define Pooh the-empty-stream)

(define (play strategyFu strategyPooh firstMoveOfFu numberOfRounds)
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
  (define (beats-favorite favorite)
    (cond
      ((equal? favorite 1) 4)
      ((equal? favorite 2) 3)
      ((equal? favorite 3) 4)
      ((equal? favorite 4) 2)))
  (cond ((and (equal? strategyFu 1) (equal? strategyPooh 1))
         (let ()
           (define (fu n)
             (cons-stream (stream-ref Pooh (- n 2)) (fu (+ 1 n))))
           (define (pooh n)
             (cons-stream (stream-ref Fu (- n 2)) (pooh (+ 1 n))))

           ; (stream-ref (fu 1) numberOfRounds)
           (set! Fu (cons-stream firstMoveOfFu (fu 2)))
           (set! Pooh (cons-stream 4 (pooh 2)))))
        ((and (equal? strategyFu 2) (equal? strategyPooh 1))
         (let ()
           (define (fu n)
             (cons-stream (get-from Pooh (- n 1)) (fu (+ 1 n))))
           (define (pooh n)
             (cons-stream (stream-ref Fu (- n 2)) (pooh (+ 1 n))))

           ;(stream-ref (fu 1) numberOfRounds)
           (set! Fu (cons-stream firstMoveOfFu (fu 2)))
           (set! Pooh (cons-stream 4 (pooh 2)))))
        ((and (equal? strategyFu 1) (equal? strategyPooh 2))
         (let ()
           (define (fu n)
             (cons-stream (stream-ref Pooh (- n 2)) (fu (+ 1 n))))
           (define (pooh n)
             (cons-stream (get-from Fu (- n 1)) (pooh (+ 1 n))))

           ;(stream-ref (fu 1) numberOfRounds)
           (set! Fu (cons-stream firstMoveOfFu (fu 2)))
           (set! Pooh (cons-stream 4 (pooh 2)))))
        ((and (equal? strategyFu 2) (equal? strategyPooh 2))
         (let ()
           (define (fu n)
             (cons-stream (get-from Pooh (- n 1)) (fu (+ 1 n))))
           (define (pooh n)
             (cons-stream (get-from Fu (- n 1)) (pooh (+ 1 n))))
           
           ;(stream-ref (fu 2) numberOfRounds)
           (set! Fu (cons-stream firstMoveOfFu (fu 2)))
           (set! Pooh (cons-stream 4 (pooh 2))))))

  (define (compare-hdef x1 x2)
    (cond
      ((and (equal? x1 1) (equal? x2 2)) 1)
      ((and (equal? x1 1) (equal? x2 3)) 3)
      ((and (equal? x1 1) (equal? x2 4)) 4)
      ((and (equal? x1 2) (equal? x2 3)) 3)
      ((and (equal? x1 2) (equal? x2 4)) 2)
      ((and (equal? x1 3) (equal? x2 4)) 4)
      
      ((and (equal? x1 2) (equal? x2 1)) 1)
      ((and (equal? x1 3) (equal? x2 1)) 3)
      ((and (equal? x1 4) (equal? x2 1)) 4)
      ((and (equal? x1 3) (equal? x2 2)) 3)
      ((and (equal? x1 4) (equal? x2 2)) 2)
      ((and (equal? x1 4) (equal? x2 3)) 4)))
  
  (define (find-winner n fu pooh win1 win2)
    (if (= n 0)
        (cond ((> win1 win2) 1)
              ((equal? win1 win2) 0)
              ((< win1 win2) 2))
        (cond ((equal? (stream-car fu) (stream-car pooh))
               (find-winner (- n 1) (stream-cdr fu) (stream-cdr pooh) win1 win2))
              ((= (compare-hdef (stream-car fu) (stream-car pooh)) (stream-car fu))
               (find-winner (- n 1) (stream-cdr fu) (stream-cdr pooh) (+ win1 1) win2))
              (else
               (find-winner (- n 1) (stream-cdr fu) (stream-cdr pooh) win1 (+ win2 1))))))

  (find-winner numberOfRounds Fu Pooh 0 0))





;(play 2 1 3 10)
;(play 2 1 1 4)
;(play 2 1 4 7)
;(play 2 1 2 10)
;(play 1 2 4 3)
;(play 1 1 1 3)
;(play 1 1 2 4)
;
;(play 2 2 3 10)
;(play 2 2 4 9)
;(play 2 2 2 4)
;(play 2 2 1 5)


;(define (print s n)
;  (if (= n 0)
;      '()
;      (cons (stream-car s) (print (stream-cdr s) (- n 1)))))
;(print Fu 10)
;(print Pooh 10)


;Fu
;Pooh





