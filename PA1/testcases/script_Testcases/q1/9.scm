#lang sicp
(define (positive-and-zero? element)
  (cond
    [(positive?  element)
     #t]
    [(zero?  element)
     #t]
    [else
     #f]))

(define (filter check in)
  (if (null? in)
      '()
      (if (check (car in))
          (cons (car in) (filter check (cdr in)))
          (filter check (cdr in)))))

(define (filter_modified in first op)
  (if(eq? op <)
     (map (lambda (element)
         (+ element first))
         (filter negative?
                 (map (lambda (element)
                        (- element first))
                      in)))
     (map (lambda (element)
         (+ element first))
         (filter positive-and-zero?
                 (map (lambda (element)
                        (- element first))
                      in)))))

(define (partition in op1 op2)
  (if (null? in)
      '()
      (append (append (partition (filter_modified  (cdr in) (car in) op1) op1 op2) (list (car in))) (partition (filter_modified  (cdr in) (car in) op2) op1 op2))))

(define (make-qsort op)
  (cond
    [(eq? op >)
     (lambda (in)
       (partition in > <))]
    [ (eq? op <)
     (lambda (in)
       (partition in < >))]
    [else
     (lambda (in)
       (display "invalid operator"))]))

;((make-qsort >) '(6 1 1 2 9 8 0 5))
;((make-qsort <) '(6 1 1 2 9 8 0 5))
;((make-qsort =) '(6 1 1 2 9 8 0 5))

(define asc-sort (make-qsort <))
(define l '(-8 -28 30 88 72 -37 30 -61 -94 62))
(asc-sort l)