#lang sicp
(define enumerate
  (lambda (first last)
    (if (equal? first last)
        '()
        (cons first (enumerate (+ first 1) last)))))

(enumerate 1 7)
