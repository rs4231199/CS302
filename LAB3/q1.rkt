#lang racket

(define make-account
  (lambda (balance real-password)
    (lambda (entered-password action)
      (lambda (amount)
        (if (equal? real-password entered-password)
            (cond
              [(equal? 'withdraw action)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient balance")]
              [(equal? 'deposit action)
               (begin 
                 (set! balance (+ balance amount))
                 balance)]
              [else
               "Wrong action"])
            "Incorrect password")))))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'withdraw) 40)
((acc 'nakli-password 'withdraw) 50)
((acc 'secret-password 'deposit) 20)


