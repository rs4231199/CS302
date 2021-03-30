#lang racket

(define make-account
  (lambda (balance real-password)
    (define wrong_password_count 0)
    (define call_the_cops
      (lambda ()
        "you have the right to remain silent..."))
    (lambda (entered-password action)
      (lambda (amount)
        (if (equal? real-password entered-password)
            (begin (set! wrong_password_count 0)
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
                      "Wrong action"]))
            (begin (set! wrong_password_count (+ wrong_password_count 1))
                   (if (>= wrong_password_count 5)
                       (call_the_cops)
                       "Incorrect password")))))))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'secret-password 'withdraw) 40)
((acc 'nakli-password 'withdraw) 50)
((acc 'secret-password 'deposit) 20)

((acc 'fake-password 'withdraw) 40)
((acc 'fake-password 'withdraw) 40)
((acc 'fake-password 'withdraw) 40)
((acc 'fake-password 'withdraw) 40)
((acc 'fake-password 'withdraw) 40)
((acc 'fake-password 'withdraw) 40)

((acc 'secret-password 'deposit) 20)
((acc 'fake-password 'withdraw) 40)

