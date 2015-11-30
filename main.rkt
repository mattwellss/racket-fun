#lang racket

(require "account.rkt" "server.rkt")

(define my-account (create-account))

(run (make-server
      (hash
       "hello" (λ (query)
                 '(html (head (title "Hello World!"))
                        (body (h1 "Hello world"))))
       "balance" (λ (query)
                   `(html (head (title "Your account balance"))
                          (body (h1 "Account balance")
                                (p ,(number->string (balance my-account))))))
       "foldl-balance" (λ (query)
                         `(html (head (title "Your account balance"))
                                (body (h1 "Account balance")
                                      (p ,(number->string (foldl-balance my-account))))))
       "deposit" (λ (query)
                   (set! my-account (deposit 20 my-account))
                   `(html (head (title "Successful deposit")))))
      3030))

