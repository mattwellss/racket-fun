#lang racket/base

(require racket/list)
(require racket/contract)

(struct account (transactions number))

(provide (contract-out
          [deposit (-> positive? account? any)]
          [withdraw (-> positive? positive-account? any)]
          [balance (-> account? any)]))

(provide create-account account-number)

(define account-count 0)

(define (positive-account? my-account)
  (> (balance my-account) 0))

(define (create-account)
  (set! account-count (add1 account-count))
  (account '() account-count))

(define (deposit amount my-account)
  (add-transaction my-account amount))

(define (withdraw amount my-account)
  (add-transaction my-account (* -1 amount)))

(define (add-transaction my-account amount)
  (account
   (cons amount (account-transactions my-account))
   (account-number my-account)))

(define (balance my-account)
  (let int-balance ([transactions (account-transactions my-account)]
                    [sum 0])
    (if (empty? transactions)
        sum
        (int-balance (rest transactions) (+ (first transactions) sum)))))
