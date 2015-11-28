#lang racket/base

(require racket/tcp net/url xml "account.rkt")

(provide serve)

(define my-account (create-account))

(define (serve port-no)
  (let ([serve-custo (make-custodian)])
    (parameterize ([current-custodian serve-custo])
      (define listener (tcp-listen port-no 5 #t))
      (define (loop)
        (accept-and-handle listener)
        (loop))
      (thread loop))
    (λ ()
      (custodian-shutdown-all serve-custo))))

(define (accept-and-handle listener)
  (let ([custo (make-custodian)])
    (parameterize ([current-custodian custo])
      (define-values (in out) (tcp-accept listener))
      (thread (λ ()
                (handle in out)
                (close-input-port in)
                (close-output-port out))))
    (thread (λ ()
              (sleep 10)
              (custodian-shutdown-all custo)))))

(define (handle in out)
  (let ([req regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
             (read-line in)])
    (when req
      (regexp-match #rx"(\r\n|^)\r\n" in)
      (let ([xexpr (dispatch (list-ref req 1))])
        (display "HTTP/1.0 200 OKAY\r\n" out)
        (display "Server: racket\r\nContent-Type: text/html\r\n\r\n<!DOCTYPE html>\r\n" out)
        (display (xexpr->string xexpr) out)))))

(define (dispatch str-path)
  (let* ([url (string->url str-path)]
         [path (map path/param-path (url-path url))]
         [match-cb (hash-ref dispatch-table (car path) #f)]
         [query (url-query url)])
    (if match-cb
        (match-cb query)
        (notfound query))))

(define (notfound query)
  '(html (head (title "404"))
         (body
          (h1 ((class "404")) "Nothing found here"))))

(define dispatch-table
  (hash
   "hello" (λ (query)
             '(html (head (title "Hello World!"))
                    (body (h1 "Hello world"))))
   "balance" (λ (query)
               `(html (head (title "Your account balance"))
                      (body (h1 "Account balance")
                            (p ,(number->string (balance my-account))))))
   "deposit" (λ (query)
               (set! my-account (deposit 20 my-account))
               `(html (head (title "Successful deposit"))))))



