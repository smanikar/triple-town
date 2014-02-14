#lang racket

(require net/http-client)

(module+ test
  (require rackunit))

(define (client h p m u)
  (define hc (http-conn-open h #:port p))
  (define-values (_ __ port) (http-conn-sendrecv! hc u
                       #:method m))
  (port->string port))

(module+ test
  (check-equal? (client "127.0.0.1" 8080 #"GET" "/variant") 
                "<variant value=\"basic\"></variant>")
  (check-equal? (client "127.0.0.1" 8080 #"POST" "/variant") 
                "<variant value=\"error\"></variant>"))
