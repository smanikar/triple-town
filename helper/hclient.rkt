#lang racket

(require web-server/servlet
         web-server/servlet-env
         xml
         net/http-client
         rackunit
         test-engine/racket-tests)

(define (variant-client-get)
  (define hc (http-conn-open "127.0.0.1" #:port 8080))
  (define-values (_ __ port) (http-conn-sendrecv! hc "/variant"
                       #:method #"GET"))
  (port->string port))

(define (variant-client-post)
  (define hc (http-conn-open "127.0.0.1" #:port 8080))
  (define-values (_ __ port) (http-conn-sendrecv! hc "/variant"
                       #:method #"POST"))
  (port->string port))

(module+ test
  (check-equal? (variant-client-get) 
                "<variant value=\"basic\"></variant>")
  (check-equal? (variant-client-post) 
                "<variant value=\"error\"></variant>"))
                