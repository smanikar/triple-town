#lang racket 

(require web-server/http net/url) 

(define (display-request req) 
  (display (format "~s\n~s\n" (request-method req) (request-uri req))) 
  (for ((r (request-headers/raw req))) 
    (displayln r)) 
  (for ((b (request-bindings/raw req))) 
    (displayln b)) 
  (display (format "~s\n" (request-post-data/raw req)))) 

(define (connect host port) 
  (define main-cust (make-custodian)) 
  (parameterize ((current-custodian main-cust)) 
    (define-values (in out)(tcp-connect host port)) 
    (define-values (ip-in port-in ip-out port-out) (tcp-addresses in #t)) 
    (define method #"GET") 
    (define uri (string->url "http://localhost:8080/resources/data")) 
    (define headers 
      (list 
       (make-header #"Host" #"localhost:8080") 
       (make-header #"Connection" #"keep-alive") 
       (make-header #"User-Agent" #"Mozilla/5.0 (testClient.rkt 0.1)") 
       (make-header #"Accept-Encoding" #"gzip") 
       (make-header #"Accept" #"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8") 
       (make-header #"Accept-Language" #"en-us") 
       (make-header #"Accept-Charset" #"ISO-8859-1,UTF-8;q=0.7,*;q=0.7") 
       (make-header #"Cache-control" #"no-cache"))) 
    
    (define req (make-request 
                 method 
                 uri 
                 headers 
                 (delay empty) 
                 #"" 
                 "127.0.0.1" 
                 8080 
                 "127.0.0.1")) 
    (display (format "Client: ~a:~s\nServer: ~a:~s\n" ip-in port-in ip-out port-out)) 
    (print req out) 
    (for ([i (in-range 10000000)]) 
      i) 
    (display 
     (format "Response:\n~s" 
             (port->string (get-impure-port (string->url "http://localhost:8080"))))) 
    (close-input-port in) 
    (close-output-port out) 
    (custodian-shutdown-all main-cust))) 
