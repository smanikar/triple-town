#lang racket
;#lang web-server/base	 
;#lang web-server

(require web-server/servlet-env
         web-server/http
         net/url)

(require web-server/stuffers/stuffer)
(require web-server/stuffers/serialize)
(require web-server/stuffers/hash)

(provide interface-version stuffers start)
(define interface-version 'stateless)

(define (display-request req)
  (display (format "Method: ~s\nURL: ~s\n" 
                   (request-method req) (url->string (request-uri req))))
  (displayln "Headers:\n")
  (for ((r (request-headers/raw req)))
    (displayln r)) 
  (displayln "Bindings:\n") 
  (for ((b (request-bindings/raw req))) 
    (displayln (format "Bindings: ~s\n" b))) 
  (display (format "POST-data:\n~s\n" (request-post-data/raw req))))

(define stuffers 
  (stuffer-chain 
   serialize-stuffer 
   (md5-stuffer (build-path (find-system-path 'home-dir) ".urls"))))

(define (start req) 
  ; handle request 
  (fprintf (current-output-port) "\n\n**** new request ****\n~s" req) 
  (display "\n---\n\n") 
  (display-request req) 
  (display "**** sending response ...") 
  ; send response 
  (begin0 
    (response/xexpr 
     `(html (body (h2 "Servlet response...")))) 
    (displayln "sent! ****\n"))) 

(serve/servlet start 
               #:stateless? #t 
               #:servlet-regexp #rx"" 
               #:port 8080) 