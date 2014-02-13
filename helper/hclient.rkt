#lang racket
;(require net/http-client)
(require web-server/servlet
         web-server/servlet-env
         xml)
(require net/http-client)

(define (client)
  (define hc (http-conn-open "127.0.0.1" #:port 8080))
  (http-conn-sendrecv! hc "/variant"
                       #:method #"GET"))
  ;(http-conn-close! hc))
      ; (make-url)
  ;(printf "~a" url))
;                                                 #"1.1"
;                                                 #"GET"
;                                                 'emtpy
;                                                 #f
;                                                 '(gzip)
;                                                 #f))