#lang racket
(require web-server/servlet
         web-server/servlet-env)
 
(define (my-app req)
  (response/xexpr
   `(html (head (title "Hello world!"))
          (body (p "Hey out there!")))))
 
(serve/servlet my-app)