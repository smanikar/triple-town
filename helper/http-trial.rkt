#lang racket
(require web-server/servlet
         web-server/servlet-env
         xml
         xml/plist)

;(define (my-app req)
;  (response/xexpr
;   `(html (head (title "Hello world!"))
;          (body (p "Hey out there!")))))    
;   
;(serve/servlet my-app)

(printf "xexpr->xml\n")
(xexpr->xml '(variant ((value "basic"))))

(printf "\ndisplay-xml")
(display-xml (read-xml (open-input-string "<doc><bold>hi</bold>there!</doc>")))
(display-xml (read-xml (open-input-string "<variant value=\"basic\"/>")))

(printf "\n\ndoc-ele\n")
(xml->xexpr (document-element
             (read-xml (open-input-string
                        "<variant value=\"basic\"/>"))))

(printf "\nread-xml\n")
(read-xml (open-input-string "<doc><bold>hi</bold>there!</doc>"))