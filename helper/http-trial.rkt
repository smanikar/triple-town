#lang racket
;#lang web-server/insta
(require web-server/servlet
         web-server/servlet-env
         xml)
         ;net/http-client)
;xml/plist
;web-server-lib)


(define (my-app req)
  (print-req req)
  (printf "~~user = ~a\n " (url-user (request-uri req)))
  (printf "~~host = ~a\n " (url-host (request-uri req)))
  (response/xexpr
   (cond
     [(equal? (request-method req) #"GET")
      `(variant ((value "basic")))]
;      `(html (head (title "Hello world!"))
;             (body (p "A GET method!")))]
     [(equal? (request-method req) #"POST")
      `(html (head (title "Hello world!"))
             (body (p "A POST method!")))])))

;   ;(read-line req)))
;   (when r
;     ; Discard the rest of the header (up to blank line):
;     (regexp-match #rx"(\r\n|^)\r\n" req)
;     ; Dispatch:
;     (let ([xexpr (dispatch (list-ref r 1))])
;       ; Send reply:


;   `(html (head (title "Hello world!"))
;          (body (p "Hey out there!")))))  

(define (print-req r)
  (printf "\n****************\n")
  (printf "req -")
  (printf "\n~a\n" r)
  (printf "method = ~a\n" (request-method r))
  (printf "t/f ~a\n" (equal? (request-method r) #"GET"))
  ;(equal? (request-method r) 'GET)
  (printf "uri = ~a\n" (request-uri r))
  (printf "headers = ~a\n" (request-headers/raw r))
  (printf "bindings = ~a\n" (request-bindings/raw-promise r))
  (printf "postdata = ~a\n" (request-post-data/raw r))
  (printf "hostip = ~a\n" (request-host-ip r))
  (printf "host-port = ~a\n" (request-host-port r))
  (printf "client-ip = ~a\n" (request-client-ip r))
  (printf "\n****************\n")
  (printf "url info -\n")
  (printf "scheme = ~a\n " (url-scheme (request-uri r)))
  (printf "user = ~a\n " (url-user (request-uri r)))
  (printf "host = ~a\n " (url-host (request-uri r)))
  (printf "port = ~a\n " (url-port (request-uri r)))
  (printf "path-absolute = ~a\n " (url-path-absolute? (request-uri r)))
  (printf "query = ~a\n " (url-query (request-uri r)))
  (printf "fragment = ~a\n " (url-fragment (request-uri r))))

;   (extract-bindings
;    'method
;    (request-bindings r))))

(define (dispatch str-path)
  ; Parse the request as a URL:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; Call a handler:
      (h (url-query url))
      ; No handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (λ (query)
             `(html (body "Hello, World!"))))

(serve/servlet my-app #:port 8080
               #:servlet-path "/variant")

;(printf "xexpr->xml\n")
;(xexpr->xml '(variant ((value "basic"))))
;
;(printf "\ndisplay-xml")
;(display-xml (read-xml 
; (open-input-string "<doc><bold>hi</bold>there!</doc>")))
;(display-xml (read-xml (open-input-string "<variant value=\"basic\"/>")))
;
;(printf "\n\ndoc-ele\n")
;(xml->xexpr (document-element
;             (read-xml (open-input-string
;                        "<variant value=\"basic\"/>"))))
;
;(printf "\nread-xml\n")
;(read-xml (open-input-string "<doc><bold>hi</bold>there!</doc>"))
;
;2/10
;serve/servlet
;url->string
;request-uri
;extract-bindings
;request-bindings
;response/xexpr
;(let-values [(a b c) (http-sendrecv... )])
;request-post-data/raw
