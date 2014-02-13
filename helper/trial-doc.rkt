#lang racket
(require racket/enter
         xml net/url)

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  (define req
    ; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; Dispatch:
    (let ([xexpr (dispatch (list-ref req 1))])
      ; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  (printf "\nstr-path = \n~a\n" str-path)
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

;(define (handle in out)
;  (printf "in is \n~a\n" in)
;  ;Discard the request header (up to blank line):
;  (printf "regex is \n~a\n" (regexp-match #rx"(\r\n|^)\r\n" in))
;  ;Send reply:
;  (display "HTTP/1.0 200 Okay\r\n" out)
;  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
;  (display "<html><body>Hello, world!</body></html>" out))

(serve 8080)