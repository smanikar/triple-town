#lang racket

(require web-server/servlet-env
         web-server/http/xexpr
         web-server/dispatch
         web-server/private/util
         web-server/http/request-structs
         web-server/http/response-structs
         net/url)

(require racket/contract)

(define-struct connection (id i-port o-port custodian)
  #:mutable)

(define-struct simple-request (method uri headers))

(provide/contract
 [struct connection
   ([id integer?]
    [i-port input-port?]
    [o-port output-port?]
    [custodian custodian?])]
 [new-connection (input-port? output-port? custodian? . -> . connection?)]
 [kill-connection! (connection? . -> . void)])

(define i (box 0))

(define (new-connection i-port o-port cust)
  (make-connection
     (begin0 (unbox i) (set-box! i (add1 (unbox i))))
     i-port o-port cust))

;; kill-connection!: connection -> void

;; Closes all the ports associated with a given connection and
;; shutdowns all resources associated with it.
(define (kill-connection! conn)
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-o-port conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-i-port conn)))
  (custodian-shutdown-all (connection-custodian conn)))

;; **************************************************
;; read-request: connection -> request
;; read the request line, and the headers
(define (read-request conn)
  (define ip 
    (connection-i-port conn))
  (define-values (method uri major minor)
    (read-request-line ip))
  (define headers 
    (read-headers ip))
  (make-simple-request method uri headers))


;; **************************************************
;; read-request-line  
(define match-method
  (let ([rx (byte-regexp #"^([^ ]+) (.+) HTTP/([0-9]+)\\.([0-9]+)$")])
    (lambda (a) (regexp-match rx a))))

; read-request-line : iport -> bytes url number number
; to read in the first line of an http request, AKA the "request line"
; effect: in case of errors, complain [MF: where] and close the ports
(define (read-request-line ip)
  (define line (read-bytes-line ip 'any))
  (if (eof-object? line)
      (network-error 'read-request "http input closed abruptly")
      (cond
        [(match-method line)
         => (match-lambda
              [(list _ method url major minor)
               (values method
                       (string->url (bytes->string/utf-8 url))
                       (string->number (bytes->string/utf-8 major))
                       (string->number (bytes->string/utf-8 minor)))])]
        [else (network-error 'read-request "malformed request ~a" line)])))

;; **************************************************
;; read-headers  
(define match-colon
  (let ([rx (byte-regexp (bytes-append #"^([^:]*):[ " (bytes 9) #"]*(.*)"))])
    (lambda (a) (regexp-match rx a))))

; read-headers : iport -> (listof header?)
(define (read-headers in)
  (let read-header ()
    (define l (read-bytes-line in 'any))
    (cond
      [(eof-object? l) null]
      [(zero? (bytes-length l)) null]
      [(match-colon l) 
       => (match-lambda
            [(list _ field value)
             (list* (make-header field (read-one-head in value))
                    (read-header))])]
      [else (network-error 'read-headers "malformed header")])))

; read-one-head : iport bytes -> bytes
(define (read-one-head in rhs)
  (match (peek-byte in)
    [(or 32 9) ;(or (eq? c #\space) (eq? c #\tab))
     ; (read-bytes-line in 'any) can't return eof
     ; because we just checked with peek-char
     ; Spidey: FLOW
     (read-one-head in (bytes-append rhs (read-bytes-line in 'any)))]
    [_ rhs]))

(define (my-app req)
  
  (read-request (make-connection 8080 8080 
                                 'localhost)))
  ;(response/xexpr
  ;`(html (head (title "Hello world!"))
  ;       (body (p "Hey out there!"))))
  
;  (response
;   301 #"OK"
;   (current-seconds) TEXT/HTML-MIME-TYPE
;   empty
;   (λ (op) (write-bytes #"<html><body>Hello, World!</body></html>" op))))

(serve/servlet my-app)