#lang racket

(require net/http-client)

(module+ test
  (require rackunit))

(define move-req 
  #"<game><board><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"mansion\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"house\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"grass\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"bush\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row></board><current><tile value=\"crystal\"></tile></current><storehouse><tile value=\"crystal\"></tile></storehouse></game>")
(define (client h p m u d)
  (define hc (http-conn-open h #:port p))
  (define-values (_ __ port) (http-conn-sendrecv! hc u
                       #:method m #:data d))
  (port->string port))

(client "127.0.0.1" 8080 #"POST" "/move" move-req)

;(module+ test
;  (check-equal? (client "127.0.0.1" 8080 #"GET" "/variant") 
;                "<variant value=\"basic\"></variant>")
;  (check-equal? (client "127.0.0.1" 8080 #"POST" "/variant") 
;                "<variant value=\"error\"></variant>"))

;(define move-req 
;  '(game 
;    (board 
;     (row (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "hut"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank")))))
;     (row (cell (tile ((value "tree"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank")))))
;     (row (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "mansion"))))
;          (cell (tile ((value "blank")))))
;     (row (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "house"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank")))))
;     (row (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "grass"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank")))))
;     (row (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "blank"))))
;          (cell (tile ((value "tree"))))
;          (cell (tile ((value "blank"))))))
;    (current (tile ((value "crystal"))))
;;    (storehouse (tile ((value "grass"))))))
                