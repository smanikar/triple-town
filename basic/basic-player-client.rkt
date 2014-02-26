#lang racket

(require "basic-player-functions.rkt")

(module+ test
  (require rackunit))

(define e2 ;
  #"<game><board><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"house\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"castle\"></cell><cell tile=\"castle\"></cell><cell tile=\"blank\"></cell><cell tile=\"mansion\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"bush\"></cell><cell tile=\"floating-castle\"></cell><cell tile=\"floating-castle\"></cell><cell tile=\"mansion\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"bush\"></cell><cell tile=\"castle\"></cell><cell tile=\"tree\"></cell><cell tile=\"tree\"></cell><cell tile=\"grass\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"grass\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"grass\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"tree\"></cell></row></board><current tile=\"crystal\"></current><storehouse tile=\"blank\"></storehouse></game>")
(define e3 
  #"<game><board><row><cell tile=\"grass\"></cell><cell tile=\"bush\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"hut\"></cell><cell tile=\"grass\"></cell><cell tile=\"bush\"></cell><cell tile=\"bush\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"hut\"></cell><cell tile=\"blank\"></cell><cell tile=\"bush\"></cell><cell tile=\"mansion\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"tree\"></cell><cell tile=\"bush\"></cell><cell tile=\"mansion\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row></board><current tile=\"grass\"></current><storehouse tile=\"crystal\"></storehouse></game>")
(define e4
  #"<game><board><row><cell tile=\"grass\"></cell><cell tile=\"bush\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"hut\"></cell><cell tile=\"grass\"></cell><cell tile=\"bush\"></cell><cell tile=\"bush\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"hut\"></cell><cell tile=\"blank\"></cell><cell tile=\"bush\"></cell><cell tile=\"mansion\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"tree\"></cell><cell tile=\"bush\"></cell><cell tile=\"mansion\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row><row><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell><cell tile=\"blank\"></cell></row></board><current tile=\"grass\"></current><storehouse tile=\"none\"></storehouse></game>")
(define e5
  #"<game><board><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell></row><row><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell></row><row><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell></row></board><current tile=\"imperial-bot\"></current><storehouse tile=\"none\"></storehouse></game>")
(define e6 ;
  #"<game><board><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell></row><row><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell></row><row><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell></row><row><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell><cell tile=\"tree\"></cell><cell tile=\"house\"></cell></row></board><current tile=\"grass\"></current><storehouse tile=\"crystal\"></storehouse></game>")
(define e7 ;
  #"<game></game>")

(module+ test
  (check-equal? 
   (client/post "127.0.0.1" 8080 "/move" e2)
   "<place row=\"1\" column=\"2\"></place>")
  (check-equal? 
   (client/post "127.0.0.1" 8080 "/move" e3)
   "<place row=\"2\" column=\"1\"></place>")
  (check-equal? 
   (client/post "127.0.0.1" 8080 "/move" e4)
   "<place row=\"2\" column=\"1\"></place>")
  (check-equal? 
   (client/post "127.0.0.1" 8080 "/move" e5)
   "<store></store>")
  (check-equal? 
   (client/post "127.0.0.1" 8080 "/move" e6)
   "<place row=\"0\" column=\"1\"></place>")
  (check-equal?
   (client/post "127.0.0.1" 8080 "/move" "<game></game>")
   "<error value=\"invalid-tree\"></error>"))

(module+ test
  (check-equal? (client/get "127.0.0.1" 8080 "/variant") 
                "<variant value=\"basic\"></variant>")
  (check-equal? (client/post "127.0.0.1" 8080 "/variant" #"void") 
                "<error value=\"invalid-method-or-path\"></error>"))

;(client/post "127.0.0.1" 8080 "/move" e2)
;(client/post "127.0.0.1" 8080 "/move" e3)
;(client/post "127.0.0.1" 8080 "/move" e4)
;(client/post "127.0.0.1" 8080 "/move" e5)
;(client/post "127.0.0.1" 8080 "/move" e6)
;(client/post "127.0.0.1" 8080 "/move" e7)