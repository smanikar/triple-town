#lang racket

(require xml
         )

(require "basic-moves-functions.rkt"
         "basic-player-server-functions.rkt"
         "basic-moves-definitions.rkt"
         "basic-player-server-helpers.rkt")

(define (parse-server-response r)
  (define e (xml->xexpr (read-xml/element (open-input-string r))))
  (match e
    [`(store ()) (values 0 0)]
    [`(place () 
             (row ((value ,y)))
             (column ((value ,x))))
     (values (string->number x) (string->number y))]))
     
(define (query-move b v)
  (define store (tile-v (car (car b))))
  (define xml-req (string->bytes/utf-8 (bcs->xml b (symbol->string v) 
            (symbol->string (if (symbol=? store 'blank)
                                     'none store)))))
  ;xml-req)
  ;(valid-xml? (bytes->string/utf-8 xml-req)))
  (parse-server-response (client "127.0.0.1" 8080 #"POST" "/move" xml-req)))
  
(define (main-loop n)
  (let loop ([b (generate-board n)]
             [v (generate-input)])
    (cond 
      [(not (end-game? b))
        (printf "\nNew-tile = ~a\n" v)
        (display-board b symbol->string/tile)
        (printf "**********************")
        (define-values (x y) (query-move b v))
        (define-values (b1 v1) (decide-move b x y v))
        (if (true? v1)
            (loop b1 v1)
            (loop b1 (generate-input)))]
      [else
        (printf "End of Game\nGood Bye!\n")])))

(main-loop 6)
        