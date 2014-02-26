#lang racket

(require xml)

(require "basic-moves-functions.rkt"
         "basic-player-functions.rkt"
         "basic-moves-definitions.rkt"
         "basic-player-helpers.rkt")

(define (parse-server-response r)
  (printf "res - ~a\n" r)
  (define e (xml->xexpr (read-xml/element (open-input-string r))))
  (match e
    [`(store ()) (values 0 0)]
    [`(place ((row ,y) (column ,x)))
     (values (string->number x) (string->number y))]
    [lse (error "Invalid response\n" e)]))

(define (query-move b v)
  (define store (tile-v (car (car b))))
  (define xml-req (string->bytes/utf-8 
                   (bcs->xml b (symbol->string v) 
                             (symbol->string (if (symbol=? store 'blank)
                                                 'none store)))))
  ;(query-move (generate-board 6) 'grass)
  ;(display xml-req)
;  (if (false? (valid-xml? (bytes->string/utf-8 xml-req)))
;      (error "Invalid XML\n") (bytes->string/utf-8 xml-req))
  (parse-server-response (client/post "127.0.0.1" 8080 "/move" xml-req)))

(define (main-loop n)
  (let loop ([b (generate-board n)]
             [v (generate-input)]
             [c 1])
    (cond 
      [(not (end-game? b))
       ;(display-board b symbol->string/tile)
       ;(printf "\nNew-tile = ~a\n" v)
       ;(read)
       (define-values (x y) (query-move b v))
       ;(printf "\nMove - (~a , ~a)\n" x y)
       ;(printf "**********************\n")
       (define-values (b1 v1) (decide-move b x y v))
       (if (true? v1)
           (loop b1 v1 (add1 c))
           (loop b1 (generate-input) (add1 c)))]
      [else
       (printf "End of Game\nGood Bye! ~a \n" c)])))

(main-loop 6)