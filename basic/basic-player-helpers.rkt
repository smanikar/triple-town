#lang racket

(require "basic-moves-definitions.rkt")

(provide (all-defined-out))

;; Misc. Helper functions
;(xml->xexpr (read-xml/element (open-input-string ...)))

(define (board->xml/row r)
  (string-append 
   (cond
     [(empty? r) ""]
     [(and (equal? (tile-x (first r)) 0)
           (equal? (tile-y (first r)) 0)) 
      (board->xml/row (rest r))]
     [else
      (string-append "<cell tile=\"" 
                     (symbol->string (tile-v (first r))) 
                     "\"></cell>"
                     (board->xml/row (rest r)))])))

(define (board->xml b)
  (string-append
   (cond
     [(empty? b) ""]
     [else
      (string-append "<row>" (board->xml/row (first b)) "</row>"
                     (board->xml (rest b)))])))

;; bcs->xml : board string string -> string
;; converts 'b' 'c' 's' to XML string tree

(define (bcs->xml b c s)
  (string-append "<game><board>" (board->xml b) 
                 "</board><current tile=\""
                 c "\"></current><storehouse tile=\""
                 s "\"></storehouse></game>"))