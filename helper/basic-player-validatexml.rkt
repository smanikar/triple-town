#lang racket
;; -------------------------------------------------------------------------
;; recognizing an XML that belongs to a grammar 

;; E = <game>B C S </game>
;; B = <board> R0 R R R R R </board>
;; C = <current> T </current>
;; S = <storehouse> T </storehouse>
;; R0 = <row> L L L L L </row>
;; R = <row> L L L L L L </row>
;; T = <tile> <value = V> </tile>
;; L = <cell> T </cell>
;; V = "blank"
;;   | "grass"
;;   | "bush"
;;   | "tree"
;;   | "hut"
;;   | "mansion"
;;   | "castle"
;;   | "floating-castle"
;;   | "triple-castle"

;; valid-xml? : String -> boolean
;; does E produce e? 

(define (valid-xml? e:str)
  (define e:xml (read-xml/element (open-input-string e:str)))
  (define e:xexpr (xml->xexpr e:xml))
  (let valid-xml? ((e e:xexpr))
    (match e
      [`(game () ,b ,c ,s) 
       (and (valid-board? b) 
            (valid-current? c) 
            (valid-store? s))]
      ;(build-board b c s)]
      [else #f])))

(module+ test
  (check-equal? (valid-xml? (bcs->xml b9 "grass" "none"))
                #t)
  (check-equal? (valid-xml? (bcs->xml b1 "grass" "none"))
                #f))

;; -------------------------------------------------------------------------
;; valid-board? string -> boolean
;; is b a valid board (1 row0 and 5 rows)

(define (valid-board? b)
  (match b
    [`(board() ,r0 ,r1 ,r2 ,r3 ,r4 ,r5) 
     (and (valid-row0? r0)
          (valid-row? r1)
          (valid-row? r2)
          (valid-row? r3)
          (valid-row? r4)
          (valid-row? r5))]
    [else  #f]))

(module+ test
  (check-equal? (valid-board? (xml->xexpr (read-xml/element (open-input-string in1)))) #t)
  (check-equal? (valid-board? (xml->xexpr (read-xml/element (open-input-string in2)))) #f)
  )

;; -------------------------------------------------------------------------
;; valid-row0? : string -> boolean
;; is r a valid row0 ( 5 cells)

(define (valid-row0? r)
  (match r
    [`(row() ,l0 ,l1 ,l2 ,l3 ,l4)
     (and (valid-cell? l0)
          (valid-cell? l1)
          (valid-cell? l2)
          (valid-cell? l3)
          (valid-cell? l4))]
    [else #f]))

(module+ test
  (check-equal? (valid-row0? (xml->xexpr (read-xml/element (open-input-string in3)))) #t)
  (check-equal? (valid-row0? (xml->xexpr (read-xml/element (open-input-string in4)))) #f))

;; -------------------------------------------------------------------------
;; valid-row? : string -> boolean
;; is r a valid row (6 cells)

(define (valid-row? r)
  (match r
    [`(row() ,l0 ,l1 ,l2 ,l3 ,l4 ,l5)
     (and (valid-cell? l0)
          (valid-cell? l1)
          (valid-cell? l2)
          (valid-cell? l3)
          (valid-cell? l4)
          (valid-cell? l5))]
    [else #f]))

(module+ test
  (check-equal? (valid-row? (xml->xexpr (read-xml/element (open-input-string in5)))) #t)
  (check-equal? (valid-row? (xml->xexpr (read-xml/element (open-input-string in6)))) #f))

;; -------------------------------------------------------------------------
;; valid-current? : string -> boolean
;; is c a valid current tile (1 input tile)

(define (valid-current? c)
  (match c
    [`(current() ,t) (valid-input-tile? t)]
    [else #f]))

(module+ test
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in7)))) #t)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in8)))) #f)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in9)))) #f)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in10)))) #f)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in11)))) #f))

;; -------------------------------------------------------------------------
;; valid-store? string -> boolean
;; is s a valid storehouse tile?

(define (valid-store? s)
  (match s
    [`(storehouse() ,t) (valid-store-tile? t)]
    [else #f]))

(module+ test
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in12)))) #t)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in13)))) #f)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in14)))) #f)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in15)))) #f)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in16)))) #f))

;; -------------------------------------------------------------------------
;; valid-cell? string -> boolean
;; is c a valid cell?

(define (valid-cell? c)
  (match c
    [`(cell() ,t) (valid-board-tile? t)]
    [else #f]))

;; -------------------------------------------------------------------------
;; valid-input-tile? string -> boolean
;; is v a valid input tile?

(define (valid-input-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "grass" "bush" "tree" "hut"
                         "crystal" "imperial-robot"))
     (and (member f vlist) #t)]
    [else #f]))

;; -------------------------------------------------------------------------
;; valid-store-tile? string -> boolean
;; is v a valid storehouse tile?

(define (valid-store-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "none" "grass" "bush" "tree" "hut"
                         "crystal" "imperial-robot"))
     (and (member f vlist) #t)]
    [else #f]))

;; -------------------------------------------------------------------------
;; valid-board-tile? string -> boolean
;; is v a valid board tile?

(define (valid-board-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "blank" "grass" "bush" "tree" "hut"
                         "house" "mansion" "castle" 
                         "floating-castle" "triple-castle"))
     (and (member f vlist) #t)]
    [else #f]))