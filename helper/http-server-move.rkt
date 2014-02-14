#lang racket

(require web-server/servlet
         web-server/servlet-env
         xml)

(module+ test
  (require rackunit))

(define-struct tile (v x y) #:transparent)

;; ---------------------------------------------------

(define (populate-board l n)
  (for/list ([i (range n)]
             [x l])
    list
    (for/list ([j (range n)]
               [y x])
      list (tile y i j))))

;; ---------------------------------------------------
;; populate-board: string string string -> (list board symbol)
;;  convert xml to list-of-list-of-tiles
;;  

(define (build-board e:str)
  (define e:xml (read-xml/element (open-input-string e:str)))
  (define e:xexpr (xml->xexpr e:xml))
  (let valid-xml? ((e e:xexpr))
    (match e
      [`(game () ,b ,c ,s)
       (match s
         [`(storehouse () (tile ((value ,v))))
          (match b
            [`(board () ,r0 ,r1 ,r2 ,r3 ,r4 ,r5)
             (define board 
               (list (build-row0 r0 v)
                     (build-row r1)
                     (build-row r2)
                     (build-row r3)
                     (build-row r4)
                     (build-row r5)))
             (match c
               [`(current() (tile ((value ,t))))
                (list board (string->symbol t))])])])])))

;; build-row0 : string symbol -> list of tiles
;; Build a list of tiles from xml string and storehouse value 'v'

(define (build-row0 r v)
  (match r
    [`(row () 
           (cell () (tile ((value ,v0))))
           (cell () (tile ((value ,v1))))
           (cell () (tile ((value ,v2))))
           (cell () (tile ((value ,v3))))
           (cell () (tile ((value ,v4)))))
     (list (string->symbol v)
           (string->symbol v0)
           (string->symbol v1)
           (string->symbol v2)
           (string->symbol v3)
           (string->symbol v4))]))

(define (build-row r)
  (match r
    [`(row () 
           (cell () (tile ((value ,v0))))
           (cell () (tile ((value ,v1))))
           (cell () (tile ((value ,v2))))
           (cell () (tile ((value ,v3))))
           (cell () (tile ((value ,v4))))
           (cell () (tile ((value ,v5)))))
     (list (string->symbol v0)
           (string->symbol v1)
           (string->symbol v2)
           (string->symbol v3)
           (string->symbol v4)
           (string->symbol v5))]))


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

;; String -> boolean
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

(define (valid-board? b)
  (match b
    [`(board() ,r0 ,r1 ,r2 ,r3 ,r4 ,r5) 
     (and (valid-row? r0)
          (valid-rows? r1)
          (valid-rows? r2)
          (valid-rows? r3)
          (valid-rows? r4)
          (valid-rows? r5))]
    [else  #f]))


(define (valid-row? r)
  (match r
    [`(row() ,l0 ,l1 ,l2 ,l3 ,l4)
     (and (valid-cell? l0)
          (valid-cell? l1)
          (valid-cell? l2)
          (valid-cell? l3)
          (valid-cell? l4))]
    [else #f]))

(define (valid-rows? r)
  (match r
    [`(row() ,l0 ,l1 ,l2 ,l3 ,l4 ,l5)
     (and (valid-cell? l0)
          (valid-cell? l1)
          (valid-cell? l2)
          (valid-cell? l3)
          (valid-cell? l4)
          (valid-cell? l5))]
    [else #f]))

(define (valid-current? c)
  (match c
    [`(current() ,t) (valid-inp? t)]
    [else #f]))

(define (valid-inp? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "grass" "bush" "tree" "hut"
                         "crystal" "imperial-bot"))
     (and (member f vlist) #t)]
    [else #f]))

(define (valid-store? s)
  (match s
    [`(storehouse() ,t) (valid-inp? t)]
    [else #f]))

(define (valid-cell? c)
  (match c
    [`(cell() ,t) (valid-tile? t)]
    [else #f]))

(define (valid-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "blank" "grass" "bush" "tree" "hut"
                         "house" "mansion" "castle" 
                         "floating-castle" "triple-castle"))
     (and (member f vlist) #t)]
    [else #f]))

;display-board-row : list-of-tiles -> list-of-tiles
; Displays a row of tiles

(define (display-board-row row)
  (cond
    [(empty? row) empty]
    [(cons? row)
     (let ([s (symbol->string (tile-v (first row)))])
       (printf "~a" s)
       (for ([i (range (- 15 (string-length s)))])
         (printf "~a" " "))
       (printf "|"))
     (display-board-row (rest row))]))

;display-board : board -> board
; Displays a board of tiles

(define (display-board board)
  (cond
    [(empty? board) empty]
    [(cons? board)
     (display-board-row (first board))
     (printf "\n")
     (display-board (rest board))]))

(define (variant-server req)
  (response/xexpr
   (cond
     [(and (equal? (request-method req) #"POST")
           (request-post-data/raw req))
      (define e:str (bytes->string/utf-8 (request-post-data/raw req)))
      (cond 
        [(false? (valid-xml? e:str)) 
         `(move ((value "error-invalid-tree")))]
        [else 
         (define l (build-board e:str))
         (define b (populate-board (first l) (length (first l))))
         (printf "\ncurr ~a\n" (rest l))
         (display-board b)
         (printf "\n**********\n")
         `(move ((value "valid")))])]
      [else `(move ((value "error-not-method-post")))])))

(serve/servlet variant-server #:port 8080
              #:servlet-path "/move")

(define move-res-1
  `(place (row ((value "2")))
          (column ((value "4")))))

(define move-res-2
  `(store))

(define move-res-3
  `(collect (row ((value "3")))
            (column ((value "5")))))
