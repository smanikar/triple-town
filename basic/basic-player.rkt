; Srikanth Manikarnike
; U0706564

#lang racket
(require web-server/servlet
         web-server/servlet-env
         xml)

(require "basic.rkt")

(module+ test
  (require rackunit))

;; ---------------------------------------------------
;; populate-board : list num -> board
;; Populates an nxn board with values in list b

(define (populate-board b n)
  (for/list ([i (range n)]
             [x b])
    list
    (for/list ([j (range n)]
               [y x])
      list (tile y j i))))

(module+ test
  (check-equal? (populate-board  (list (list 'hut 'hut 'grass)
                                       (list 'blank 'blank 'grass)
                                       (list 'tree 'bush 'tree)) 3)
                (list
                 (list (tile 'hut 0 0) (tile 'hut 1 0) (tile 'grass 2 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) (tile 'grass 2 1))
                 (list (tile 'tree 0 2) (tile 'bush 1 2) (tile 'tree 2 2))))
  (check-equal? (populate-board (list (list 'none 'blank)
                                      (list 'blank 'none)) 2)
                (list (list (tile 'none 0 0) (tile 'blank 1 0)) 
                      (list (tile 'blank 0 1) (tile 'none 1 1)))))

;; --------------------------------------------------------------
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

(module+ test
  (check-equal? (build-row0 
                 '(row () 
                       (cell () (tile ((value "tree"))))
                       (cell () (tile ((value "hut"))))
                       (cell () (tile ((value "blank"))))
                       (cell () (tile ((value "blank"))))
                       (cell () (tile ((value "bush")))))
                 "crystal")
                '(crystal tree hut blank blank bush)))

;; --------------------------------------------------------------
;; build-row0 : string symbol -> list of tiles
;; Build a list of tiles from xml string and storehouse value 'v'

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

(module+ test
  (check-equal? (build-row
                 '(row ()
                       (cell () (tile ((value "mansion"))))
                       (cell () (tile ((value "tree"))))
                       (cell () (tile ((value "hut"))))
                       (cell () (tile ((value "blank"))))
                       (cell () (tile ((value "blank"))))
                       (cell () (tile ((value "bush"))))))
                '(mansion tree hut blank blank bush)))
;; ---------------------------------------------------
;; populate-board: string string string -> (list board symbol)
;;  convert xml to list-of-list-of-tiles

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

(define r1
  "<game><board><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"mansion\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"house\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"grass\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"bush\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row></board><current><tile value=\"crystal\"></tile></current><storehouse><tile value=\"blank\"></tile></storehouse></game>")

(module+ test
  (check-equal? (build-board r1)
                '(((blank blank blank hut blank blank)
                   (tree blank blank blank blank blank)
                   (blank blank blank blank mansion blank)
                   (blank blank blank house blank blank)
                   (blank blank blank grass blank bush)
                   (blank blank blank blank tree blank))
                  crystal)))

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

;; valid-current? : string -> boolean
;; is c a valid current tile (1 input tile)

(define (valid-current? c)
  (match c
    [`(current() ,t) (valid-input-tile? t)]
    [else #f]))

;; valid-store? string -> boolean
;; is s a valid storehouse tile?

(define (valid-store? s)
  (match s
    [`(storehouse() ,t) (valid-store-tile? t)]
    [else #f]))

;; valid-cell? string -> boolean
;; is c a valid cell?

(define (valid-cell? c)
  (match c
    [`(cell() ,t) (valid-board-tile? t)]
    [else #f]))

;; valid-input-tile? string -> boolean
;; is v a valid input tile?

(define (valid-input-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "grass" "bush" "tree" "hut"
                         "crystal" "imperial-bot"))
     (and (member f vlist) #t)]
    [else #f]))

;; valid-store-tile? string -> boolean
;; is v a valid storehouse tile?

(define (valid-store-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "blank" "grass" "bush" "tree" "hut"
                         "crystal" "imperial-bot"))
     (and (member f vlist) #t)]
    [else #f]))

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

;; --------------------------------------------------------------------

(define move-res-1
  `(place (row ((value "2")))
          (column ((value "4")))))

(define move-res-2
  `(store))

(define move-res-3
  `(collect (row ((value "3")))
            (column ((value "5")))))

(define (play-move b v)
  move-res-1)

;; --------------------------------------------------------------------
;; variant-server : http request -> http response
;; a server that accepts HTTP requests containing a payload (if any) in XML 
;; format and that responds with in XML format. 
;; There are two kinds of queries to the server - variant and move

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
         (printf "**********\n")
         (play-move b (rest l))
         ;`(move ((value "valid")))
         ])]
     [else `(move ((value "error-not-method-post")))])))

(serve/servlet variant-server #:port 8080
              #:servlet-path "/move")
;(serve/servlet variant-server #:port 8080
;              #:servlet-path "/variant")