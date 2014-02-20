; Srikanth Manikarnike
; U0706564

#lang racket
(require web-server/servlet
         xml
         net/http-client)

(require "basic-moves-definitions.rkt"
         "basic-moves-functions.rkt"
         "basic-player-definitions.rkt"
         "basic-player-helpers.rkt")

(provide (all-defined-out))

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
               (list (build-row0 r0 (if (string=? v "none") "blank" v))
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

(module+ test
  (check-equal? (valid-xml? (bcs->xml b9 "grass" "none"))
                #t)
  (check-equal? (valid-xml? (bcs->xml b1 "grass" "none"))
                #f))

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
  (define in1 "<board><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"mansion\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"house\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"grass\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"bush\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row></board>")
  (define in2 "<board><row><cell><tile value =\"blank\"></tile></cell></row><row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row></board>")
  (check-equal? (valid-board? (xml->xexpr (read-xml/element (open-input-string in1)))) #t)
  (check-equal? (valid-board? (xml->xexpr (read-xml/element (open-input-string in2)))) #f)
  )

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
  (define in3 "<row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row>")
  (define in4 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (check-equal? (valid-row0? (xml->xexpr (read-xml/element (open-input-string in3)))) #t)
  (check-equal? (valid-row0? (xml->xexpr (read-xml/element (open-input-string in4)))) #f))

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
  (define in5 "<row><cell><tile value =\"castle\"></tile></cell><cell><tile value =\"castle\"></tile></cell><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"mansion\"></tile></cell><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"blank\"></tile></cell></row>")
  (define in6 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (check-equal? (valid-row? (xml->xexpr (read-xml/element (open-input-string in5)))) #t)
  (check-equal? (valid-row? (xml->xexpr (read-xml/element (open-input-string in6)))) #f))

;; valid-current? : string -> boolean
;; is c a valid current tile (1 input tile)

(define (valid-current? c)
  (match c
    [`(current() ,t) (valid-input-tile? t)]
    [else #f]))

(module+ test
  (define in7 "<current><tile value =\"crystal\"></tile></current>")
  (define in8 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (define in9 "<current><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></current>")
  (define in10 "<current><cell><tile value =\"blank\"></tile></cell></current>")
  (define in11 "<current value =\"blank\"></current>")
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in7)))) #t)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in8)))) #f)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in9)))) #f)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in10)))) #f)
  (check-equal? (valid-current? (xml->xexpr (read-xml/element (open-input-string in11)))) #f))

;; valid-store? string -> boolean
;; is s a valid storehouse tile?

(define (valid-store? s)
  (match s
    [`(storehouse() ,t) (valid-store-tile? t)]
    [else #f]))

(module+ test
  (define in12 "<storehouse><tile value =\"crystal\"></tile></storehouse>")
  (define in13 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (define in14 "<storehouse><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></storehouse>")
  (define in15 "<storehouse><cell><tile value =\"blank\"></tile></cell></storehouse>")
  (define in16 "<storehouse value =\"blank\"></storehouse>")
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in12)))) #t)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in13)))) #f)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in14)))) #f)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in15)))) #f)
  (check-equal? (valid-store? (xml->xexpr (read-xml/element (open-input-string in16)))) #f))

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
                         "crystal" "imperial-robot"))
     (and (member f vlist) #t)]
    [else #f]))

;; valid-store-tile? string -> boolean
;; is v a valid storehouse tile?

(define (valid-store-tile? v)
  (match v
    [`(tile ((value ,f)))
     (define vlist (list "none" "grass" "bush" "tree" "hut"
                         "crystal" "imperial-robot"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Basic
;3 Grass	                        Bush        20
;3 Bushes	                        Tree       100
;3 Trees	                        Hut        500
;3 Huts	                        House     2000
;3 Houses                          Mansion   5000
;3 Mansions                        Castle   20000
;3 Castles                         FCastl  100000
;4 FCastles                        TCas   1000000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Place Input
;1 Grass                            Grass       5
;1 Bush	                         Bush	    20
;1 Tree                             Tree      100
;1 Hut	                         Hut       500
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Crystal
;2 Grass, 1 Crystal                 Bush       20
;2 Bushes, 1 Crystal                Tree      100
;2 Trees, 1 Crystal                 Hut       500
;2 Huts,  1 Crystal                 House    2000
;2 Houses, 1 Crystal                Mansion  5000
;2 Mansions, 1 Crystal              Castle  20000
;2 Castles, 1 Crystal               FCastl 100000
;3 FCastles, 1 Crystal              TCas  1000000
;1 Crystal                          Blank       0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Imperial Bot
;1 bot, 1 grass                     Blank      -5
;1 bot, 1 bush                      Blank     -20
;1 bot, 1 tree                      blank    -100
;1 bot, 1 hut                       Blank    -500
;1 bot, 1 house                     Blank   -2000
;1 bot, 1 Mansions                  Blank   -5000
;1 bot, 1 Castles                   Blank  -20000
;1 bot, 1 FCastles                  Blank -100000
;1 bot, 1 Triple-Castle             Blank-1000000 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Storehouse
;1 Grass                                       25
;1 Bush                                       120
;1 Tree                                       600
;1 Hut                                       2500
;1 Crystal                                  10000
;1 bot                                       5000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Proximity
;2 Grass                                       10
;2 Bush                                        50


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Advanced - TBD
;4 Grass	                         Bush*      40
;5 Grass	                         Bush*	    45
;3 Grass, 2 Bushes	                 Tree	   125
;5 Grass, 2 Bushes	                 Tree      145
;2 Bushes,1 Bush*	                 Tree      120
;3 Grass, 2 Bushes*	         Tree	   225
;2 Bushes, 2 Trees	                 Hut       620
;3 Grass, 1 Bush*, 1 Bush, 2 Trees  Hut	   625
;4 Trees                            Hut*     1100
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; collapse-points : symbol num -> num
;;  a table of points obtained when 'n' of 'v' tiles are collapsed

(define (collapse-points v n)
  ;(printf "collapse-points - ~a, ~a\n" v n)
  (cond
    [(>= n 4)
     (case v
       ['grass 40]
       ['bush 200]
       ['tree 1000]
       ['hut 4000]
       ['house 10000]
       ['mansion 40000]
       ['castle 200000]
       ['floating-castle 2000000]
       [else (error "collapse-points4")])]
    [(equal? n 3) 
     (case v
       ['grass 20]
       ['bush 100]
       ['tree 500]
       ['hut 2000]
       ['house 5000]
       ['mansion 20000]
       ['castle 100000]
       ['floating-castle 1000000]
       [else (error "collapse-points3")])]
    ;    [(equal? n 3) 
    ;     (case v
    ;       ['grass 5]
    ;       ['bush 20]
    ;       ['tree 100]
    ;       ['hut 500]
    ;       ['house 2000]
    ;       ['mansion 5000]
    ;       ['castle 20000]
    ;       ['floating-castle 100000]
    ;       ['triple-castle 600000]
    ;       [else (error "collapse-points3")])]
    [(equal? n 2)
     (case v
       [(grass) 10]
       [(bush) 50]
       [(tree) 250]
       [(hut) 1000]
       [(house) 2500]
       [(mansion) 10000]
       [(castle) 50000]
       [(floating-castle) 300000]
       [else (error "collapse-points2")])]
    [(equal? n 1) 
     (case v
       ['grass 5]
       ['bush 20]
       ['tree 100]
       ['hut 500]
       ['house 2000]
       ['mansion 5000]
       ['castle 20000]
       ['floating-castle 100000]
       ['triple-castle 1000000]
       [else (error "collapse-points1")])]
    [(equal? n 0) 0]
    [else (error "collapse-points-main")]))

; points-at* : board num num sybmol -> num
;  Collapse board multiple times until no more collapses are possible, 
;  and returns points earned

(define (points-at* b x y v p)
  (let ([bb (replace b x y v)]) 
    (let loop1 ([board bb]
                [point p]
                [value v]
                [count (cadr (count-neighbours bb x y v empty))])      
      (if (or (and (symbol=? value 'floating-castle) (> count 3))
              (and (not (symbol=? value 'floating-castle)) (> count 2)))
          (let* ([b1 (replace (car (replace-neighbours board x y value empty))
                               x y (next-tile value))]
             [p1 (+ point (collapse-points v count))])
            (let * ([c1 (cadr (count-neighbours b1 x y (next-tile value) empty))]
                    [v1 (next-tile value)])
              (loop1 b1 p1 v1 c1)))
          (+ point (collapse-points v count))))))

;; replace-points : board num num val num -> num
;;  Replaces items and returns score from replacement

(define (replace-points b x y v p n)
  (values (replace b x y v) (+ p (collapse-points v n))))

; swap-store-house : board symbol -> (values board symbol)
;  Swaps the store house with tile with 'v'

;; As of now, storehouse is not used as a strategy. It is only used as
;; a last option to store good tiles for later use. So the there is no
;; score change. And, the state of the board should not be stored by
;; the player. So returning just p.

(define (swap-store-house-points b v p)
  (define st (tile-v (car (car b))))
  ;(printf "Swap ~a, ~a(v)\n" st v)
  (if (symbol=? st v)
      -inf.0
      (- (hash-ref storehouse-points-hash v) 
         (hash-ref storehouse-points-hash st))))

; collapse-crystal : board num num -> num
;  Places crystal at ('x','y') and s 'b'

(define (crystal-collapse-points b x y p)
  (let ([v (for/first ([i crystal-list]
                       #:when (> (cadr (count-neighbours 
                                        (replace b x y i) x y i empty))
                                 2)) i)])
    (if (false? v)
        ;(values b p)
        p
        (points-at* b x y v p))))

;; 

; decide-move : board num num symbol num -> num
;  Decide whether move is 
;   - store-house
;   - imperial-robot
;   - crystal
;   - others
;  and take corresponding action

(define (decide-move-points b x y v p n)
  (cond
    [(and (equal? x 0) (equal? y 0))
     (swap-store-house-points b v p)]
    
    [else
     (case (tile-v (get-tile b x y))
       [(blank) ; (x,y) is blank
        (cond
          ; imperial-robot
          [(symbol=? v 'imperial-robot)
           p]
          ; crystal
          [(symbol=? v 'crystal)
           (crystal-collapse-points b x y p)]
          [else 
           (points-at* b x y v p)])]
       ; everything else
       [else 
        (if (symbol=? v 'imperial-robot)
            (hash-ref imp-bot-points-hash (tile-v (get-tile b x y)))
            ;(- p (collapse-points (tile-v (get-tile b x y)) 1))
            ;            (let ([pp (- p (collapse-points (tile-v (get-tile b x y)) 1))])
            ;              (if (> pp 0) pp 0))
            p)])]))

(define (choose-move b v n)
  (define st (tile-v (car (car b))))
  (define ptable (generate-points-board b v n))
  ;(display-board/any ptable)
  (define max-pt (find-max ptable (ptile -inf.0 0 0) max-tiles?))
  ;(printf "\nMax - ~a\n" max-pt)
  (cond
    ; if storehouse is empty
    [(symbol=? st 'blank)
     (values (ptile-x max-pt) (ptile-y max-pt))]
    [else ;non empty store house
     (define stable (generate-points-board b st n))
     (display-board/any stable)
     (define max-st (find-max stable (ptile -inf.0 0 0) max-tiles?))
     (printf "\nMax - ~a\n" max-st)
     (cond 
       [(> (ptile-p max-st) (ptile-p max-pt))
        (values (ptile-x max-st) (ptile-y max-st))]
       [else
        (values (ptile-x max-pt) (ptile-y max-pt))])]))

;; generate-points-board : board symbol num -> board
;;  Plays move 'v' on every tile of 'b', collapses and returns a board
;;  of points

(define (generate-points-board b v n)
  (for/list ([i (range n)])
    list
    (for/list ([j (range n)])
      list (ptile (decide-move-points b j i v 0 n) j i))))

;; max-tiles? : ptile ptile -> ptile
;; Returns tile with higher points

(define (max-tiles? x y)
  (if (>= (ptile-p x) (ptile-p y))
      x y))

;; find-max : board (ptile ...) (ptile ptile -> ptile) -> ptile
;;  Folds board to find the tile with maximum points

(define (find-max b ACCUM-ZERO ACCUM-MAX?)
  (for/fold ([max-x ACCUM-ZERO])
    ([x b])
    (ACCUM-MAX? max-x 
                (for/fold ([max-y ACCUM-ZERO])
                  ([y x])
                  (ACCUM-MAX? max-y y)))))

;; genrate-xexpr-move-response : HTTP req -> X-expr
;;  Generates appropriate X-expr for given input

(define (generate-xexpr-move-response req)
  (cond
    ; if not POST method or POST data is empty
    [(false? (and (equal? (request-method req) #"POST")
                  (request-post-data/raw req)))
     `(move ((value "error-not-method-post")))]
    ; POST method with valid post data
    [else
     (define e:str (bytes->string/utf-8 (request-post-data/raw req)))
     (cond 
       ; Not valid XML Tree
       [(false? (valid-xml? e:str)) 
        `(move ((value "error-invalid-tree")))]
       ; Valid XML Tree
       [else
        (define l (build-board e:str))
        (define f (first l))
        (define n 6)
        (define b (populate-board f n))
        (define v (cadr l))
        (printf "\nNew Request: Current = ~a\n" v)
        (printf "*********************************\n")
        ;(display-board/any b)
        (define-values (x y) (choose-move b v n))
        (printf "Response - (~a, ~a)\n" x y)
        (printf "*********************************\n")
        ;(define pb (generate-points-board b v n))
        ;(display-board pb number->string/ptile)
        ;(define t (find-max pb (ptile 0 0 0) max-tiles?))
        ;(define x (ptile-x t))
        ;(define y (ptile-y t))
        (cond
          [(and (equal? x 0) (equal? y 0)) `(store ())]
          [else
           `(place (row ((value ,(number->string y))))
                   (column ((value ,(number->string x)))))])])]))

;; --------------------------------------------------------------------
;; move-server : http request -> http response
;; a server that accepts HTTP requests containing a payload (if any) in XML 
;; format and that responds with in XML format. 
;; There are two kinds of queries to the server - variant and move

(define (move-server req)
  (response/xexpr (generate-xexpr-move-response req)))

;; genrate-xexpr-variant-response : HTTP req -> X-expr
;;  Generates appropriate X-expr for given input

(define (generate-xexpr-variant-response req)
  (cond
    ; if not method GET
    [(false? (equal? (request-method req) #"GET"))
     `(variant ((value "error")))]
    [else `(variant ((value "basic")))])) 

;; --------------------------------------------------------------------
;; variant-server : http request -> http response
;; a server that accepts HTTP requests containing a payload (if any) in XML 
;; format and that responds with in XML format. 
;; There are two kinds of queries to the server - variant and move
(define (variant-server req)
  (response/xexpr (generate-xexpr-variant-response req)))

(define (client h p m u d)
  (define hc (http-conn-open h #:port p))
  (define-values (_ __ port) (http-conn-sendrecv! hc u
                                                  #:method m #:data d))
  (port->string port))

;(display-board/any b9)
;(choose-move b9 'grass 6)