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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Basic
;3 Grass	                        Bush        20
;3 Bushes	                        Tree        80
;3 Trees	                        Hut        320
;3 Huts	                        House     1280
;3 Houses                          Mansion   5120
;3 Mansions                        Castle   20480
;3 Castles                         FCastl   81920
;4 FCastles                        TCas    655360
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Place Input
;1 Grass                            Grass       5
;1 Bush	                         Bush	    20
;1 Tree                             Tree       80
;1 Hut	                         Hut       320
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Crystal
;2 Grass, 1 Crystal                 Bush       20
;2 Bushes, 1 Crystal                Tree       80
;2 Trees, 1 Crystal                 Hut       320
;2 Huts,  1 Crystal                 House    1280
;2 Houses, 1 Crystal                Mansion  5120
;2 Mansions, 1 Crystal              Castle  20480
;2 Castles, 1 Crystal               FCastl  81920
;3 FCastles, 1 Crystal              TCas   655360
;1 Crystal                          Blank       0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Points Breakdown - Imperial Bot
;1 bot, 1 grass                     Blank      -5
;1 bot, 1 bush                      Blank     -20
;1 bot, 1 tree                      blank     -80
;1 bot, 1 hut                       Blank    -320
;1 bot, 1 house                     Blank   -1280
;1 bot, 1 Mansions                  Blank   -5120
;1 bot, 1 Castles                   Blank  -20480
;1 bot, 1 FCastles                  Blank  -81920
;1 bot, 1 Triple-Castle             Blank -655360 
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
;Points Breakdown - Adjacency
;2 Grass                                       10
;2 Bush                                        50

;; --------------------------------------------------------------------
;; collapse-points : symbol num -> num
;;  a table of points obtained when 'n' of 'v' tiles are collapsed

(define (collapse-points v n)
  ;(printf "collapse-points - ~a, ~a\n" v n)
  (cond
    [(equal? n 0) 0]
    [(equal? n 1) 
     (case v
       ['grass 5]
       ['bush 20]
       ['tree 80]
       ['hut 320]
       ['house 1280]
       ['mansion 5120]
       ['castle 20480]
       ['floating-castle 81920]
       ['triple-castle 655360]
       [else (error "collapse-points1")])]
    [(equal? n 2)
     (* 2 (collapse-points v 1))]
    [(equal? n 3)
     (* 4 (collapse-points v 1))]
    [(>= n 4)
     (* 8 (collapse-points v 1))]
    [else (error "collapse-main")]))

(module+ test
  (check-equal? (collapse-points 'triple-castle 0) 0)
  (check-equal? (collapse-points 'tree 1) 80)
  (check-equal? (collapse-points 'house 2) 2560)
  (check-equal? (collapse-points 'floating-castle 3) 327680)
  (check-equal? (collapse-points 'bush 4) 160))

;; --------------------------------------------------------------------
;; is-collapsable? : board num num symbol num -> 
;;                       (list boolean [board-or-#f] num num symbol)
;;  Checks if a board is collapsable and returns collapsed board or #f

(define (collapsable? b x y v p)
  (define c (cadr (count-neighbours b x y v empty)))
  (if (and (not (symbol=? v 'triple-castle))
           (or (and (symbol=? v 'floating-castle) (> c 3))
               (and (not (symbol=? v 'floating-castle)) (> c 2))))
      (list (replace (car (replace-neighbours b x y v '())) x y (next-tile v))
            c
            (+ p -20 20 (collapse-points v c))
            v
            )
      (list #f c (+ p (collapse-points v (sub1 c))) v)))

(module+ test
  (check-equal? (collapsable? b2 1 2 'bush 8) (list #f 1 8 'bush))
  (check-equal? (collapsable? b4 1 2 'bush 0) 
                (list
                 (list
                  (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0) (tile 'blank 3 0))
                  (list (tile 'tree 0 1) (tile 'blank 1 1) (tile 'grass 2 1) (tile 'blank 3 1))
                  (list (tile 'blank 0 2) (tile 'tree 1 2) (tile 'blank 2 2) (tile 'blank 3 2))
                  (list (tile 'tree 0 3) (tile 'blank 1 3) (tile 'grass 2 3) (tile 'blank 3 3)))
                 3
                 80
                 'bush)))

;; --------------------------------------------------------------------
; points-at* : board num num sybmol -> num
;  Collapse board multiple times until no more collapses are possible, 
;  and returns points earned

(define (points-at* b x y v p)
  (define bb (replace b x y v))
  ;(define pp (+ p (collapse-points v 1)))
  (let loop ([l (collapsable? bb x y v p)])
    (let* ([b1 (first  l)]
           [c1 (second l)]
           [p1 (third  l)]
           [v1 (fourth l)])
      (if (false? b1)
          (+ p1 (collapse-points v 1))
          ;(loop (collapsable? b1 x y v1 (+ p1 (collapse-points v1 c1)) (λ (x) (next-tile x))))))))
          (loop (collapsable? b1 x y (next-tile v1) p1))))))

(module+ test
  (check-equal? (points-at* b2 0 1 'grass 0) 5)
  (check-equal? (points-at* b2 0 1 'hut 51) 691)
  (check-equal? (points-at* b2 2 2 'bush -100) -60)
  (check-equal? (points-at* t4 0 2 'grass 0) 25)
  (check-equal? (points-at* t4 1 0 'grass 0) 105))

;; --------------------------------------------------------------------
; swap-store-house-points : board symbol -> num
;  Returns the points from storehouse swap

(define (swap-store-house-points b v p)
  (define st (tile-v (car (car b))))
  ;(printf "Swap ~a, ~a(v)\n" st v)
  (if (symbol=? st v)
      -inf.0
      (+ p (- (hash-ref storehouse-points-hash v) 
              (hash-ref storehouse-points-hash st)))))

(module+ test
  (check-equal? (swap-store-house-points b9 'grass 0) -inf.0)
  (check-equal? (swap-store-house-points t3 'imperial-robot 0) 1600)
  (check-equal? (swap-store-house-points b1 'crystal -100) 14145))

;; --------------------------------------------------------------------
; collapse-crystal : board num num -> num
;  Places crystal at ('x','y') and s 'b'

(define (crystal-collapse-points b x y p)
  (let ([v (for/first ([i crystal-list]
                       #:when (> (cadr (count-neighbours 
                                        (replace b x y i) x y i empty))
                                 2)) i)])
    (if (false? v)
        p
        (points-at* b x y v p))))

(module+ test
  (check-equal? (crystal-collapse-points t4 0 2 0) 25)
  (check-equal? (crystal-collapse-points t4 1 0 0) 100)
  (check-equal? (crystal-collapse-points b2 1 0 0) 0))

;; --------------------------------------------------------------------
; decide-move : board num num symbol num -> num
;  Decide whether move is store-house, imperial-robot, crystal

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
            p)])]))

;; --------------------------------------------------------------------
;; greater/ptile : ptile ptile -> ptile
;; Returns ptile with higher points

(define (greater/ptile x y)
  (if (>= (ptile-p x) (ptile-p y))
      x y))

(module+ test
  (check-equal? (greater/ptile (ptile -inf.0 0 0) (ptile 2 0 0))
                (ptile 2 0 0))
  (check-equal? (greater/ptile (ptile 3 1 2) (ptile 1 2 3))
                (ptile 3 1 2)))


;; --------------------------------------------------------------------
;; find-max : board (ptile ...) (ptile ptile -> ptile) -> ptile
;;  Folds board to find the tile with maximum points

(define (find-max b ACCUM-ZERO ACCUM-MAX?)
  (for/fold ([max-x ACCUM-ZERO])
    ([x b])
    (ACCUM-MAX? max-x 
                (for/fold ([max-y ACCUM-ZERO])
                  ([y x])
                  (ACCUM-MAX? max-y y)))))

(module+ test
  (check-equal? (find-max
                 (list (list (ptile 21 0 0) (ptile -17 1 0))
                       (list (ptile -inf.0 0 1) (ptile +inf.0 1 1)))
                 (ptile -inf.0 0 0)
                 greater/ptile)
                (ptile +inf.0 1 1))
  (check-equal? (find-max
                 (list (list 1 2 3 4)
                       (list 9 8 7 6)
                       (list 2 2 11 -11))
                 0 (λ (x y) (if (> x y) x y)))
                11))

;; --------------------------------------------------------------------
;; generate-points-board : board symbol num -> board
;;  Plays move 'v' on every tile of 'b', collapses and returns a board
;;  of points

(define (generate-points-board b v n)
  (for/list ([i (range n)])
    (for/list ([j (range n)]) 
      (ptile (decide-move-points b j i v 0 n) j i))))

;; --------------------------------------------------------------------
;; choose-move : board symbol num -> (values num num)
;;  Returns an optimal ('x','y') pair to place 'v' on 'b'

(define (choose-move b v n)
  ;(define st (tile-v (car (car b))))
  (define ptable (generate-points-board b v n))
  ;(display-board/any ptable)
  (define max-pt (find-max ptable (ptile -inf.0 0 0) greater/ptile))
  ;(printf "\nMax - ~a\n" max-pt)
  (values (ptile-x max-pt) (ptile-y max-pt)))

;; ---------------------------------------------------
;; populate-board : list num -> board
;; Populates nxn board with values in list b

(define (populate-board b n)
  (for/list ([i (range n)]
             [x b])
    (for/list ([j (range n)]
               [y x])
      (tile y j i))))

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
           (string->symbol v4))]
    [_ (raise (make-my-exception 
               "failed"
               (current-continuation-marks)))]))

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
           (string->symbol v5))]
    [_ (raise (make-my-exception 
               "failed"
               (current-continuation-marks)))]))

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
  (define e:xexpr (xml->xexpr (read-xml/element (open-input-string e:str))))
  (match e:xexpr
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
              (list board (string->symbol t))]
             [_ (raise (make-my-exception 
                        "failed"
                        (current-continuation-marks)))])]
          [_ (raise (make-my-exception 
                     "failed"
                     (current-continuation-marks)))])]
       [_ (raise (make-my-exception 
                  "failed"
                  (current-continuation-marks)))])]
    [_ (raise (make-my-exception 
               "failed"
               (current-continuation-marks)))]))

(module+ test
  (check-equal? (build-board r1)
                '(((blank blank blank hut blank blank)
                   (tree blank blank blank blank blank)
                   (blank blank blank blank mansion blank)
                   (blank blank blank house blank blank)
                   (blank blank blank grass blank bush)
                   (blank blank blank blank tree blank))
                  crystal)))

;; --------------------------------------------------------------------
;; respons/move : HTTP req -> X-expr
;;  Generates appropriate X-expr for given input

(define (response/move e:str)
  ;(printf "~a\n" e:str)
  (define l 
    (with-handlers ([exn:fail? (λ (v) `(error ((value "invalid-tree"))))])
      (build-board e:str)))
  ;(printf "l = \n~a\n" l)
  (cond 
    [(symbol? (first l)) l]
    [else 
     (define f (first l))
     (define n 6)
     (define b (populate-board f n))
     (define v (cadr l))
     ;(printf "\nNew Request: Current = ~a\n" v)
     ;(printf "*********************************\n")
     ;(display-board/any b)
     (define-values (x y) (choose-move b v n))
     ;(printf "Response - (~a, ~a)\n" x y)
     ;(printf "*********************************\n")
     (cond
       [(and (equal? x 0) (equal? y 0)) `(store ())]
       [else
        `(place (row ((value ,(number->string y))))
                (column ((value ,(number->string x)))))])]))

;; --------------------------------------------------------------------
;; move-server : http request -> http response
;; a server that accepts HTTP requests containing a payload (if any) in XML 
;; format and that responds with in XML format. 
;; There are two kinds of queries to the server - variant and move

(define (server/player req)
  (response/xexpr
   (cond 
     ; if not POST method or POST data is empty
     [(and (equal? (request-method req) #"POST") 
           (string=? (url->string (request-uri req)) "/move"))
      (response/move (bytes->string/utf-8 (request-post-data/raw req)))]
     [(and (equal? (request-method req) #"GET") 
           (string=? (url->string (request-uri req)) "/variant"))
      (response/variant)]
     [else `(error ((value "invalid-method-or-path")))])))

;; --------------------------------------------------------------------
;; response/variant : HTTP req -> X-expr
;;  Generates appropriate X-expr for given input

(define (response/variant)
  `(variant ((value "basic"))))

;; --------------------------------------------------------------------

(define (client/post h p u d)
  (define hc (http-conn-open h #:port p))
  (define-values (_ __ port) (http-conn-sendrecv! hc u
                                                  #:method #"POST" #:data d))
  (port->string port))

;; --------------------------------------------------------------------

(define (client/get h p u)
  (define hc (http-conn-open h #:port p))
  (define-values (_ __ port) (http-conn-sendrecv! hc u
                                                  #:method #"GET"))
  (port->string port))