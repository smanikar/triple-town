#lang racket
(require rackunit)
(require test-engine/racket-tests)

;next-tile : symbol -> symbol
; Gets the next tile of cur-tile
(define (next-tile cur-tile)
  (cond
    ;[(symbol=? cur-tile 'blank) 'grass]
    [(symbol=? cur-tile 'grass) 'bush]
    [(symbol=? cur-tile 'bush)  'tree]
    [(symbol=? cur-tile 'tree)  'hut]
    [(symbol=? cur-tile 'hut) 'house]
    [(symbol=? cur-tile 'house) 'mansion]
    [(symbol=? cur-tile 'mansion) 'castle]
    [(symbol=? cur-tile 'castle) 'floating-castle]
    [(symbol=? cur-tile 'floating-castle) 'triple-castle]
    [(symbol=? cur-tile 'tombstone) 'church]
    [(symbol=? cur-tile 'church)  'cathedral]
    [(symbol=? cur-tile 'cathedral)  'chest]
    [(symbol=? cur-tile 'chest) 'large-chest]
    [(symbol=? cur-tile 'large-chest) 'rock]
    [(symbol=? cur-tile 'rock) 'mountain]
    [else (error (format "not a tile: ~s~n" cur-tile))]))

(module+ test 
  (check-equal? (next-tile 'grass) 'bush)
  (check-equal? (next-tile 'bush)  'tree)
  (check-equal? (next-tile 'tree)  'hut)
  (check-equal? (next-tile 'hut) 'house)
  (check-equal? (next-tile 'house) 'mansion)
  (check-equal? (next-tile 'mansion) 'castle)
  (check-equal? (next-tile 'castle) 'floating-castle)
  (check-equal? (next-tile 'floating-castle) 'triple-castle)
  (check-equal? (next-tile 'tombstone) 'church)
  (check-equal? (next-tile 'church) 'cathedral)
  (check-equal? (next-tile 'cathedral) 'chest)
  (check-equal? (next-tile 'chest) 'large-chest)
  (check-equal? (next-tile 'large-chest) 'rock)
  (check-equal? (next-tile 'rock) 'mountain)
  (check-error (next-tile 'test) "not a tile: test")
  )

; A tile is (make-tile symbol num num)
(define-struct tile (v x y) #:transparent)

;A store house an tile to store
(define store-house empty)

;A list of all possible inputs
(define input-list (list 
                    'grass 
                    'bush 
                    'tree 
                    'hut 
                    'crystal 
                    'imperial-bot 
                    'bear 
                    'ninja-bear))

(define t1 (list (list (tile 'grass 0 0) (tile 'blank 1 0))
                 (list (tile 'blank 0 1) (tile 'grass 1 1))))

(define t2 (list (list (tile 'grass 0 0) (tile 'blank 1 0) (tile 'grass 2 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'blank 1 2) (tile 'grass 2 2))))

(define b1 (list (list (tile 'grass 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
                 (list (tile 'grass 0 1) (tile 'hut   1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'blank 1 2) (tile 'grass 2 2))))

(define b2 (list (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut   1 1) (tile 'blank 2 1))
                 (list (tile 'blank 0 2) (tile 'bush  1 2) (tile 'blank 2 2))))

(define b3 (list (list (tile 'bush  0 0) (tile 'grass 1 0) (tile 'grass 2 0))
                 (list (tile 'bush  0 1) (tile 'grass 1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'blank 1 2) (tile 'grass 2 2))))


;gen:input : none -> symbol
; Generates a symbol from 0 to 7 based on weightes

(define (gen-input)
  (let ([n (random 100)])
    (cond
      [(< n 61) (list-ref input-list 0)]
      [(< n 76) (list-ref input-list 1)]
      [(< n 78) (list-ref input-list 2)]
      [(< n 79) (list-ref input-list 3)]
      [(< n 81) (list-ref input-list 4)]
      [(< n 84) (list-ref input-list 5)]
      [(< n 99) (list-ref input-list 6)]
      [(< n 100) (list-ref input-list 7)]
      [else (error "random number")])))

;get-tile: boards num num -> tile-or-#f
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile b x y)
  (cond
    [(empty? b) #f]
    [(cons? b)
     (list-ref (list-ref b y) x)]))

(module+ test
  (check-equal? (get-tile empty 0 0) #f)
  (check-equal? (get-tile (list (list (tile 'blank 0 0) (tile 'grass 1 0))
                                (list (tile 'grass 0 1) (tile 'blank 1 1))) 
                          0 1) 
                (tile 'grass 0 1))
  (check-equal? (get-tile  (list (list (tile 'blank 0 0)))
                           0 0) 
                (tile 'blank 0 0)))

;on-board? : num num -> bool
; Checks if a given tile ('x','y') is on the 'board'

(define (on-board? x y size)
  (and (and (< x size) (> x -1)) 
       (and (< y size) (> y -1))))

(module+ test
  (check-equal? (on-board? 1 0 0) #f)
  (check-equal? (on-board? 1 0 (length t1)) #t)
  (check-equal? (on-board? 1 2 (length t1)) #f)
  (check-equal? (on-board? -1 0 (length t2)) #f)
  (check-equal? (on-board? 3 0 (length b2)) #f)
  (check-equal? (on-board? 2 2 (length b2)) #t)
  )

;valid-neighbour? : board num num symbol list -> boolean
; Checks if a given tile is a valid neighbour based on 'v'

(define (valid-neighbour? b x y v l)
  (and (on-board? x y (length b))
       (let ([t (get-tile b x y)])
         (and (symbol=? (tile-v t) v)
              (false? (member t l))))))

(module+ test
  (check-equal? (valid-neighbour? empty -1 -1 'test empty) #f)
  (check-equal? (valid-neighbour? t2    -1 -1 'test empty) #f)
  (check-equal? (valid-neighbour? empty  1  2 'test empty) #f)
  (check-equal? (valid-neighbour? t2 0 2 'grass empty) #t)
  (check-equal? (valid-neighbour? t2 0 2 'grass (list (tile 'grass 0 2))) #f))

; replace-row : list num val -> list
;  Replces one item in a list (with 0-based indexing)

(define (replace-row r x v)
  (cond
    [(empty? r) empty]
    [(cons? r) (if (= x 0)
                   (cons (tile v 
                               (tile-x (first r)) 
                               (tile-y (first r))) 
                         (rest r))
                   (cons (first r)
                         (replace-row (rest r) (- x 1) v)))]))

(module+ test 
  (check-equal? (replace-row empty -1 'test) empty)
  (check-equal? (replace-row (list (tile 'hut 0 0)) -1 'reddit)   
                (list (tile 'hut 0 0)))
  (check-equal? (replace-row (list (tile 'hut 0 0)) 2 'reddit)   
                (list (tile 'hut 0 0)))
  (check-equal? (replace-row (list (tile 'hut 0 0)) -1 'reddit)   
                (list (tile 'hut 0 0)))
  (check-equal? (replace-row (list (tile 'hut 0 0)) 0 'reddit)   
                (list (tile 'reddit 0 0)))
  (check-equal? (replace-row 
                 (list (tile 'grass 0 0) (tile 'grass 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 2 'grass)
                (list (tile 'grass 0 0) (tile 'grass 1 0) 
                      (tile 'grass 2 0) (tile 'blank 3 0))))


; replace : board num num val -> board
;  Replaces one item in the board

(define (replace b x y v)
  (cond
    [(empty? b) empty]
    [(cons? b) (if (= y 0)
                   (cons (replace-row (first b) x v)
                         (rest b))
                   (cons (first b) (replace (rest b) x (- y 1) v)))]))

(module+ test
  (check-equal? (replace empty -1 -1 'reddit) empty)
  (check-equal? (replace t1 0 1 'reddit)
                (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                      (list (tile 'reddit 0 1) (tile 'grass 1 1))))
  (check-equal? (replace b2 2 1 'random)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'random 2 1))
                 (list (tile 'blank 0 2) (tile 'bush 1 2) (tile 'blank 2 2)))))

;count-neighbours :  
; board num num symbol list -> (list [list-or-#f], num)

(define (count-neighbours b x y v l)
  (cond [(empty? b) (list #f 0)]
        [(cons? b)
         (if (valid-neighbour? b x y v l)
             (let* ([t (get-tile b x y)]
                    [l1 (count-neighbours b x (sub1 y) v (cons t l))]     ;north
                    [l2 (count-neighbours b x (add1 y) v (car l1))]       ;south
                    [l3 (count-neighbours b (add1 x) y v (car l2))]       ;east
                    [l4 (count-neighbours b (sub1 x) y v (car l3))])      ;west
               (list (car l4) (+ 1 (cadr l1) (cadr l2) (cadr l3) (cadr l4))))
             (list l 0))]))

(module+ test
  (check-equal? (count-neighbours empty -1 -1 'cons empty) (list #f 0))
  (check-equal? (count-neighbours t1 -1 -1 'cons empty) (list empty 0))
  (check-equal? (count-neighbours t1 1 1 'cons empty) (list empty 0))
  (check-equal? (count-neighbours 
                 t1 1 1 'grass (list (tile 'grass 1 1))) 
                (list (list (tile 'grass 1 1)) 0))
  (check-equal? (count-neighbours t1 1 1 'grass empty) 
                (list (list (tile 'grass 1 1)) 1))
  (check-equal? (count-neighbours b1 0 2 'grass empty) 
                (list (list (tile 'grass 1 0) (tile 'grass 0 0) 
                            (tile 'grass 0 1) (tile 'grass 0 2)) 4)))

;replace-neighbours :
; board num num symbol list -> (list boolean [board-or-#f], list)

(define (replace-neighbours b x y v l)
  (cond
    [(empty? b) (list #f l)]
    [(cons? b)
     (if (valid-neighbour? b x y v l)
         (let* ([t (get-tile b x y)]
                [b0 (replace b x y 'blank)]
                [l1 (replace-neighbours b0 x (sub1 y) v (cons t l))]       ;north
                [l2 (replace-neighbours (car l1) x (add1 y) v (cadr l1))]  ;south
                [l3 (replace-neighbours (car l2) (add1 x) y v (cadr l2))]  ;east
                [l4 (replace-neighbours (car l3) (sub1 x) y v (cadr l3))]) ;west
           l4)
         (list b l))]))

(module+ test
  (check-equal? (replace-neighbours empty -1 -1 'cons empty) (list #f empty))
  (check-equal? (replace-neighbours t1 -1 -1 'cons empty) (list t1 empty))
  (check-equal? (replace-neighbours t1 1 1 'cons empty) (list t1 empty))
  (check-equal? (replace-neighbours 
                 t1 1 1 'grass (list (tile 'grass 1 1))) 
                (list t1 (list (tile 'grass 1 1))))
  (check-equal? (replace-neighbours t1 1 1 'grass empty) 
                (list (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                            (list (tile 'blank 0 1) (tile 'blank 1 1))) 
                      (list (tile 'grass 1 1))))
  (check-equal? (replace-neighbours b1 0 2 'grass empty) 
                (list
                 (list
                  (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
                  (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'blank 2 1))
                  (list (tile 'blank 0 2) (tile 'blank 1 2) (tile 'grass 2 2)))
                 (list (tile 'grass 1 0) (tile 'grass 0 0) 
                       (tile 'grass 0 1) (tile 'grass 0 2)))))

; place-tile : board num num symbol -> boolean [board-or-#f]
;  Places on board and returns if place-tile was successful
(define (place-tile b x y v)
  (cond 
    [(empty? b) #f]
    [(cons? b)
     (and (on-board? x y (length b))
          (and (symbol=? (tile-v (get-tile b x y)) 'blank)
               (replace b x y v)))]))

(module+ test
  (check-equal? (place-tile empty -1 -1 'reddit) #f)
  (check-equal? (place-tile t1 -1 -1 'reddit) #f)
  (check-equal? (place-tile t1 1 1 'reddit) #f)
  (check-equal? (place-tile t1 0 1 'reddit)
                (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                      (list (tile 'reddit 0 1) (tile 'grass 1 1))))
  (check-equal? (place-tile b2 2 1 'random)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'random 2 1))
                 (list (tile 'blank 0 2) (tile 'bush 1 2) (tile 'blank 2 2)))))

; multi-collapse : board num num sybmol -> boolean [board-or-#f]
;  Try replace-neighbours as many times till it reurns #f

(define (multi-collapse b x y v)
  (cond
    [(empty? b) #f]
    [(cons? b)
     (let loop ([board b]
                [value v]
                [count (cadr (count-neighbours b x y v empty))])
       (if (> count 2)
           (let* ([b1 (replace (car (replace-neighbours board x y value empty))
                               x y (next-tile value))]
                  [c1 (cadr (count-neighbours b1 x y (next-tile value) empty))])
          (loop b1 (next-tile v) c1))
           board))]))

(define (collapse b x y v)
 (multi-collapse (place-tile b x y v)))


;  (let ([b1 (place-tile b x y v)])
;    (let ([res (count-neighbours b1 x y v empty)])
;      (if (> (cadr res) 2)
;          (replace (car (replace-neighbours b1 x y v empty)) 
;                   x y (next-tile v))
;          b1))))

; decide-move : board num num symbol -> boolean [board-or-#f]
;  Decide whether move is 
;   - store-house
;   - imperial-robot
;   - crystal
;   - select chest or large-chest 


;move : board -> boolean
; Displays board, reads new tile, collapses board, displays board

(define (move b)
  (cond
    [(empty? b) #f]
    [(cons? b)
     (display-board b)
     (let ([inp (gen-input)])
       (printf "New tile - ~a\n" inp)
       (printf "Enter (x,y) - \n")
       (printf "Collapsing...\n")
       (collapse b (read) (read) inp))]))

;display-board-row : list-of-tiles -> list-of-tiles
; Displays a row of tiles

(define (display-board-row row)
  (cond
    [(empty? row) empty]
    [(cons? row)
     (printf " ~a " (tile-v (first row)))
     (display-board-row (rest row))]))

;display-board : board -> board
; Displays a board of tiles

(define (display-board board)
  (cond
    [(empty? board) empty]
    [(cons? board)
     (display-board-row (first board))
     (printf "\n")
     (display-board (rest board))
     board]))

(multi-collapse b3 1 0 'grass)
;(display-board (move (move (move b1))))
