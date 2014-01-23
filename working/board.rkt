#lang racket
(require rackunit)
(require test-engine/racket-tests)

; Task for 1/22
; function that takes a move and put it on the board and makes a collapse

; A tile is either
;   - 'blank 
;   - 'grass
;   - 'bush
;   - 'tree
;   - 'shack
;   - 'house
;   - 'mansion
;   - 'castle

; next-tile : symbol -> symbol
; Gets the next tile of cur-tile
(define (next-tile cur-tile)
  (cond
    ;[(symbol=? cur-tile 'blank) 'grass]
    [(symbol=? cur-tile 'grass) 'bush]
    [(symbol=? cur-tile 'bush)  'tree]
    [(symbol=? cur-tile 'tree)  'shack]
    [(symbol=? cur-tile 'shack) 'house]
    [(symbol=? cur-tile 'house) 'mansion]
    [(symbol=? cur-tile 'mansion) 'castle]
    [else (error (format "not a tile: ~s~n" cur-tile))]))

(module+ test 
  (check-equal? (next-tile 'grass) 'bush)
  (check-equal? (next-tile 'bush)  'tree)
  (check-equal? (next-tile 'tree)  'shack)
  (check-equal? (next-tile 'shack) 'house)
  (check-equal? (next-tile 'house) 'mansion)
  (check-equal? (next-tile 'mansion) 'castle)
  (check-error (next-tile 'test) "not a tile: test")
  )


; A tile is
;  (make-tile symbol num num boolean)
(define-struct tile (v x y c #:transparent)
  
  (define game-board
    (list
     (list (tile 'blank 0 0 #t) (tile 'blank 1 0 #t) (tile 'blank 2 0 #f))
     (list (tile 'blank 0 1 #t) (tile 'grass 1 1 #f) (tile 'blank 2 1 #f))
     (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))
  
  (define board-size (length game-board))
  
  ;get-tile: list-of-list-of-tiles num num -> tile
  ; Returns the 'tile' with coordinates ('x','y') on 'board'
  
  (define (get-tile board x y)
    (list-ref (list-ref board y) x))
  
  ;on-board : list-of-list-of-tiles num num -> bool
  ;Checks if a given tile ('x','y') is on the 'board'
  
  (define (on-board? board x y)
    (cond
      [(empty? board) #f]
      [(and (and (< x board-size) (> x -1)) 
            (and (< y board-size) (> y -1))) #t]
      [else #f]))
  
  
  ; replace-neighbour-row list-of-tiles num num -> list-of-tiles
  ; Find and replace all visited tiles with blank except ('x', 'y')
  
  (define (replace-neighbour-row row x y)
    (cond
      [(empty? row) empty]
      [(cons? row)
       (let* ([tv (tile-v (first row))]
              [tx (tile-x (first row))]
              [ty (tile-y (first row))]
              [tc (tile-c (first row))])
         (cond 
           [tc 
            (cond 
              [(and (equal? tx x)
                    (equal? ty y)
                    (not (symbol=? tv 'blank)))
               (cons (tile (next-tile tv) x y #t)
                     (replace-neighbour-row (rest row) x y))]
              [else  (cons (tile 'blank tx ty #t) 
                           (replace-neighbour-row (rest row) x y))])]
           [else (cons (tile tv tx ty tc)
                       (replace-neighbour-row (rest row) x y))]))]))
  
  ;replace-neighbours : list-of-list-of-tiles num num -> list-of-list-of-tiles
  ;Finds and replaces all neighbours of ('x','y') on 'board' with higher field
  ;Algo -
  ;Scan through the entire board, and replace all visited tiles except this-tile
  ;with 'blank. 
  ;Replace this tile with 'next-tile'
  
  (define (replace-neighbour board x y)
    (cond
      [(empty? board) empty]
      [(cons? board) 
       (cons (replace-neighbour-row(first board) x y)
             (replace-neighbour (rest board) x y))]))
  
  ; valid-neighbour? : list-of-list-of-tile num num symbol -> boolean
  ; Checks if a given tile is a valid neighbour based on 'val'
  (define (valid-neighbour? board x y val)
    (if (on-board? board x y)
        (if (and (symbol=? (tile-v (get-tile board x y)) val)
                 (not (tile-c (get-tile board x y)))
                 (not (symbol=? (tile-v (get-tile board x y)) 'blank)))
            #t #f)
        #f))
  
  ;count-neighbour :  list-of-list-of-tile num num num -> num
  ;Returns 'count', the number of relevant neighbouring tiles of ('x','y') 
  ; that are same
  ;Algo-
  ;Increment count
  ;Mark (x,y) as visited
  ;Visit all the valid neighbouring tiles(8) and call count-neighbour
  
  (define (count-neighbour board x y c)
    (cond
      [(empty? board) 0]
      [else
       (set! c (add1 c))
       (set-tile-c! (get-tile board x y) #t)
       (define this-tile (struct-copy tile (get-tile board x y)))
       ;north
       (cond 
         [(valid-neighbour? board x (- y 1) (tile-v this-tile))
          (set! c (count-neighbour board x (- y 1) c))
          ;north-east
          (cond
            [(valid-neighbour? board (+ x 1) (- y 1) (tile-v this-tile))
             (set! c (count-neighbour board (+ x 1) (- y 1) c))c])
          ;north-west
          (cond
            [(valid-neighbour? board (- x 1) (- y 1) (tile-v this-tile))
             (set! c (count-neighbour board (- x 1) (- y 1) c))c])c]
         [else c])
       
       ;south
       (cond
         [(valid-neighbour? board x (+ y 1) (tile-v this-tile))
          (set! c (count-neighbour board x (+ y 1) c))
          ;south-east
          (cond
            [(valid-neighbour? board (+ x 1) (+ y 1) (tile-v this-tile))
             (set! c (count-neighbour board (+ x 1) (+ y 1) c))c])
          ;south-west
          (cond
            [(valid-neighbour? board (- x 1) (- y 1) (tile-v this-tile))
             (set! c (count-neighbour board (- x 1) (- y 1) c))c])c]
         [else c])
       
       ;east
       (cond
         [(valid-neighbour? board (+ x 1) y (tile-v this-tile))
          (set! c (count-neighbour board (+ x 1) y c))
          ;north-east
          (cond
            [(valid-neighbour? board (+ x 1) (- y 1) (tile-v this-tile))
             (set! c (count-neighbour board (+ x 1) (- y 1) c))c])
          ;south-east
          (cond
            [(valid-neighbour? board (+ x 1) (+ y 1) (tile-v this-tile))
             (set! c (count-neighbour board (+ x 1) (+ y 1) c))c])]
         [else c])
       
       ;west
       (cond
         [(valid-neighbour? board (- x 1) y (tile-v this-tile))
          (set! c (count-neighbour board (- x 1) y c))
          ;north-west
          (cond
            [(valid-neighbour? board (- x 1) (- y 1) (tile-v this-tile))
             (set! c (count-neighbour board (- x 1) (- y 1) c))c])
          ;south-west
          (cond
            [(valid-neighbour? board (- x 1) (- y 1) (tile-v this-tile))
             (set! c (count-neighbour board (- x 1) (- y 1) c))c])c]
         [else c])]))
  
  ; reset-check-row : list-of-tiles -> list-of-tiles
  ; Resets all check fields in 'row' to #f
  
  (define (reset-check-row row)
    (cond
      [(empty? row) row]
      [(cons? row) (cons (tile (tile-v (first row))
                               (tile-x (first row))
                               (tile-y (first row))
                               #f)
                         (reset-check-row (rest row)))]))
  
  
  ; reset-check : list of list of tiles -> list of list of tiles
  ; Resets all check fields in 'board' to #f
  
  (define (reset-check board)
    (cond
      [(empty? board) board]
      [(cons? board) (cons (reset-check-row (first board))
                           (reset-check (rest board)))]))
  
  ; collapse-board : list-of-list-of-tile num num -> boolean [board-or-#f]
  ; Collapse all possible 3 or more occurances of 'val' into next 'val' for ('x','y')
  ; Returns if collapse was successful
  
  (define (collapse-board board x y)
    (let-values ([(b n) (count-neighbour (reset-check board) x y 0)])
      (cond 
        [(> n 2)
         (cond 
           [(replace-neighbour b x y) #t]
           [else #f])]
        [else #f])))
  
  ;place-tile-board-row : list-of-tiles, tile -> list-of-tiles
  ;Place tile on row
  (define (place-tile-row r v x y)
    (cond
      [(empty? r)  empty]
      [(cons? r) 
       (cond
         [(and (equal? (tile-x (first r)) x)
               (equal? (tile-y (first r)) y))
          (list* (tile v (tile-x (first r)) (tile-y (first r)) #f)
                 (place-tile-row (rest r) v x y))]
         [else (list* (first r) 
                      (place-tile-row (rest r) v x y))])]))
  
  ;place-tile-board : list-of-list-of-tiles, tile -> list-of-list-of-tiles
  ;Places tile on board
  (define (place-tile b v x y)
    (cond
      [(empty? b) empty]
      [(cons? b) 
       (list* (place-tile-row (first b) v x y)
              (place-tile (rest b) v x y))]))
  
  ; place-tile-on : tile, list-of-list-of-tiles -> boolean [board-or-#f]
  ; Places on board and returns if place-tile was successful
  (define (place-tile-on b v x y)
    (cond 
      [(symbol=? (tile-v (get-tile b (tile-x t) (tile-y t))) 'blank)
       (place-tile b v x y)]
      [else #f]))
  
  (define (display-board-row row)
    (cond
      [(empty? row) empty]
      [(cons? row)
       (printf " ~a " (tile-v (first row)))
       (display-board-row (rest row))]))
  
  (define (display-board board)
    (cond
      [(empty? board) empty]
      [(cons? board)
       (display-board-row (first board))
       (printf "\n")
       (display-board (rest board))]))
  
  ;(printf "Before collapse \n")
  ;(display-board game-board)
  ;(collapse-board-init)
  ;(printf "After collapse \n")
  
  (display-board game-board)
  
  (printf "Place tile 'grass at (2 2) - ")
  (place-tile (tile 'grass 2 2 #f) game-board)
  (printf "Collapse (2 2) - ")
  (collapse-board game-board 2 2)
  (display-board game-board)
  
  (printf "Place tile 'grass at (2 1) - ")
  (place-tile (tile 'grass 2 1 #f) game-board)
  (printf "Collapse (2 2) - ")
  (collapse-board game-board 2 1)
  (display-board game-board)
  
  (printf "Place tile 'grass at (1 2) - ")
  (place-tile (tile 'grass 1 2 #f) game-board)
  (printf "Collapse (1 2) - ")
  (collapse-board game-board 1 2)
  (display-board game-board)