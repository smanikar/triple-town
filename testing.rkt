#lang racket
(require rackunit)
(require test-engine/racket-tests)

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

;Board-size = 3
(define board-size 3)

; A tile is
;  (make-tile symbol num num boolean)
(define-struct tile ((val) x y (chk #:mutable)) #:transparent)

(define board1
  (list
   (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'grass 0 1 #t) (tile 'blank 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

(define board1b
  (list
   (list (tile 'grass 0 0 #f) (tile 'grass 1 0 #f) (tile 'blank 2 0 #f))
   (list (tile 'grass 0 1 #f) (tile 'blank 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

(define board1a
  (list
   (list (tile 'bush  0 0 #t) (tile 'blank 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'blank 0 1 #t) (tile 'blank 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

;get-tile: list-of-list-of-tiles num num -> tile
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile board x y)
  (list-ref (list-ref board y) x))

(module+ test 
  (check-equal? (get-tile board1  0 1) (tile 'grass 0 1 #t))
  (check-equal? (get-tile board1a 0 1) (tile 'blank 0 1 #t))
  )

;on-board : list-of-list-of-tiles num num -> bool
;Checks if a given tile ('x','y') is on the 'board'

(define (on-board? board x y)
  (cond
    [(empty? board) #f]
    [(and (and (< x (- board-size 1)) (> x -1)) 
          (and (< y (- board-size 1)) (> y -1))) #t]
    [else #f]))

(module+ test
  (check-equal? (on-board? empty  1 0) #f)
  (check-equal? (on-board? board1  1 0) #t)
  (check-equal? (on-board? board1a 1 8) #f)
  (check-equal? (on-board? board1  -1 0) #f)
  )

; replace-neighbour-row list-of-tiles num num -> list-of-tiles
; Find and replace all visited tiles with blank except ('x', 'y')

(define (replace-neighbour-row row x y)
  (cond
    [(empty? row) empty]
    [(cons? row) 
     (cond 
       [(tile-chk (first row)) 
        (cond 
          [(and (equal? (tile-x (first row)) x)
                (equal? (tile-y (first row)) y))
           (list* (tile (next-tile (tile-val (first row))) x y #t)
                  (replace-neighbour-row (rest row) x y))]
          [else  (list* (tile 'blank 
                              (tile-x (first row)) 
                              (tile-y (first row)) 
                              #t) 
                        (replace-neighbour-row (rest row) x y))])]
       [else (list* (tile (tile-val (first row)) 
                          (tile-x (first row))
                          (tile-y (first row))
                          (tile-chk (first row)))
                    (replace-neighbour-row (rest row) x y))])]))

(module+ test
  (check-equal? (replace-neighbour-row empty 0 0) empty)
  (check-equal? (replace-neighbour-row (list (tile 'grass 0 0 #f) 
                                             (tile 'bush  0 1 #t)
                                             (tile 'bush  0 2 #t)) 0 1)
                (list (tile 'grass 0 0 #f)
                      (tile 'tree  0 1 #t)
                      (tile 'blank 0 2 #t)))
  (check-equal? (replace-neighbour-row (list (tile 'blank 1 0 #f) 
                                             (tile 'bush 1 1 #t)
                                             (tile 'blank 1 2 #t)) 1 1)
                (list (tile 'blank 1 0 #f) 
                      (tile 'tree  1 1 #t)
                      (tile 'blank 1 2 #t)))
  (check-error (replace-neighbour-row (list (tile 'blank 2 0 #f) 
                                            (tile 'blank 2 1 #t)
                                            (tile 'blank 2 2 #t)) 2 2)
               "not a tile: blank"))

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

(module+ test 
  (check-equal? (replace-neighbour board1 0 0) board1a)
  ;(check-equal? (replace-neighbour board1 1 0) board1)
  )
        
;count-neighbour :  list-of-list-of-tile num num num -> num
;Returns 'count', the number of relevant neighbouring tiles of ('x','y') that are same
;Algo-
;Increment count
;Mark (x,y) as visited
;If N is on the board and val of N is same, call (count-neighbour N), 
;                     if NE is on the board call (count-neighbour NE), 
;                     if NW is on the board call (count-neighbour NW), 
;If S is on the board and val of S is same, call (count-neighbour S), 
;                     if SE is on the board call (count-neighbour SE), 
;                     if SW is on the board call (count-neighbour SW),
;If E is on the board and val of E is same, call (count-neighbour E), 
;                     if NE is on the board call (count-neighbour NE),
;                     if SE is on the board call (count-neighbour SE), 
;If W is on the board and val of W is same, call (count-neighbour W), 
;                     if NW is on the board call (count-neighbour NW), 
;                     if SW is on the board call (count-neighbour SW),


(module+ test 
  (check-equal? (count-neighbour board1b 0 0 0) 2)
  (check-equal? (count-neighbour board1 1 0 0) 1)
  )