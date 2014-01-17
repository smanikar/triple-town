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

;Board-size = 3
(define board-size 3)

; A tile is
;  (make-tile symbol num num boolean)
(define-struct tile ((val #:mutalbe) x y (chk #:mutable)))

; A board is a list-of-list-of-tile
; (define board1
;   (list
;    (list (tile 'grass 1 1) (tile 'grass 1 2) (tile 'blank 1 3))
;    (list (tile 'grass 2 1) (tile 'blank 2 2) (tile 'blank 2 3))
;    (list (tile 'blank 3 1) (tile 'blank 3 2) (tile 'blank 3 3))))


; (define board1a
;   (list
;    (list (tile 'grass 1 1) (tile 'blank 1 2) (tile 'blank 1 3))
;    (list (tile 'blank 2 1) (tile 'blank 2 2) (tile 'blank 2 3))
;    (list (tile 'blank 3 1) (tile 'blank 3 2) (tile 'blank 3 3))))

(define board1
  (list
   (list (tile 'grass 0 0) (tile 'grass 0 1) (tile 'blank 0 2))
   (list (tile 'grass 1 0) (tile 'blank 1 1) (tile 'blank 1 2))
   (list (tile 'blank 2 0) (tile 'blank 2 1) (tile 'blank 2 2))))

(define board1a
  (list
   (list (tile 'bush  0 0) (tile 'blank 0 1) (tile 'blank 0 2))
   (list (tile 'blank 1 0) (tile 'blank 1 1) (tile 'blank 1 2))
   (list (tile 'blank 2 0) (tile 'blank 2 1) (tile 'blank 2 2))))
  
;get-tile: list-of-list-of-tiles num num -> tile
; Returns the 'tile' with coordinates ('x','y') on 'board'
(define (get-tile board x y)
  (list-ref (list-ref board x) y))

(module+ test 
  (check-equal? (get-tile board1  1 0) (tile 'grass 1 0))
  (check-equal? (get-tile board1a 1 0) (tile 'blank 1 0))
  ) 

;on-board : list-of-list-of-tiles num num -> bool
;Checks if a given tile ('x','y') is on the 'board'

(define (on-board? board x y)
  (cond
    [(and (and (< x (- board-size 1)) (> x -1))
           (and (< y (- board-size 1)) (> y -1))) #t]
    [else #f]))

(module+ test 
  (check-equal? (on-board? board1  1 0) #t)
  (check-equal? (get-tile board1a 1 8) #f)
  ) 

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
      (cons (replace-row(first board) x y)
            (replace-neighbour (rest board) x y))]))

(module+ test 
  (check-equal? (replace-neighbour board1 0 0) board1a)
  ;(check-equal? (replace-neighbour board1 1 0) board1)
  ) 
; replace-row list-of-tiles num num -> list-of-tiles
; Find and replace all visited tiles with blank except ('x', 'y')

(define (replace-row row x y)
  (cond
    [(empty? row) empty]
    [(cons? row) 
     (cond 
       [(tile-chk(first row)) 
        (cond 
          [(and (=? (tile-x (first row)) x)
                (=? (tile-y (first row)) y))
           (list* (tile (next-tile (tile-val (first row))) x y #t) 
                  (replace-row(rest row) x y))]
          ;(set-tile-val! (first row) (next-tile (tile-val (first row))))]                  
          [else  (list* (tile 'blank x y #t) 
                        (replace-row(rest row) x y))])]
       [else (list* (tile (tile-val (first row)) 
                          (tile-x (first row))
                          (tile-y (first row))
                          (tile-chk (first row)))
                    (replace-row(rest row) x y))])]))
     
(module+ test
  (check-equal? (replace-row empty 0 0) emtpy)
  (check-equal? (replace-row (list (tile 'grass 0 0 #f) 
                                   (tile 'bush  0 1 #t)
                                   (tile 'bush  0 2 #t)) 0 1)
                (list (tile 'grass 0 0 #f)
                      (tile 'tree  0 1 #t)
                      (tile 'blank 0 2 #t)))
  (check-equal? (replace-row (list (tile 'blank 1 0 #f) 
                                   (tile 'grass 1 1 #t)
                                   (tile 'blank 1 2 #t)) 1 1)
                (list (tile 'blank 1 0 #f) 
                      (tile 'tree  1 1 #t)
                      (tile 'blank 1 2 #t)))
  (check-equal? (replace-row (list (tile 'blank 2 0 #f) 
                                   (tile 'blank 2 1 #t)
                                   (tile 'blank 2 2 #t)) 2 2)
                (list (tile 'blank 2 0 #f) 
                      (tile 'blank 2 1 #t)
                      (tile 'blank 2 2 #t))))
 
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

(define (count-neighbour board x y c)
  (set! c (add1 c))
  (define this-tile (struct-copy tile (get-tile board x y)))
  (set! lval (tile-val this-tile))
  (set-tile-chk! this-tile #t)
  
  ;north check
  (if (and (on-board? board x y-1) 
           (symbol=? (tile-val(get-tile board x y-1)) lval)
           (tile-chk (get-tile board x y-1)))
      (set!  c (count-neighbour board x y-1 c))
      
      ;north-east check
      (if (and (on-board? board x+1 y-1) 
               (symbol=? (tile-val(get-tile board x+1 y-1)) lval)
               (tile-chk (get-tile board x+1 y-1)))
          (set! c (count-neighbour board x+1 y-1 c)))
      
      ;north-west check
      (if (and (on-board? board x-1 y-1) 
               (symbol=? (tile-val(get-tile board x-1 y-1)) lval)
               (tile-chk (get-tile board x-1 y-1)))
          (set! c (count-neighbour board x-1 y-1 c))))
  
  ;south check
  (if (and (on-board? board x y+1) 
           (symbol=? (tile-val(get-tile board x y+1)) lval)
           (tile-chk (get-tile board x y+1)))
      (set!  c (count-neighbour board x y+1 c))
      
      ;south-east check
      (if (and (on-board? board x+1 y+1) 
               (symbol=? (tile-val(get-tile board x+1 y+1)) lval)
               (tile-chk (get-tile board x+1 y+1)))
          (set! c (count-neighbour board x+1 y+1 c)))
      
      ;south-west check
      (if (and (on-board? board x-1 y-1) 
               (symbol=? (tile-val(get-tile board x-1 y+1)) lval)
               (tile-chk (get-tile board x-1 y+1)))
          (set! c (count-neighbour board x-1 y+1 c))))
  
   ;east check
  (if (and (on-board? board x+1 y) 
           (symbol=? (tile-val(get-tile board x+1 y)) lval)
           (tile-chk (get-tile board x+1 y)))
      (set!  c (count-neighbour board x+1 y c))
      
      ;north-east check
      (if (and (on-board? board x+1 y-1) 
               (symbol=? (tile-val(get-tile board x+1 y-1)) lval)
               (tile-chk (get-tile board x+1 y-1)))
          (set! c (count-neighbour board x+1 y-1 c)))
      
      ;south-east check
      (if (and (on-board? board x+1 y+1) 
               (symbol=? (tile-val(get-tile board x+1 y+1)) lval)
               (tile-chk (get-tile board x+1 y+1)))
          (set! c (count-neighbour board x+1 y+1 c))))
  
  ;west check
  (if (and (on-board? board x-1 y) 
           (symbol=? (tile-val(get-tile board x-1 y)) lval)
           (tile-chk (get-tile board x-1 y)))
      (set!  c (count-neighbour board x+1 y c))
      
      ;north-west check
      (if (and (on-board? board x-1 y-1) 
               (symbol=? (tile-val(get-tile board x-1 y-1)) lval)
               (tile-chk (get-tile board x-1 y-1)))
          (set! c (count-neighbour board x-1 y-1 c)))
      
      ;south-west check
      (if (and (on-board? board x-1 y-1) 
               (symbol=? (tile-val(get-tile board x-1 y+1)) lval)
               (tile-chk (get-tile board x-1 y+1)))
          (set! c (count-neighbour board x-1 y+1 c))))
  c)

(module+ test 
  (check-equal? (count-neighbour board1 0 0) 2)
  (check-equal? (count-neighbour board1 1 0) 1)
  )

;collapse-tile : list-of-list-of-tile num num -> list-of-list-of-tile
;Collapses all neighbours of ('x','y') based on 'val' of 'tile'('x','y') in 'board'

;Algo-
;check all neighbours of xy
;if xy is blank, return board
;if (> 1)neighbours are the same as xy, replace them with blank
;   and replace xy with next-tile
;else return board

(define (collapse-tile board x y)
  (cond
    [(symbol=? (tile-val (get-tile board x y)) 'blank) board]
    [(> (count-neighbour board x y 0) 1) (replace-neighbour board x y)]
    [else board]))
  
(module+ test 
  (check-equal? (collapse-board board1) board1a)
  ) 


; collapse-board : list-of-list-of-tile -> list-of-list-of-tile
; Collapse all possible 3 or more occurances of field into higher field in 'board'

(define (collapse-board board)
  (reset-check board)
  (collapse-tile board 1 1))


; collapse: list-of-tiles -> list-of-tiles
; Finds three consecutive occrances of the same tile and replaces it 
; with a next tile

;(define (collapse lot)
;  (cond
;    [(empty? lot) ...]
;    [(cons? lot) ... (first lot)
;                 ... collapse(rest lot) ...]))

(define (collapse lot)
   (cond
    [(empty? lot) empty]
    [(cons? lot) 
     (cond
       [(empty? (rest lot)) (cons (first lot) (collapse (rest lot)))]
       [(cons? (rest lot)) 
        (cond
          [(empty? (rest (rest lot))) (cons (first lot) (collapse (rest lot)))]
          [(cons? (rest (rest lot))) 
           (cond
             [(and (symbol=? (first lot) (first (rest lot)))
                   (symbol=? (first (rest lot)) (first (rest (rest lot)))))
              (list* 'blank (next-tile (first lot)) 'blank 
                     (collapse (rest (rest (rest lot)))))]
             [else (cons (first lot) (collapse (rest lot)))])])])]))

(module+ test 
  (check-equal? (collapse empty) empty)
  
  (check-equal? (collapse (list 'grass))
                (list 'grass))
  
  (check-equal? (collapse (list 'grass 'grass))
                (list 'grass 'grass))
  
  (check-equal? (collapse (list 'grass 'grass 'blank 'grass))
                (list 'grass 'grass 'blank 'grass))
 
  (check-equal? (collapse (list 'grass 'grass 'grass))
                (list 'blank 'bush 'blank))
  
  (check-equal? (collapse (list 'grass 'grass 'grass 'tree))
                (list 'blank 'bush 'blank 'tree))
  
  (check-equal? (collapse (list 'tree 'grass 'grass 'grass 'tree))
                (list 'tree 'blank 'bush 'blank 'tree)
  
  (check-equal? (collapse (list 'blank 'bush 'bush 'bush))
                (list 'blank 'blank 'tree 'blank)))
  
  (check-equal? (collapse (list 'tree 'grass 'grass 'grass 'tree 'tree 'tree))
                (list 'tree 'blank 'bush 'blank 'blank 'shack 'blank))
  )