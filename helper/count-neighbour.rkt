#lang racket
(require rackunit)
(require test-engine/racket-tests)

; A tile is
;  (make-tile symbol num num boolean)
(define-struct tile (v x y c) #:transparent)

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

(define board-size 3)

;get-tile: list-of-list-of-tiles num num -> tile
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile board x y)
  (list-ref (list-ref board y) x))

;on-board : list-of-list-of-tiles num num -> bool
;Checks if a given tile ('x','y') is on the 'board'

(define (on-board? x y)
  (if (and (and (< x board-size) (> x -1)) 
           (and (< y board-size) (> y -1)))
      #t
      #f))

; valid-neighbour? : list-of-list-of-tile num num symbol -> boolean
; Checks if a given tile is a valid neighbour based on 'val'
(define (valid-neighbour? board x y v l)
  (if (on-board? x y)
      (let ([t (get-tile board x y)])
        (if (and (symbol=? (tile-v t) v)
                 (false? (member t l)))
            #t 
            #f))
      #f))

;count-neighbour :  
;list-of-list-of-tile num num symbol list -> (list boolean [board-or-#f], num)

(define (count-neighbour b x y v l)
  (if (valid-neighbour? b x y v l)
        (let* ([t (get-tile b x y)]
               ;north
               [r1 (count-neighbour b x (sub1 y) v (cons t l))]
               ;south
               [r2 (count-neighbour (car r1) x (add1 y) v (cons t l))]
               ;east
               [r3 (count-neighbour (car r2) (add1 x) y v (cons t l))]
               ;west
               [r4 (count-neighbour (car r3) (sub1 x) y v (cons t l))])
          (list (car r4) (+ 1 (cadr r1) (cadr r2) (cadr r3) (cadr r4))))
        (list b 0)))

(define (replace-neighbours b x y v l)
  (if (valid-neighbour? b x y v l)
      (let* ([t (get-tile b x y)]
             [b0 (replace b x y 'blank)]
             [b1 (replace-neighbours b0 x (sub1 y) v (cons t l))]
             [b2 (replace-neighbours b1 x (add1 y) v (cons t l))]
             [b3 (replace-neighbours b2 (add1 x) y v (cons t l))]
             [b4 (replace-neighbours b3 (sub1 x) y v (cons t l))])
        b4)
      b))
   
; replace-row : list num val -> list
;  Replces one item in a list (with 0-based indexing)

(define (replace-row l i v)
  (cond
    [(empty? l) empty]
    [(cons? l) (if (= i 0)
                   (cons (tile v 
                               (tile-x (first l)) 
                               (tile-y (first l)) 
                               (tile-c (first l))) 
                         (rest l))
                   (cons (first l)
                         (replace-row (rest l) (- i 1) v)))]))

; replace : board num num val -> board
;  Replaces one item in the board (with 0-based indexing)

(define (replace b x y v)
  (cond
    [(empty? b) empty]
    [(cons? b) (if (= y 0)
                   (cons (replace-row (first b) x v)
                         (rest b))
                   (cons (first b) (replace (rest b) x (- y 1) v)))]))

; place-tile : tile, list-of-list-of-tiles -> boolean [board-or-#f]
; Places on board and returns if place-tile was successful
(define (place-tile b x y v)
  (if (on-board? x y)
      (if (symbol=? (tile-v (get-tile b x y)) 'blank)
          (replace b x y v)
          #f)
      #f))
  

; collapse : tile, list-of-list-of-tiles -> boolean [board-or-#f]
; Collapses board

(define (collapse b v x y)
  (let ([b0 (place-tile b x y v)])
    (if b0
        (let ([res (count-neighbour b0 x y v empty)])
          (if (> (cadr res) 2)
              (let ([b2 (replace-neighbours (car res) x y v empty)])
                (if b2
                    (replace b2 x y (next-tile v))
                    #f))
              #f))
        #f)))


(define b1
  (list
   (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'grass 0 1 #t) (tile 'shack 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'grass 0 2 #f) (tile 'blank 1 2 #f) (tile 'grass 2 2 #f))))

(define b2
  (list
   (list (tile 'blank 0 0 #t) (tile 'blank 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'blank 0 1 #t) (tile 'shack 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'bush  1 2 #f) (tile 'blank 2 2 #f))))

(define bt1
  (list (list 'grass 'grass 'grass)
        (list 'blank 'blank 'blank)
        (list 'blank 'blank 'blank)))

(define bt2
  (list (list 'grass 'grass 'grass)
        (list 'blank 'blank 'blank)
        (list 'blank 'shack 'blank)))

(module+ test
  (check-equal? (collapse b1 'grass 1 2) b2))

;collapse-board-row : list-of-tile num num symbol list -> boolean [row-or-#f]
;Collapses all  neighbours of ('x','y')

;;;;;;;;;;;;
;INCOMPLETE;
;;;;;;;;;;;;

(define (collapse-board-row r x y v)
  (cond
    [(empty? r) empty]
    [(cons? r) empty]))


;collapse-board : list-of-list-of-tile num num symbol list -> boolean [board-or-#f]
;collapses all neighbours of ('x','y')

;;;;;;;;;;;;
;INCOMPLETE;
;;;;;;;;;;;;

(define (collapse-board b x y v)
  (cond
    [(empty? b) empty]
    [(cons? b) 
     (cons(collapse-board-row (first b) x y  v)
          (collapse-board (rest b) x y v))]))

(define mlist (list (list 1 2) (list 3 4)))
;(module+ test
;  (check-equal? (list (list 1 2) (list 3 4)) mlist)
;  (check-equal? (count-neighbour b1 0 0 'grass empty)
;                (list b1 3))
;  (check-equal? (count-neighbour b2 0 0 'grass empty)
;                (list b2 5)))
