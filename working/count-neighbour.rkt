#lang racket
(require rackunit)
(require test-engine/racket-tests)

; A tile is
;  (make-tile symbol num num boolean)
(define-struct tile (v x y c) #:transparent)

(define b1
  (list
   (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'blank 0 1 #t) (tile 'grass 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

(define board-size (length b1))

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
          (and (< y board-size) (> y -1))) 
     #t]
    [else #f]))

; valid-neighbour? : list-of-list-of-tile num num symbol -> boolean
; Checks if a given tile is a valid neighbour based on 'val'
(define (valid-neighbour? board x y v l)
  (if (on-board? board x y)
      (let ([t (get-tile board x y)])
        (if (and (symbol=? (tile-v t) v)
                 (false? (member t l))
                 (false? (symbol=? (tile-v t) 'blank)))
            
            #t 
            #f))
      #f))

(define visited-list empty)
;count-neighbour :  
;list-of-list-of-tile num num symbol list -> (list boolean [board-or-#f], num)

;Returns 'count', the number of relevant neighbouring tiles of ('x','y') 
; that are same
;Algo-
;Increment count
;Mark (x,y) as visited
;Visit all the valid neighbouring tiles(8) and call count-neighbour

(define (count-neighbour b x y v l)
  (let ([ret (list b 0)])
    (cond
      [(cond 
         [(on-board? b x y)
          (let ([t (get-tile b x y)])
            (cond 
              [(and (symbol=? (tile-v t) v)
                    (false? (member t l)))
               (let* (;north
                      [r1 (count-neighbour b x (sub1 y) v (cons t l))]
                      ;south
                      [r2 (count-neighbour (car r1) x (add1 y) v (cons t l))]
                      ;east
                      [r3 (count-neighbour (car r2) (add1 x) y v (cons t l))]
                      ;west
                      [r4 (count-neighbour (car r3) (sub1 x) y v (cons t l))])
                 (list (car r4) (+ 1 (cadr r1) (cadr r2) (cadr r3) (cadr r4))))]
              [else ret]))]
         [else ret])]
      [else ret])))

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


(define b2
  (list
   (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'grass 0 1 #t) (tile 'grass 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'grass 0 2 #f) (tile 'grass 1 2 #f) (tile 'grass 2 2 #f))))

(define mlist (list (list 1 2) (list 3 4)))
(module+ test
  (check-equal? (list (list 1 2) (list 3 4)) mlist)
  (check-equal? (count-neighbour b1 0 0 'grass empty)
                (list b1 3))
  (check-equal? (count-neighbour b2 0 0 'grass empty)
                (list b2 5)))
