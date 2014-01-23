#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; VERSION 1        ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;(set! lval (tile-val this-tile))
  (set-tile-chk! this-tile #t)
  
  ;north check
;  (if (and (on-board? board x y-1) 
;           (symbol=? (tile-val(get-tile board x y-1)) lval)
;           (tile-chk (get-tile board x y-1)))
;      (set!  c (count-neighbour board x y-1 c))
;      
;      ;north-east check
;      (if (and (on-board? board x+1 y-1) 
;               (symbol=? (tile-val(get-tile board x+1 y-1)) lval)
;               (tile-chk (get-tile board x+1 y-1)))
;          (set! c (count-neighbour board x+1 y-1 c)))
;      
;      ;north-west check
;      (if (and (on-board? board x-1 y-1) 
;               (symbol=? (tile-val(get-tile board x-1 y-1)) lval)
;               (tile-chk (get-tile board x-1 y-1)))
;          (set! c (count-neighbour board x-1 y-1 c))))
;  
;  ;south check
;  (if (and (on-board? board x y+1) 
;           (symbol=? (tile-val(get-tile board x y+1)) lval)
;           (tile-chk (get-tile board x y+1)))
;      (set!  c (count-neighbour board x y+1 c))
;      
;      ;south-east check
;      (if (and (on-board? board x+1 y+1) 
;               (symbol=? (tile-val(get-tile board x+1 y+1)) lval)
;               (tile-chk (get-tile board x+1 y+1)))
;          (set! c (count-neighbour board x+1 y+1 c)))
;      
;      ;south-west check
;      (if (and (on-board? board x-1 y-1) 
;               (symbol=? (tile-val(get-tile board x-1 y+1)) lval)
;               (tile-chk (get-tile board x-1 y+1)))
;          (set! c (count-neighbour board x-1 y+1 c))))
;  
;   ;east check
;  (if (and (on-board? board x+1 y) 
;           (symbol=? (tile-val(get-tile board x+1 y)) lval)
;           (tile-chk (get-tile board x+1 y)))
;      (set!  c (count-neighbour board x+1 y c))
;      
;      ;north-east check
;      (if (and (on-board? board x+1 y-1) 
;               (symbol=? (tile-val(get-tile board x+1 y-1)) lval)
;               (tile-chk (get-tile board x+1 y-1)))
;          (set! c (count-neighbour board x+1 y-1 c)))
;      
;      ;south-east check
;      (if (and (on-board? board x+1 y+1) 
;               (symbol=? (tile-val(get-tile board x+1 y+1)) lval)
;               (tile-chk (get-tile board x+1 y+1)))
;          (set! c (count-neighbour board x+1 y+1 c))))
;  
;  ;west check
;  (if (and (on-board? board x-1 y) 
;           (symbol=? (tile-val(get-tile board x-1 y)) lval)
;           (tile-chk (get-tile board x-1 y)))
;      (set!  c (count-neighbour board x+1 y c))
;      
;      ;north-west check
;      (if (and (on-board? board x-1 y-1) 
;               (symbol=? (tile-val(get-tile board x-1 y-1)) lval)
;               (tile-chk (get-tile board x-1 y-1)))
;          (set! c (count-neighbour board x-1 y-1 c)))
;      
;      ;south-west check
;      (if (and (on-board? board x-1 y-1) 
;               (symbol=? (tile-val(get-tile board x-1 y+1)) lval)
;               (tile-chk (get-tile board x-1 y+1)))
;          (set! c (count-neighbour board x-1 y+1 c)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         VERSION 2                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-neighbour board x y c)
  (set! c (add1 c))
  (set-tile-chk! (get-tile board x y) #t)
  (define this-tile (struct-copy tile (get-tile board x y)))
  ;(set! lval (tile-val this-tile))
  
  ;north check
  (cond 
    [(on-board? board x (- y 1))
      (cond 
        [(and (symbol=? (tile-val(get-tile board x (- y 1))) 
                         (tile-val this-tile))
               (not (tile-chk (get-tile board x (- y 1)))))
          ((set!  c (count-neighbour board x (- y 1) c))
           
      ;north-east check
           (cond 
             [(on-board? board (+ x 1) (- y 1))
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (+ x 1) (- y 1))) 
                                 (tile-val this-tile))
                       (not (tile-chk (get-tile board (+ x 1) (- y 1)))))
                  ((set! c (count-neighbour board (+ x 1) (- y 1) c))
                   c)]
                 [else c])]
               [else c])
      
      ;north-west check
           (cond 
             [(on-board? board (- x 1) (- y 1)) 
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (- x 1) (- y 1))) 
                                  (tile-val this-tile))
                        (not (tile-chk (get-tile board (- x 1) (- y 1)))))
                   ((set! c (count-neighbour board (- x 1) (- y 1) c))
                   c)]
                 [else c])]
             [else c]))]
        [else c])]
    [else c])
          
  ;south check
  (cond 
    [(on-board? board x (+ y 1))
      (cond 
        [(and (symbol=? (tile-val(get-tile board x (+ y 1))) 
                         (tile-val this-tile))
               (not (tile-chk (get-tile board x (+ y 1)))))
          ((set!  c (count-neighbour board x (+ y 1) c))
           
      ;south-east check
           (cond 
             [(on-board? board (+ x 1) (+ y 1))
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (+ x 1) (+ y 1))) 
                                 (tile-val this-tile))
                       (not (tile-chk (get-tile board (+ x 1) (+ y 1)))))
                  ((set! c (count-neighbour board (+ x 1) (+ y 1) c))
                   c)]
                 [else c])]
               [else c])
       
      ;south-west check
           (cond 
             [(on-board? board (- x 1) (+ y 1)) 
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (- x 1) (+ y 1))) 
                                  (tile-val this-tile))
                        (not (tile-chk (get-tile board (- x 1) (+ y 1)))))
                   ((set! c (count-neighbour board (- x 1) (+ y 1) c))
                   c)]
                 [else c])]
             [else c]))]
        [else c])]
    [else c])
  
  ;west check
  (cond 
    [(on-board? board (- x 1) y)
      (cond 
        [(and (symbol=? (tile-val(get-tile board (- x 1) y)) 
                         (tile-val this-tile))
               (not (tile-chk (get-tile board (- x 1) y))))
          ((set!  c (count-neighbour board (- x 1) y c))
           
           ;north-west check
           (cond 
             [(on-board? board (- x 1) (- y 1)) 
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (- x 1) (- y 1))) 
                                  (tile-val this-tile))
                        (not (tile-chk (get-tile board (- x 1) (- y 1)))))
                   ((set! c (count-neighbour board (- x 1) (- y 1) c))
                   c)]
                 [else c])]
             [else c])
           
           ;south-west check
           (cond 
             [(on-board? board (- x 1) (+ y 1)) 
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (- x 1) (+ y 1))) 
                                  (tile-val this-tile))
                        (not (tile-chk (get-tile board (- x 1) (+ y 1)))))
                   ((set! c (count-neighbour board (- x 1) (+ y 1) c))
                   c)]
                 [else c])]
             [else c]))]
        [else c])]
    [else c])
  
  ;east check
  (cond 
    [(on-board? board (+ x 1) y)
      (cond 
        [(and (symbol=? (tile-val(get-tile board (+ x 1) y)) 
                         (tile-val this-tile))
               (not (tile-chk (get-tile board (+ x 1) y))))
          ((set!  c (count-neighbour board (+ x 1) y c))
           
            ;north-east check
           (cond 
             [(on-board? board (+ x 1) (- y 1))
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (+ x 1) (- y 1))) 
                                 (tile-val this-tile))
                       (not (tile-chk (get-tile board (+ x 1) (- y 1)))))
                  ((set! c (count-neighbour board (+ x 1) (- y 1) c))
                   c)]
                 [else c])]
               [else c])
           
           ;south-east check
           (cond 
             [(on-board? board (+ x 1) (+ y 1))
               (cond 
                 [(and (symbol=? (tile-val(get-tile board (+ x 1) (+ y 1))) 
                                 (tile-val this-tile))
                       (not (tile-chk (get-tile board (+ x 1) (+ y 1)))))
                  ((set! c (count-neighbour board (+ x 1) (+ y 1) c))
                   c)]
                 [else c])]
               [else c])
           )]
        [else c])]
    [else c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         VERSION 3                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;count-neighbour :  list-of-list-of-tile num num num -> num
;Returns 'count', the number of relevant neighbouring tiles of ('x','y') that are same
(define (count-neighbour board x y c)
  (cond
    [(empty? board) ...]
    [(cons? board) 
     (cons (count-neighbour-row (first board) x y c)
           (count-neighbour (rest board) x y c))]))

;count-neighbour-row :  list-of-list-of-tile num num num -> ...
;Retuns 'count', the number of relevant neighbouring tiles of ('x', 'y') that are same
; and marks them as visited

(define (count-neighbour-row row x y c)
  (cond
    [(empty? row) ...]
    [(cons? row) 
     ... (first row) ...
     (count-neighbour-row x y (rest row) c) ...]))