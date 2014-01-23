#lang racket
; collapse-board : list-of-list-of-tile -> void
; Collapse all possible 3 or more occurances of 'val' into next 'val' in 'board'

(define (collapse-board-init)
  (for-each 
   (lambda (lot)
     (for-each
      (lambda (tile)
        (set! game-board (reset-check game-board))
        (if (not (symbol=? (tile-val tile) 'blank))
            (if (> (count-neighbour game-board
                                    (tile-x tile)
                                    (tile-y tile)
                                    0)
                   2)
                (set! game-board 
                      (replace-neighbour game-board (tile-x tile) (tile-y tile)))
                empty)
            empty))
      lot))
   game-board))