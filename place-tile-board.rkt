#lang racket
;place-tile-board-row : list-of-tiles, tile -> list-of-tiles
;Place tile on row
(define (place-tile-board-row row in-tile)
  (cond
    [(empty? row)  empty]
    [(cons? row) 
     (cond
      [(and (equal? (tile-x (first row)) (tile-x in-tile))
            (equal? (tile-y (first row)) (tile-y in-tile)))
       (list* (tile (tile-val in-tile)
                    (tile-x (first row))
                    (tile-y (first row))
                    #f)
              (place-tile-board-row (rest row) in-tile))]
      [else (list* (first row) 
                   (place-tile-board-row (rest row) in-tile))])]))

;place-tile-board : list-of-list-of-tiles, tile -> list-of-list-of-tiles
;Places tile on board
(define (place-tile-board board in-tile)
  (cond
    [(empty? board) empty]
    [(cons? board) 
     (cons (place-tile-board-row (first board) in-tile)
           (place-tile-board (rest board) in-tile))]))