#lang racket
(require rackunit)
(require test-engine/racket-tests)

(define-struct tile (val x y chk) #:transparent)

;place-tile-board-row : list-of-tiles, tile -> list-of-tiles
;Place tile on row
(define (place-tile-row row v x y)
  (cond
    [(empty? row)  empty]
    [(cons? row) 
     (cond
      [(and (equal? (tile-x (first row)) x)
            (equal? (tile-y (first row)) y))
       (list* (tile v (tile-x (first row)) (tile-y (first row)) #f)
             (place-tile-row (rest row) v x y))]
      [else (list* (first row) 
                  (place-tile-row (rest row) v x y))])]))

;place-tile-board : list-of-list-of-tiles, tile -> list-of-list-of-tiles
;Places tile on board
(define (place-tile board v x y)
  (cond
    [(empty? board) empty]
    [(cons? board) 
     (list* (place-tile-row (first board) v x y)
           (place-tile (rest board) v x y))]))

(module+ test 
  (check-equal? (place-tile empty 'blank 0 0) empty)
  (check-equal? (place-tile 
                 (list (list (tile 'blank 0 0 #t) (tile 'blank 1 0 #t))
                       (list (tile 'blank 0 1 #f) (tile 'grass 1 1 #t)))
                 'grass 1 0)
                (list (list (tile 'blank 0 0 #t) (tile 'grass 1 0 #f))
                      (list (tile 'blank 0 1 #f) (tile 'grass 1 1 #t))))
  (check-equal? (place-tile 
                 (list
                  (list (tile 'blank 0 0 #t) (tile 'blank 1 0 #t) (tile 'blank 2 0 #f))
                  (list (tile 'blank 0 1 #t) (tile 'grass 1 1 #f) (tile 'blank 2 1 #f))
                  (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f)))
                 'grass 2 1)
                (list
                 (list (tile 'blank 0 0 #t) (tile 'blank 1 0 #t) (tile 'blank 2 0 #f))
                 (list (tile 'blank 0 1 #t) (tile 'grass 1 1 #f) (tile 'grass 2 1 #f))
                 (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f)))))