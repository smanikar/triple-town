#lang racket
; Preliminary version for 1D

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