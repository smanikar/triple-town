#lang racket
(require rackunit)

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
    [else 'bad-input]
    ))

(module+ test 
  ;(check-equal? (next-tile 'blank) 'grass)
  (check-equal? (next-tile 'grass) 'bush)
  (check-equal? (next-tile 'bush)  'tree)
  (check-equal? (next-tile 'tree)  'shack)
  (check-equal? (next-tile 'shack) 'house)
  (check-equal? (next-tile 'house) 'mansion)
  (check-equal? (next-tile 'mansion) 'castle)
  (check-equal? (next-tile 'test) 'bad-input))

; collapse: list-of-tiles -> list-of-tiles
; Finds three consecutive occrances of the same tile and replaces it 
; with a next tile

;(define (collapse lot)
;  (cond
;    [(empty? lot) ...]
;    [(cons? lot) ... (first lot)
;                 ... collapse(rest lot) ...]))

(define (collapse lot)
  (flatten (cond
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
              (cons (cons 'blank (cons (next-tile (first lot)) 'blank)) (collapse (rest (rest (rest lot)))))            
              ]
             [else (cons (first lot) (collapse (rest lot)))])])])])))

(module+ test 
  (check-equal? (collapse empty) empty)
  
  (check-equal? (collapse (cons 'grass (cons 'grass empty))) 
                (cons 'grass (cons 'grass empty)))
  (check-equal? (collapse (cons 'grass (cons 'grass (cons 'blank (cons 'grass empty)))))
                (cons 'grass (cons 'grass (cons 'blank (cons 'grass empty)))))
 
  (check-equal? (collapse (cons 'grass (cons 'grass (cons 'grass empty))))
                (cons 'blank (cons 'bush (cons 'blank empty))))
  
  (check-equal? (collapse (cons 'grass (cons 'grass (cons 'grass (cons 'tree empty)))))
                (cons 'blank (cons 'bush (cons 'blank (cons 'tree empty)))))
  
  (check-equal? (collapse (cons 'tree (cons 'grass (cons 'grass (cons 'grass (cons 'tree empty))))))
                (cons 'tree (cons 'blank (cons 'bush (cons 'blank (cons 'tree empty))))))
  
  (check-equal? (collapse (cons 'blank (cons 'bush (cons 'bush (cons 'bush empty)))))
               (cons 'blank (cons 'blank (cons 'tree (cons 'blank empty)))))
  
  (check-equal? (collapse (cons 'tree (cons 'grass (cons 'grass (cons 'grass (cons 'tree (cons 'tree (cons 'tree empty))))))))
                (cons 'tree (cons 'blank (cons 'bush (cons 'blank (cons 'blank (cons 'shack (cons 'blank empty))))))))
  )



