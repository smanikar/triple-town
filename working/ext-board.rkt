#lang racket
(require rackunit)
(require test-engine/racket-tests)

;next-tile : symbol -> symbol
; Gets the next tile of cur-tile
(define (next-tile cur-tile)
  (cond
    ;[(symbol=? cur-tile 'blank) 'grass]
    [(symbol=? cur-tile 'grass) 'bush]
    [(symbol=? cur-tile 'bush)  'tree]
    [(symbol=? cur-tile 'tree)  'hut]
    [(symbol=? cur-tile 'hut) 'house]
    [(symbol=? cur-tile 'house) 'mansion]
    [(symbol=? cur-tile 'mansion) 'castle]
    [(symbol=? cur-tile 'castle) 'floating-castle]
    [(symbol=? cur-tile 'floating-castle) 'triple-castle]
    [(symbol=? cur-tile 'tombstone) 'church]
    [(symbol=? cur-tile 'church)  'cathedral]
    [(symbol=? cur-tile 'cathedral)  'chest]
    [(symbol=? cur-tile 'chest) 'large-chest]
    ;[(symbol=? cur-tile 'large-chest) 'rock]
    [(symbol=? cur-tile 'rock) 'mountain]
    [(symbol=? cur-tile 'mountain) 'large-chest]
    [else (error (format "not a tile: ~s~n" cur-tile))]))

(module+ test 
  (check-equal? (next-tile 'grass) 'bush)
  (check-equal? (next-tile 'bush)  'tree)
  (check-equal? (next-tile 'tree)  'hut)
  (check-equal? (next-tile 'hut) 'house)
  (check-equal? (next-tile 'house) 'mansion)
  (check-equal? (next-tile 'mansion) 'castle)
  (check-equal? (next-tile 'castle) 'floating-castle)
  (check-equal? (next-tile 'floating-castle) 'triple-castle)
  (check-equal? (next-tile 'tombstone) 'church)
  (check-equal? (next-tile 'church) 'cathedral)
  (check-equal? (next-tile 'cathedral) 'chest)
  (check-equal? (next-tile 'chest) 'large-chest)
  ;(check-equal? (next-tile 'large-chest) 'rock)
  (check-equal? (next-tile 'rock) 'mountain)
  (check-equal? (next-tile 'mountain) 'large-chest)
  (check-error (next-tile 'test) "not a tile: test")
  )

; A tile is (make-tile symbol num num)
(define-struct tile (v x y) #:transparent)

;A list of all possible inputs
(define input-list (list 
                    'grass 
                    'bush 
                    'tree 
                    'hut 
                    'crystal 
                    'imperial-robot 
                    'bear 
                    'ninja-bear))

; A list of all possible tiles that can be collapsed by a crystal
(define crystal-list '(floating-castle chest castle mansion cathedral 
                                       house church hut tombstone tree bush grass))

(define t1 (list (list (tile 'grass 0 0) (tile 'blank 1 0))
                 (list (tile 'blank 0 1) (tile 'grass 1 1))))

(define t2 (list (list (tile 'grass 0 0) (tile 'blank 1 0) (tile 'grass 2 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'blank 1 2) (tile 'grass 2 2))))

(define b1 (list (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
                 (list (tile 'grass 0 1) (tile 'hut   1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'grass 1 2) (tile 'grass 2 2))))

(define b2 (list (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut   1 1) (tile 'blank 2 1))
                 (list (tile 'blank 0 2) (tile 'bush  1 2) (tile 'blank 2 2))))

(define b3 (list (list (tile 'blank 0 0) (tile 'grass 1 0) 
                       (tile 'grass 2 0) (tile 'blank 3 0))
                 (list (tile 'bush  0 1) (tile 'grass 1 1) 
                       (tile 'grass 2 1) (tile 'blank 3 1))
                 (list (tile 'bush  0 2) (tile 'grass 1 2) 
                       (tile 'blank 2 2) (tile 'blank 3 2))
                 (list (tile 'grass 0 3) (tile 'blank 1 3) 
                       (tile 'grass 2 3) (tile 'blank 3 3))))

(define b4 (list (list (tile 'blank 0 0) (tile 'blank 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'tree  0 1) (tile 'bush  1 1) 
                       (tile 'grass 2 1) (tile 'blank 3 1))
                 (list (tile 'bush  0 2) (tile 'bush  1 2) 
                       (tile 'blank 2 2) (tile 'blank 3 2))
                 (list (tile 'tree  0 3) (tile 'blank 1 3) 
                       (tile 'grass 2 3) (tile 'blank 3 3))))

(define b5 (list (list (tile 'blank 0 0) (tile 'blank 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'tree  0 1) (tile 'bush  1 1) 
                       (tile 'grass 2 1) (tile 'bear  3 1))
                 (list (tile 'bush  0 2) (tile 'bush  1 2) 
                       (tile 'tombstone 2 2) (tile 'blank 3 2))
                 (list (tile 'tree  0 3) (tile 'bear  1 3)
                       (tile 'tombstone 2 3) (tile 'blank 3 3))))

(define b6 (list (list (tile 'blank 0 0) (tile 'grass 1 0) 
                       (tile 'bush 2 0) (tile 'blank 3 0))
                 (list (tile 'hut  0 1) (tile 'grass  1 1) 
                       (tile 'bush 2 1) (tile 'bear  3 1))
                 (list (tile 'hut  0 2) (tile 'blank  1 2) 
                       (tile 'bush 2 2) (tile 'mansion 3 2))
                 (list (tile 'tree  0 3) (tile 'tree  1 3) 
                       (tile 'bush 2 3) (tile 'mansion 3 3))))

;gen:input : none -> symbol
; Generates a symbol from 0 to 7 based on weightes

(define (gen-input)
  (let ([n (random 100)])
    (cond
      [(< n 61) (list-ref input-list 0)]
      [(< n 76) (list-ref input-list 1)]
      [(< n 78) (list-ref input-list 2)]
      [(< n 79) (list-ref input-list 3)]
      [(< n 81) (list-ref input-list 4)]
      [(< n 84) (list-ref input-list 5)]
      [(< n 99) (list-ref input-list 6)]
      [(< n 100) (list-ref input-list 7)]
      [else (error "random number")])))

;get-tile: boards num num -> tile-or-#f
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile b x y)
  (cond
    [(empty? b) #f]
    [(cons? b)
     (list-ref (list-ref b y) x)]))

(module+ test
  (check-equal? (get-tile empty 0 0) #f)
  (check-equal? (get-tile (list (list (tile 'blank 0 0) (tile 'grass 1 0))
                                (list (tile 'grass 0 1) (tile 'blank 1 1))) 
                          0 1) 
                (tile 'grass 0 1))
  (check-equal? (get-tile  (list (list (tile 'blank 0 0)))
                           0 0) 
                (tile 'blank 0 0)))

;on-board? : num num -> bool
; Checks if a given tile ('x','y') is on the 'board'

(define (on-board? x y size)
  (and (and (< x size) (> x -1)) 
       (and (< y size) (> y -1))
       (not (and (equal? x 0) (equal? y 0)))))

(module+ test
  (check-equal? (on-board? 1 0 0) #f)
  (check-equal? (on-board? 0 0 (length b2)) #f)
  (check-equal? (on-board? 1 0 (length t1)) #t)
  (check-equal? (on-board? 1 2 (length t1)) #f)
  (check-equal? (on-board? -1 0 (length t2)) #f)
  (check-equal? (on-board? 3 0 (length b2)) #f)
  (check-equal? (on-board? 2 2 (length b2)) #t)
  )

;valid-neighbour? : board num num symbol list -> boolean
; Checks if a given tile is a valid neighbour based on 'v'

(define (valid-neighbour? b x y v l)
  (and (on-board? x y (length b))
       (let ([t (get-tile b x y)])
         (and (symbol=? (tile-v t) v)
              (false? (member t l))))))

(module+ test
  (check-equal? (valid-neighbour? empty -1 -1 'test empty) #f)
  (check-equal? (valid-neighbour? t2    -1 -1 'test empty) #f)
  (check-equal? (valid-neighbour? empty  1  2 'test empty) #f)
  (check-equal? (valid-neighbour? t2 0 2 'grass empty) #t)
  (check-equal? (valid-neighbour? t2 0 2 'grass (list (tile 'grass 0 2))) #f))

; replace-row : list num val -> list
;  Replces one item in a list (with 0-based indexing)

(define (replace-row r x v)
  (cond
    [(empty? r) empty]
    [(cons? r) (if (= x 0)
                   (cons (tile v 
                               (tile-x (first r)) 
                               (tile-y (first r))) 
                         (rest r))
                   (cons (first r)
                         (replace-row (rest r) (- x 1) v)))]))

(module+ test 
  (check-equal? (replace-row empty -1 'test) empty)
  (check-equal? (replace-row (list (tile 'hut 0 0)) -1 'reddit)   
                (list (tile 'hut 0 0)))
  (check-equal? (replace-row (list (tile 'hut 0 0)) 2 'reddit)   
                (list (tile 'hut 0 0)))
  (check-equal? (replace-row (list (tile 'hut 0 0)) -1 'reddit)   
                (list (tile 'hut 0 0)))
  (check-equal? (replace-row (list (tile 'hut 0 0)) 0 'reddit)   
                (list (tile 'reddit 0 0)))
  (check-equal? (replace-row 
                 (list (tile 'grass 0 0) (tile 'grass 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 2 'grass)
                (list (tile 'grass 0 0) (tile 'grass 1 0) 
                      (tile 'grass 2 0) (tile 'blank 3 0))))


; replace : board num num val -> board
;  Replaces one item in the board

(define (replace b x y v)
  (cond
    [(empty? b) empty]
    [(cons? b) (if (= y 0)
                   (cons (replace-row (first b) x v)
                         (rest b))
                   (cons (first b) (replace (rest b) x (- y 1) v)))]))

(module+ test
  (check-equal? (replace empty -1 -1 'reddit) empty)
  (check-equal? (replace t1 0 1 'reddit)
                (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                      (list (tile 'reddit 0 1) (tile 'grass 1 1))))
  (check-equal? (replace b2 2 1 'random)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'random 2 1))
                 (list (tile 'blank 0 2) (tile 'bush 1 2) (tile 'blank 2 2)))))

;count-neighbours :  
; board num num symbol list -> (list [list-or-#f], num)

(define (count-neighbours b x y v l)
  (if (valid-neighbour? b x y v l)
      (let* ([t (get-tile b x y)]
             [l1 (count-neighbours b x (sub1 y) v (cons t l))]     ;north
             [l2 (count-neighbours b x (add1 y) v (car l1))]       ;south
             [l3 (count-neighbours b (add1 x) y v (car l2))]       ;east
             [l4 (count-neighbours b (sub1 x) y v (car l3))])      ;west
        (list (car l4) (+ 1 (cadr l1) (cadr l2) (cadr l3) (cadr l4))))
      (list l 0)))

(module+ test
  ;(check-equal? (count-neighbours empty -1 -1 'cons empty) (list #f 0))
  ;(check-equal? (count-neighbours t1 -1 -1 'cons empty) (list empty 0))
  (check-equal? (count-neighbours t1 1 1 'cons empty) (list empty 0))
  (check-equal? (count-neighbours 
                 t1 1 1 'grass (list (tile 'grass 1 1))) 
                (list (list (tile 'grass 1 1)) 0))
  (check-equal? (count-neighbours t1 1 1 'grass empty) 
                (list (list (tile 'grass 1 1)) 1))
  (check-equal? (count-neighbours b1 0 2 'grass empty) 
                (list (list (tile 'grass 2 2) (tile 'grass 1 2) 
                            (tile 'grass 0 1) (tile 'grass 0 2)) 4)))

;replace-neighbours :
; board num num symbol list -> (list boolean [board-or-#f], list)

(define (replace-neighbours b x y v l)
  (if (valid-neighbour? b x y v l)
      (let* ([t (get-tile b x y)]
             [b0 (replace b x y 'blank)]
             [l1 (replace-neighbours b0 x (sub1 y) v (cons t l))]       ;north
             [l2 (replace-neighbours (car l1) x (add1 y) v (cadr l1))]  ;south
             [l3 (replace-neighbours (car l2) (add1 x) y v (cadr l2))]  ;east
             [l4 (replace-neighbours (car l3) (sub1 x) y v (cadr l3))]) ;west
        l4)
      (list b l)))

(module+ test
  ;(check-equal? (replace-neighbours empty -1 -1 'cons empty) (list #f empty))
  ;(check-equal? (replace-neighbours t1 -1 -1 'cons empty) (list t1 empty))
  (check-equal? (replace-neighbours t1 1 1 'cons empty) (list t1 empty))
  (check-equal? (replace-neighbours 
                 t1 1 1 'grass (list (tile 'grass 1 1))) 
                (list t1 (list (tile 'grass 1 1))))
  (check-equal? (replace-neighbours t1 1 1 'grass empty) 
                (list (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                            (list (tile 'blank 0 1) (tile 'blank 1 1))) 
                      (list (tile 'grass 1 1))))
  (check-equal? (replace-neighbours b1 0 2 'grass empty) 
                (list
                 (list
                  (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
                  (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'blank 2 1))
                  (list (tile 'blank 0 2) (tile 'blank 1 2) (tile 'blank 2 2)))
                 (list (tile 'grass 2 2) (tile 'grass 1 2) 
                       (tile 'grass 0 1) (tile 'grass 0 2)))))

; multi-collapse : board num num sybmol -> boolean [board-or-#f]
;  Try replace-neighbours as many times till it reurns #f

(define (multi-collapse b x y v)
  (let loop ([board b]
             [value v]
             [count (cadr (count-neighbours b x y v empty))])
    (if (> count 2)
        (let* ([b1 (replace (car (replace-neighbours board x y value empty))
                            x y (next-tile value))]
               [c1 (cadr (count-neighbours b1 x y (next-tile value) empty))])
          (loop b1 (next-tile v) c1))
        board)))

(module+ test
  ;(check-equal? (multi-collapse empty -1 -1 'grass) #f)
  ;(check-equal? (multi-collapse t1 -1 -1 'grass) t1)
  (check-equal? (multi-collapse t1 1 1 'reddit) t1)
  (check-equal? (multi-collapse (list (list (tile 'blank 0 0) (tile 'grass 1 0)) 
                                      (list (tile 'grass 0 1) (tile 'grass 1 1)))
                                0 1 'grass)
                (list (list (tile 'blank 0 0) (tile 'blank 1 0)) 
                      (list (tile 'bush  0 1) (tile 'blank 1 1))))
  (check-equal? (multi-collapse (list (list (tile 'blank 0 0) (tile 'grass 1 0)) 
                                      (list (tile 'grass 0 1) (tile 'grass 1 1)))
                                1 1 'grass)
                (list (list (tile 'blank 0 0) (tile 'blank 1 0)) 
                      (list (tile 'blank 0 1) (tile 'bush  1 1))))
  (check-equal? (multi-collapse b3 1 1 'grass)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'blank 0 1) (tile 'tree 1 1) 
                       (tile 'blank 2 1) (tile 'blank 3 1))
                 (list (tile 'blank 0 2) (tile 'blank 1 2)
                       (tile 'blank 2 2) (tile 'blank 3 2))
                 (list (tile 'grass 0 3) (tile 'blank 1 3) 
                       (tile 'grass 2 3) (tile 'blank 3 3)))))

; collapse : board num num sybmol -> boolean [board-or-#f]
;  Places a tile on board and collapses it

(define (collapse b x y v)
  (multi-collapse (replace b x y v) x y v))

(module+ test
  ;(check-equal? (collapse empty -1 -1 'grass) #f)
  ;(check-equal? (collapse t1 -1 -1 'grass) t1)
  ;(check-equal? (collapse t1 1 1 'reddit) t1)
  (check-equal? (collapse (list (list (tile 'blank 0 0) (tile 'grass 1 0)) 
                                (list (tile 'blank 0 1) (tile 'grass 1 1)))
                          0 1 'grass)
                (list (list (tile 'blank 0 0) (tile 'blank 1 0)) 
                      (list (tile 'bush  0 1) (tile 'blank 1 1))))
  (check-equal? (collapse t1 1 1 'grass) t1)
  (check-equal? (collapse (list
                           (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'grass 2 0))
                           (list (tile 'bush 0 1) (tile 'grass 1 1) (tile 'blank 2 1))
                           (list (tile 'bush 0 2) (tile 'blank 1 2) (tile 'grass 2 2)))
                          1 2 'grass)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'grass 2 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) (tile 'blank 2 1))
                 (list (tile 'blank 0 2) (tile 'tree  1 2) (tile 'blank 2 2))))
  (check-equal? (collapse b4 0 2 'bush)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0)
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) 
                       (tile 'grass 2 1) (tile 'blank 3 1))
                 (list (tile 'hut   0 2) (tile 'blank 1 2) 
                       (tile 'blank 2 2) (tile 'blank 3 2))
                 (list (tile 'blank 0 3) (tile 'blank 1 3) 
                       (tile 'grass 2 3) (tile 'blank 3 3)))))

; tile-val? : board num num symbol -> boolean
;  Returns true if the value on tile ('x','y') is 'v'

(define (tile-val? b x y v)
  (and (on-board? x y (length b))
       (symbol=? (tile-v (get-tile b x y))
                 v)))

(module+ test
  (check-equal? (tile-val? t1 1 1 'grass) #t)
  (check-equal? (tile-val? t1 1 1 'hut) #f))

; swap-store-house : board symbol -> (values board symbol)
;  Swaps the store house with tile with 'v'

(define (swap-store-house b v)
  (if (symbol=? (tile-v (get-tile b 0 0)) 'blank)
      (values (replace b 0 0 v) (gen-input))
      (values (replace b 0 0 v) (tile-v (get-tile b 0 0)))))

; values->list : values -> list
;  Converts a set of values into a list of the same values

(define-syntax-rule (values->list a)
  (call-with-values (λ () a) list))

(module+ test
  (check-equal? (values->list (swap-store-house t1 'bear))
                (list (list (list (tile 'bear 0 0) (tile 'blank 1 0)) 
                            (list (tile 'blank 0 1) (tile 'grass 1 1)))
                      'grass)))


; crystal-count : board num num -> list
;  Return a list of all possible collapsable neighbours of ('x','y')

(define (crystal-count b x y)
  (for/list ([i crystal-list]
             #:when (> (cadr 
                        (count-neighbours (replace b x y i) x y i empty)) 
                       2))
    list i))

(module+ test
  (check-equal? (crystal-count b5 1 0) (list 'bush))
  (check-equal? (crystal-count b5 3 3) (list 'tombstone))
  (check-equal? (crystal-count b5 3 0) empty))

; collapse-crystal : board num num -> boolean [board-or-#f]
;  Places crystal at ('x','y') and s 'b'

(define (crystal-collapse b x y)
  (let ([l (crystal-count  b x y)])
    (cond
      [(empty? l) (replace b x y 'rock)]
      [(cons? l)
       (collapse b x y (first l))])))

(module+ test
  (check-equal? (crystal-collapse b1 2 1) 
                (list
                 (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'bush 2 1))
                 (list (tile 'blank 0 2) (tile 'blank 1 2) (tile 'blank 2 2))))
  (check-equal? (crystal-collapse b1 2 0)
                (list
                 (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'rock 2 0))
                 (list (tile 'grass 0 1) (tile 'hut 1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'grass 1 2) (tile 'grass 2 2)))))


; decide-move : board num num symbol -> (values board boolean [symbol-or-false])
;                                       a. symbol means call decide-move with new x y
;                                       b. false means move complete; go to next move
;  Decide whether move is 
;   - store-house
;   - imperial-robot
;   - crystal
;   - select chest or large-chest 

(define (decide-move b x y v)
  (cond
    [(empty? b) (values empty #f)]
    [(false? b) (values b #f)]
    [(and (equal? x 0) (equal? y 0))
     (swap-store-house b v)]
    [(not (on-board? x y (length b)))
     (values b #f)]
    [else
     (case (tile-v (get-tile b x y))
       [(blank) ; (x,y) is blank
        (cond
          ; imperial-robot
          [(symbol=? v 'imperial-robot)
           (values b v)]
          ; crystal
          [(symbol=? v 'crystal)
           (values (crystal-collapse b x y) #f)]
          [else (values (collapse b x y v) #f)])]
       ; chest or large-chest
       [(chest large-chest) 
        (values (replace b x y 'blank) v)]
       ; bear or ninja-bear
       [(bear ninja-bear)
        (if (symbol=? 'imperial-robot v)
            (values (collapse b x y 'tombstone) #f)
            (values b v))]
       ; everything else
       [else 
        (if (symbol=? v 'imperial-robot)
            (values (replace b x y 'blank) #f)
            (values b v))])]))

(module+ test
  (check-equal? (values->list (decide-move #f -1 -1 'blank))
                (list #f #f))
  (check-equal? (values->list (decide-move empty -1 -1 'blank))
                (list empty #f))
  (check-equal? (values->list (decide-move b1 -1 -1 'blank))
                (list b1 #f)))

;read-inputs : board symbol -> (values num num)
; Displays next tile 'v' and reads 'x'  and 'y' coordinate inputs

(define (read-inputs b v)
  (display-board b)
  (printf "Tile - ~a\n" v)
  (printf "Enter (x,y) - \n")
  (values (read) (read)))

;move-bears : board -> board
; Moves 'bear s and 'ninja-bear s

(define (move-bears b) b)

;move : board -> boolean
; Displays board, reads new tile, collapses board, displays board

(define (move b)
  (cond
    [(empty? b) #f]
    [(cons? b)
     (let loop ([board b]
                [val (gen-input)])
       (let-values ([(x y) (read-inputs  board val)])
         (let-values ([(b1 v1) (decide-move board x y val)])
           (if (not (false? v1)) ;when v1 is #t
               (loop b1 v1)
               (move-bears b1)))))]))

; generate-board : num -> board
;  Generates an empty board of size n X n
(define (generate-board n)
  (for/list ([i (range n)])
    (for/list ([j (range n)])
      list (tile 'blank i j))))

; main-game-loop : void -> void
;  Runs the main loop of the game. Runs infinitely.
(define (main-game-loop)
  (let loop ([b (generate-board 6)])
    (loop (move b))))
    
;display-board-row : list-of-tiles -> list-of-tiles
; Displays a row of tiles

(define (display-board-row row)
  (cond
    [(empty? row) empty]
    [(cons? row)
     (let ([s (symbol->string (tile-v (first row)))])
       (printf "~a" s)
       (for ([i (range (- 15 (string-length s)))])
         (printf "~a" " "))
       (printf "|"))
     (display-board-row (rest row))]))

;display-board : board -> board
; Displays a board of tiles

(define (display-board board)
  (cond
    [(empty? board) empty]
    [(cons? board)
     (display-board-row (first board))
     (printf "\n")
     (display-board (rest board))]))

;(decide-move b3 0 0 'grass)
;(display-board (move (move (move b5))))
