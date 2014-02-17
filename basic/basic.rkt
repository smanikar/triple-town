; Srikanth Manikarnike
; U0706564

#lang racket
(module+ test (require rackunit))

(provide (all-defined-out))

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
    [(symbol=? cur-tile 'floating-castle) 'triple-castle]))

(module+ test 
  (check-equal? (next-tile 'grass) 'bush)
  (check-equal? (next-tile 'bush)  'tree)
  (check-equal? (next-tile 'tree)  'hut)
  (check-equal? (next-tile 'hut) 'house)
  (check-equal? (next-tile 'house) 'mansion)
  (check-equal? (next-tile 'mansion) 'castle)
  (check-equal? (next-tile 'castle) 'floating-castle)
  (check-equal? (next-tile 'floating-castle) 'triple-castle))

; A tile is (make-tile symbol num num)
(define-struct tile (v x y) #:transparent)

; A board is a non-empty list

; Alias for false?
(define not false?)

; true? boolean -> boolean
;  Returns if value v is true
(define (true? v) (not (false? v)))

(module+ test 
  (check-equal? (true? #t) #t)
  (check-equal? (true? #f) #f))

;A list of all possible inputs
(define input-list (list 
                    'grass 
                    'bush 
                    'tree 
                    'hut 
                    'crystal 
                    'imperial-robot))

; A list of all possible tiles that can be collapsed by a crystal
(define crystal-list '(floating-castle castle mansion cathedral 
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
                       (tile 'grass 2 1) (tile 'tree  3 1))
                 (list (tile 'bush  0 2) (tile 'bush  1 2) 
                       (tile 'tombstone 2 2) (tile 'blank 3 2))
                 (list (tile 'tree  0 3) (tile 'hut  1 3)
                       (tile 'tombstone 2 3) (tile 'blank 3 3))))

(define b6 (list (list (tile 'blank 0 0) (tile 'grass 1 0) 
                       (tile 'bush 2 0) (tile 'blank 3 0))
                 (list (tile 'hut  0 1) (tile 'grass  1 1) 
                       (tile 'bush 2 1) (tile 'bush  3 1))
                 (list (tile 'hut  0 2) (tile 'blank  1 2) 
                       (tile 'bush 2 2) (tile 'mansion 3 2))
                 (list (tile 'tree  0 3) (tile 'tree  1 3) 
                       (tile 'bush 2 3) (tile 'mansion 3 3))))

(define b7 (list (list (tile 'blank 0 0) (tile 'tree 1 0) 
                       (tile 'house 2 0) (tile 'house 3 0))
                 (list (tile 'castle  0 1) (tile 'castle  1 1) 
                       (tile 'blank 2 1) (tile 'mansion  3 1))
                 (list (tile 'bush  0 2) (tile 'floating-castle  1 2) 
                       (tile 'floating-castle 2 2) (tile 'mansion 3 2))
                 (list (tile 'bush  0 3) (tile 'floating-castle  1 3) 
                       (tile 'tree 2 3) (tile 'tree 3 3))))

(define b8 (list (list (tile 'blank 0 0) (tile 'tree 1 0) 
                       (tile 'house 2 0) (tile 'house 3 0))
                 (list (tile 'castle  0 1) (tile 'castle  1 1) 
                       (tile 'blank 2 1) (tile 'mansion  3 1))
                 (list (tile 'bush  0 2) (tile 'floating-castle  1 2) 
                       (tile 'floating-castle 2 2) (tile 'mansion 3 2))
                 (list (tile 'bush  0 3) (tile 'castle  1 3) 
                       (tile 'tree 2 3) (tile 'tree 3 3))))

;gen:input : none -> symbol
; Generates a symbol from 0 to 7 based on weightes

(define (gen-input)
  (let ([n (random 100)])
    (cond
      [(< n 73) (list-ref input-list 0)]
      [(< n 91) (list-ref input-list 1)]
      [(< n 93) (list-ref input-list 2)]
      [(< n 94) (list-ref input-list 3)]
      [(< n 96) (list-ref input-list 4)]
      [(< n 100) (list-ref input-list 5)]
      [else (error "random number")])))

;get-tile: boards num num -> tile-or-#f
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile b x y)
  (list-ref (list-ref b y) x))

(module+ test
  ;(check-equal? (get-tile empty 0 0) #f)
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
  (check-equal? (on-board? 3 0 (length b2)) #f)
  (check-equal? (on-board? 2 2 (length b2)) #t)
  )

;valid-neighbour? : board num num symbol list -> boolean
; Checks if a given tile is a valid neighbour based on 'v'

(define (valid-neighbour? b x y v l)
  (and (on-board? x y (length b))
       (let ([t (get-tile b x y)])
         (and (symbol=? (tile-v t) v)
              (not (member t l))))))

(module+ test
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
  (check-equal? (replace-row (list (tile 'hut 0 0)) 2 'reddit)   
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

; collapse-at* : board num num sybmol -> boolean [board-or-#f]
;  Collapse board multiple times until no more collapses are possible

(define (collapse-at* b x y v)
  (let ([bb (replace b x y v)])
    (let loop ([board bb]
               [value v]
               [count (cadr (count-neighbours bb x y v empty))])
      (if (or (and (symbol=? value 'floating-castle) (> count 3))
              (and (not (symbol=? value 'floating-castle)) (> count 2)))
          (let* ([b1 (replace (car (replace-neighbours board x y value empty))
                              x y (next-tile value))]
                 [c1 (cadr (count-neighbours b1 x y (next-tile value) empty))]
                 [v1 (next-tile value)])
            (loop b1 v1 c1))
          board))))

(module+ test
  (check-equal? (collapse-at* (list (list (tile 'blank 0 0) (tile 'grass 1 0)) 
                                    (list (tile 'blank 0 1) (tile 'grass 1 1)))
                              0 1 'grass)
                (list (list (tile 'blank 0 0) (tile 'blank 1 0)) 
                      (list (tile 'bush  0 1) (tile 'blank 1 1))))
  ;(check-equal? (collapse-at* t1 1 1 'grass) t1)
  (check-equal? (collapse-at* (list
                               (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'grass 2 0))
                               (list (tile 'bush 0 1) (tile 'grass 1 1) (tile 'blank 2 1))
                               (list (tile 'bush 0 2) (tile 'blank 1 2) (tile 'grass 2 2)))
                              1 2 'grass)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'grass 2 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) (tile 'blank 2 1))
                 (list (tile 'blank 0 2) (tile 'tree  1 2) (tile 'blank 2 2))))
  (check-equal? (collapse-at* b4 0 2 'bush)
                (list
                 (list (tile 'blank 0 0) (tile 'blank 1 0)
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) 
                       (tile 'grass 2 1) (tile 'blank 3 1))
                 (list (tile 'hut   0 2) (tile 'blank 1 2) 
                       (tile 'blank 2 2) (tile 'blank 3 2))
                 (list (tile 'blank 0 3) (tile 'blank 1 3) 
                       (tile 'grass 2 3) (tile 'blank 3 3))))
  (check-equal? (collapse-at* b7 2 1 'house)
                (list
                 (list (tile 'blank 0 0) (tile 'tree 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) 
                       (tile 'triple-castle 2 1) (tile 'blank 3 1))
                 (list (tile 'bush 0 2) (tile 'blank 1 2) 
                       (tile 'blank 2 2) (tile 'blank 3 2))
                 (list (tile 'bush 0 3) (tile 'blank 1 3) 
                       (tile 'tree 2 3) (tile 'tree 3 3))))
  (check-equal? (collapse-at* b8 2 1 'house)
                (list
                 (list (tile 'blank 0 0) (tile 'tree 1 0) 
                       (tile 'blank 2 0) (tile 'blank 3 0))
                 (list (tile 'blank 0 1) (tile 'blank 1 1) 
                       (tile 'floating-castle 2 1) (tile 'blank 3 1))
                 (list (tile 'bush 0 2) (tile 'floating-castle 1 2) 
                       (tile 'floating-castle 2 2) (tile 'blank 3 2))
                 (list (tile 'bush 0 3) (tile 'castle 1 3) 
                       (tile 'tree 2 3) (tile 'tree 3 3)))))

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
  (check-equal? (values->list (swap-store-house t1 'tree))
                (list (list (list (tile 'tree 0 0) (tile 'blank 1 0)) 
                            (list (tile 'blank 0 1) (tile 'grass 1 1)))
                      'grass)))

; collapse-crystal : board num num -> boolean [board-or-#f]
;  Places crystal at ('x','y') and s 'b'

(define (crystal-collapse b x y)
  (let ([v (for/first ([i crystal-list]
                       #:when (> (cadr (count-neighbours 
                                        (replace b x y i) x y i empty)) 
                                 2)) i)])
    (if (false? v)
        b
        (collapse-at* b x y v))))

(module+ test
  (check-equal? (crystal-collapse b1 2 1) 
                (list
                 (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
                 (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'bush 2 1))
                 (list (tile 'blank 0 2) (tile 'blank 1 2) (tile 'blank 2 2))))
  (check-equal? (crystal-collapse b1 2 0)
                (list
                 (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
                 (list (tile 'grass 0 1) (tile 'hut 1 1) (tile 'blank 2 1))
                 (list (tile 'grass 0 2) (tile 'grass 1 2) (tile 'grass 2 2))))
  (check-equal? (crystal-collapse b7 2 1)
                (list
                 (list (tile 'blank 0 0) (tile 'tree 1 0) 
                       (tile 'house 2 0) (tile 'house 3 0))
                 (list (tile 'castle 0 1) (tile 'castle 1 1) 
                       (tile 'triple-castle 2 1) (tile 'mansion 3 1))
                 (list (tile 'bush 0 2) (tile 'blank 1 2) 
                       (tile 'blank 2 2) (tile 'mansion 3 2))
                 (list (tile 'bush 0 3) (tile 'blank 1 3) 
                       (tile 'tree 2 3) (tile 'tree 3 3))))
  (check-equal? (crystal-collapse b8 2 1)
                (list
                 (list (tile 'blank 0 0) (tile 'tree 1 0) 
                       (tile 'house 2 0) (tile 'house 3 0))
                 (list (tile 'castle 0 1) (tile 'castle 1 1) 
                       (tile 'floating-castle 2 1) (tile 'mansion 3 1))
                 (list (tile 'bush 0 2) (tile 'floating-castle 1 2) 
                       (tile 'floating-castle 2 2) (tile 'mansion 3 2))
                 (list (tile 'bush 0 3) (tile 'castle 1 3) 
                       (tile 'tree 2 3) (tile 'tree 3 3)))))


; decide-move : board num num symbol -> (values board boolean [symbol-or-false])

;  Decide whether move is 
;   - store-house
;   - imperial-robot
;   - crystal
;   - others
;  and take corresponding action

;    a. (board symbol) means call decide-move with new x y and old v
;    b. (board #f)     means call decide-move with new x y and v

(define (decide-move b x y v)
  (cond
    [(and (equal? x 0) (equal? y 0))
     (printf "Store-house swap - (~a) and (~a)\n" 
             v (tile-v (get-tile b x y)))
     (swap-store-house b v)]
    [(not (on-board? x y (length b)))
     (printf "(~a, ~a) is outside the board\n" x y)
     (values b v)]
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
          [else (values (collapse-at* b x y v) #f)])]
       ; everything else
       [else 
        (if (symbol=? v 'imperial-robot)
            (values (replace b x y 'blank) #f)
            (values b v))])]))

(module+ test
  
  (check-equal? 
   (values->list (decide-move t1 0 0 'tree))
   (list (list (list (tile 'tree 0 0) (tile 'blank 1 0)) 
               (list (tile 'blank 0 1) (tile 'grass 1 1)))
         'grass))
  
  (check-equal? 
   (values->list (decide-move t1 1 1 'imperial-robot))
   (list (list (list (tile 'grass 0 0) (tile 'blank 1 0))
               (list (tile 'blank 0 1) (tile 'blank 1 1)))
         #f))
  
  (check-equal? 
   (values->list (decide-move t1 1 0 'imperial-robot))
   (list (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
               (list (tile 'blank 0 1) (tile 'grass 1 1)))
         'imperial-robot))
  
  (check-equal?
   (values->list (decide-move b1 2 1 'crystal))
   (list
    (list
     (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
     (list (tile 'blank 0 1) (tile 'hut 1 1) (tile 'bush 2 1))
     (list (tile 'blank 0 2) (tile 'blank 1 2) (tile 'blank 2 2)))
    #f))
  
  (check-equal?
   (values->list (decide-move b1 2 0 'crystal))
   (list
    (list
     (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
     (list (tile 'grass 0 1) (tile 'hut 1 1) (tile 'blank 2 1))
     (list (tile 'grass 0 2) (tile 'grass 1 2) (tile 'grass 2 2)))
    #f))
  
  (check-equal?
   (values->list 
    (decide-move (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                       (list (tile 'hut 0 1) (tile 'grass 1 1)))
                 0 1 'crystal))
   (list
    (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
          (list (tile 'hut 0 1) (tile 'grass 1 1)))
    'crystal))
  
  (check-equal?
   (values->list 
    (decide-move (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                       (list (tile 'castle 0 1) (tile 'grass 1 1)))
                 0 1 'crystal))
   (list
    (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
          (list (tile 'castle 0 1) (tile 'grass 1 1)))
    'crystal))
  
  (check-equal?
   (values->list 
    (decide-move (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                       (list (tile 'castle 0 1) (tile 'grass 1 1)))
                 0 1 'imperial-robot))
   (list
    (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
          (list (tile 'blank 0 1) (tile 'grass 1 1)))
    #f))
  
  (check-equal?
   (values->list 
    (decide-move (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
                       (list (tile 'tree 0 1) (tile 'grass 1 1)))
                 0 1 'imperial-robot))
   (list
    (list (list (tile 'grass 0 0) (tile 'blank 1 0)) 
          (list (tile 'blank 0 1) (tile 'grass 1 1)))
    #f)))

;read-inputs : board symbol -> (values num num)
; Displays next tile 'v' and reads 'x'  and 'y' coordinate inputs

(define (read-inputs b v)
  (display-board b)
  (printf "Tile - ~a\n" v)
  (printf "Enter exact nonnegative integers x y - \n")
  ;(values (read) (read)))
  (let loop ([x (read)]
             [y (read)])
    (cond 
      [(and (exact-nonnegative-integer? x)
            (exact-nonnegative-integer? y))
       (values x y)]
      [else 
       (printf "Enter exact nonnegative integers x y - \n")
       (loop (read) (read))])))

;move : board -> boolean
; Displays board, reads new tile, collapses board, displays board

(define (move b)
  (let loop ([board b]
             [val (gen-input)])
    (let-values ([(x y) (read-inputs  board val)])
      (let-values ([(b1 v1) (decide-move board x y val)])
        (if (true? v1) ;when v1 is #t
            (loop b1 v1)
            b1)))))

; generate-board : num -> board
;  Generates an empty board of size n X n, integer n >= 0
(define (generate-board n)
  (for/list ([i (range n)])
    (for/list ([j (range n)])
      list (tile 'blank i j))))

(module+ test
  (check-equal? (generate-board 0) '())
  (check-equal? (generate-board 2)
                (list (list (tile 'blank 0 0) (tile 'blank 0 1)) 
                      (list (tile 'blank 1 0) (tile 'blank 1 1)))))

; end-game-row? : list-of-tiles -> boolean
;  Returns false if the row has at-least one blank tile

(define (end-game-row? r)
  (cond
    [(empty? r) empty]
    [(cons? r)
     (let* ([x (tile-x (first r))]
            [y (tile-y (first r))]
            [v (tile-v (first r))])
       (if (and (equal? x 0)
                (equal? y 0))
           (and #t (end-game-row? (rest r)))
           (if (symbol=? v 'blank)
               (and #f (end-game-row? (rest r)))
               (and #t (end-game-row? (rest r))))))]))


(module+ test 
  (check-equal? (end-game-row? (list (tile 'blank 0 0) (tile 'blank 0 1)))
                #f)
  (check-equal? (end-game-row? (list (tile 'blank 0 0) (tile 'grass 0 1)))
                '())
  (check-equal? (end-game-row? (list (tile 'grass 0 0) (tile 'grass 0 1)))
                '())
  (check-equal? (end-game-row? (list (tile 'grass 0 0) (tile 'blank 0 1)))
                #f))

; end-game? : board -> boolean
;  Returns false the board has at-least one blank tile

(define (end-game? b)
  (cond
    [(empty? b) empty]
    [(cons? b)
     (and (end-game-row? (first b))
          (end-game? (rest b)))]))

(module+ test 
  (check-equal? (end-game? (list (list (tile 'blank 0 0) (tile 'blank 0 1)) 
                                 (list (tile 'blank 1 0) (tile 'blank 1 1))))
                #f)
  (check-equal? (end-game? (list (list (tile 'blank 0 0) (tile 'grass 0 1)) 
                                 (list (tile 'grass 1 0) (tile 'grass 1 1))))
                '())
  (check-equal? (end-game? (list (list (tile 'grass 0 0) (tile 'grass 0 1)) 
                                 (list (tile 'blank 1 0) (tile 'grass 1 1))))
                #f)
  (check-equal? (end-game? (list (list (tile 'grass 0 0) (tile 'grass 0 1)) 
                                 (list (tile 'grass 1 0) (tile 'grass 1 1))))
                '()))

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

; main-game-loop : num -> void
;  Runs the main loop of the game. Runs infinitely.
(define (main-game-loop n)
  (let loop ([b (generate-board n)])
    (if (not (end-game? b))
        (loop (move b))
        (printf "End of game!\n ~a" (display-board b)))))

; Runs the main loop
;(main-game-loop 6)