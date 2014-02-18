#lang racket

(provide (all-defined-out))

; A board is a non-empty list

; A tile is (make-tile symbol num num)
(define-struct tile (v x y) #:transparent)

; Alias for false?
(define not false?)

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

;definitions
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