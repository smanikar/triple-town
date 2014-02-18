#lang racket

(require "basic-moves-definitions.rkt")

(provide (all-defined-out))

(define b9 
  (list
   (list (tile 'blank 0 0) (tile 'tree 1 0) (tile 'house 2 0) 
         (tile 'house 3 0) (tile 'blank 4 0) (tile 'blank 5 0))
   (list (tile 'castle 0 1) (tile 'castle 1 1) (tile 'blank 2 1) 
         (tile 'mansion 3 1) (tile 'blank 4 1) (tile 'blank 5 1))
   (list (tile 'bush 0 2) (tile 'floating-castle 1 2) (tile 'floating-castle 2 2) 
         (tile 'mansion 3 2) (tile 'blank 4 2) (tile 'blank 5 2))
   (list (tile 'bush 0 3) (tile 'castle 1 3) (tile 'tree 2 3) 
         (tile 'tree 3 3) (tile 'blank 4 4) (tile 'blank 5 4))
   (list (tile 'blank 0 4) (tile 'blank 1 4) (tile 'blank 2 4)
         (tile 'blank 3 4) (tile 'blank 4 4) (tile 'blank 5 4))
   (list (tile 'blank 0 4) (tile 'blank 1 4) (tile 'blank 2 4)
         (tile 'blank 3 4) (tile 'blank 4 4) (tile 'blank 5 4))))

(define b10 
  (list
   (list (tile 'blank 0 0) (tile 'tree 1 0) (tile 'house 2 0) 
         (tile 'tree 3 0) (tile 'house 4 0) (tile 'tree 5 0))
   (list (tile 'tree 0 1) (tile 'house 1 1) (tile 'tree 2 1) 
         (tile 'house 3 1) (tile 'tree 4 1) (tile 'house 5 1))
   (list (tile 'house 0 2) (tile 'tree 1 2) (tile 'house 2 2) 
         (tile 'tree 3 2) (tile 'house 4 2) (tile 'tree 5 2))
   (list (tile 'tree 0 3) (tile 'house 1 3) (tile 'tree 2 3) 
         (tile 'house 3 3) (tile 'tree 4 4) (tile 'house 5 4))
   (list (tile 'house 0 4) (tile 'tree 1 4) (tile 'house 2 4)
         (tile 'tree 3 4) (tile 'house 4 4) (tile 'tree 5 4))
   (list (tile 'tree 0 4) (tile 'house 1 4) (tile 'tree 2 4)
         (tile 'house 3 4) (tile 'tree 4 4) (tile 'house 5 4))))

(define b11
  (list
   (list (tile 'blank 0 0) (tile 'grass 1 0) (tile 'bush 2 0) 
         (tile 'blank 3 0) (tile 'blank 4 0) (tile 'blank 5 0))
   (list (tile 'hut 0 1) (tile 'grass 1 1) (tile 'bush 2 1) 
         (tile 'bush 3 1) (tile 'blank 4 1) (tile 'blank 5 1))
   (list (tile 'hut 0 2) (tile 'blank 1 2) (tile 'bush 2 2) 
         (tile 'mansion 3 2) (tile 'blank 4 2) (tile 'blank 5 2))
   (list (tile 'tree 0 3) (tile 'tree 1 3) (tile 'bush 2 3) 
         (tile 'mansion 3 3) (tile 'blank 4 4) (tile 'blank 5 4))
   (list (tile 'blank 0 4) (tile 'blank 1 4) (tile 'blank 2 4)
         (tile 'blank 3 4) (tile 'blank 4 4) (tile 'blank 5 4))
   (list (tile 'blank 0 4) (tile 'blank 1 4) (tile 'blank 2 4)
         (tile 'blank 3 4) (tile 'blank 4 4) (tile 'blank 5 4))))