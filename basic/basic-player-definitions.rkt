#lang racket

(require "basic-moves-definitions.rkt")

(provide (all-defined-out))

(define-struct (my-exception exn:fail:user) ())

(define storehouse-points-hash
  (hash 'blank 0
        'grass 25
        'bush 145
        'tree 745
        'imperial-robot 1745
        'hut 4245
        'crystal 14245))

(define imp-bot-points-hash
  (hash 'grass 4
        'bush 3
        'tree 2
        'hut 1
        'house 0
        'mansion -1
        'castle -2
        'floating-castle -3
        'triple-castle -4))

(define b9 
  (list
   (list (tile 'grass 0 0) (tile 'tree 1 0) (tile 'house 2 0) 
         (tile 'house 3 0) (tile 'blank 4 0) (tile 'blank 5 0))
   (list (tile 'castle 0 1) (tile 'castle 1 1) (tile 'blank 2 1) 
         (tile 'mansion 3 1) (tile 'blank 4 1) (tile 'blank 5 1))
   (list (tile 'bush 0 2) (tile 'floating-castle 1 2) (tile 'floating-castle 2 2) 
         (tile 'mansion 3 2) (tile 'blank 4 2) (tile 'blank 5 2))
   (list (tile 'bush 0 3) (tile 'castle 1 3) (tile 'tree 2 3) 
         (tile 'tree 3 3) (tile 'grass 4 3) (tile 'blank 5 4))
   (list (tile 'blank 0 4) (tile 'blank 1 4) (tile 'blank 2 4)
         (tile 'grass 3 4) (tile 'blank 4 4) (tile 'blank 5 4))
   (list (tile 'grass 0 5) (tile 'blank 1 5) (tile 'blank 2 5)
         (tile 'blank 3 5) (tile 'blank 4 5) (tile 'tree 5 5))))

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

(define t4 (list (list (tile 'bush 0 0) (tile 'blank 1 0) (tile 'bush  2 0))
                 (list (tile 'grass 0 1) (tile 'grass 1 1) (tile 'bush 2 1))
                 (list (tile 'blank 0 2) (tile 'blank 1 2) (tile 'blank 2 2))))

(define t3
  (list
   (list (tile 'bush 0 0) (tile 'blank 1 0) (tile 'blank 2 0) (tile 'blank 3 0))
   (list (tile 'blank 0 1) (tile 'blank 1 1) (tile 'bush 2 1) (tile 'bush 3 1))
   (list (tile 'blank 0 2) (tile 'grass 1 2) (tile 'grass 2 2) (tile 'blank 3 2))
   (list (tile 'blank 0 3) (tile 'blank 1 3) (tile 'blank 2 3) (tile 'blank 3 3))))

(define r1
  "<game><board><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"mansion\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"house\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"grass\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"bush\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row></board><current><tile value=\"crystal\"></tile></current><storehouse><tile value=\"blank\"></tile></storehouse></game>")
(define in1 "<board><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"mansion\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"house\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"grass\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"bush\"></tile></cell></row><row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"tree\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row></board>")
(define in2 "<board><row><cell><tile value =\"blank\"></tile></cell></row><row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row></board>")
  (define in3 "<row><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"hut\"></tile></cell><cell><tile value=\"blank\"></tile></cell><cell><tile value=\"blank\"></tile></cell></row>")
  (define in4 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (define in5 "<row><cell><tile value =\"castle\"></tile></cell><cell><tile value =\"castle\"></tile></cell><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"mansion\"></tile></cell><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"blank\"></tile></cell></row>")
  (define in6 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (define in7 "<current><tile value =\"crystal\"></tile></current>")
  (define in8 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (define in9 "<current><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></current>")
  (define in10 "<current><cell><tile value =\"blank\"></tile></cell></current>")
  (define in11 "<current value =\"blank\"></current>")
  (define in12 "<storehouse><tile value =\"crystal\"></tile></storehouse>")
  (define in13 "<row><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></row>")
  (define in14 "<storehouse><cell><tile value =\"blank\"></tile></cell><cell><tile value =\"grass\"></tile></cell></storehouse>")
  (define in15 "<storehouse><cell><tile value =\"blank\"></tile></cell></storehouse>")
  (define in16 "<storehouse value =\"blank\"></storehouse>")
