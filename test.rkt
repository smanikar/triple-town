#lang racket
(require rackunit)
(define-struct tile (x (y #:mutable)) #:transparent)

;(define (func x)
;  (set! x (add1 x))
;  (display x))
;
;(define (test y)
;  (set! y (- y 100))
;  (set! y (func y))
;  (display  y))
;
;(test 5)

(define (show-tile tile)
  (tile-x tile))


(for-each 
 (lambda (xyz)
   (printf "got ~a \n" xyz) 
   23)
 '(1 2 3 4))

;(define mylist (list( list 1 2 3 4) 
;                    (list 4 3 2 1)))
;(define (nexted-loop lists)
;  (for-each
;   (lambda (abc)
;     (for-each 
;      (lambda (xyz)
;        (printf "got ~a \n" xyz)
;        ;(print (first lists))
;        )
;      abc))
;   lists))
;(print (nexted-loop mylist))

((lambda (x) x) 10)

;(tile 1 2)
(define x 22222)
;(show-tile (tile 2 1))
(define lone (list (tile 1 2) (tile 2 27) (tile 3 1) (tile 4 4)))

(define new-tile (struct-copy tile (second lone)))
;printf "new tile -" (tile-x new-tile) (tile-y new-tile)

;print x
;
;print "Check eq"
;(check-equal? (tile 2 21) (tile 2 21))
;(tile-x (list-ref lone 1))
;(print-struct)
;(define mylist (tile 25 35))
;printf "Mylist-" (tile-y mylist)
;(set-tile-y! mylist 33)
;printf "Mylist-" (tile-y mylist)
;
;printf "lone-"  (tile-y (third lone))
;(set-tile-y! (third lone) 33)
;printf "lone" (tile-y (third lone))