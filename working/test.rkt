#lang racket
(require rackunit)
(define-struct tile (x (y #:mutable)) #:transparent)

(define (my-func x y)
  (values (add1 x) (add1 y)))

(define (values->list a)
  (call-with-values (λ () a) list))

(define-syntax-rule (check-values (values v ...) e)
    (check-equal? (list v ...) (call-with-values (lambda () e) list)))

  ;; Example:
(check-values (values (+ 1 2) (+ 3 4))
              (values 3 7))
;(check-values (my-func 2 3) (values 3 4))


;(define (loop-until start done? next f)
;  (let loop ([i start])
;    (unless (done? i)
;      (f i)
;      (loop (next i)))))
;
;(display (loop-until 5
;            (λ (x) (> x 10))
;            (λ (x) (add1 x))
;            (λ (x) (printf "~a\n" x))))

;(define (funct)
;  (let loop ([i 5]
;             [j 12]
;             [k -10])
;    (unless (> (+ i j k) 12)
;      (printf "~a ~a ~a\n" i j k)
;      (loop (add1 i)
;            (sub1 j)
;            (add1 k)))))
;
;(funct)
;
;(define (foo x )
;  (if (> x 3)
;      (define a 7)
;      (define b 8)
;      (+ a b x)
;      x))
;
;(foo 4)

;(define (fun3 x)
;  (if (> x 3) 
;      9
;      -1)
;  (if (< x 3)
;      8
;      -2))
;(printf "~a\n" (fun3 4))
;
;(printf "~a\n" 
;        (let ([x 5][y 10])
;          (+ x y)))
;
;(define (fun1 x)
;  (cond
;    [ (< x 0) (+ x 10)]
;    [ (> x 0) (- x 9)]
;    [ (equal? x 0)  23]))
;
;(define (less x)
;  (if (< x 0) (+ x 10) x))
;
;(define (more x)
;  (if (> x 0) (- x 9) x))
;
;(define (equal x)
;  (if (equal? x 0) 23 x))
;
;(define (fun2 x)
;  (equal (more (less x))))
;
;(printf "~a \n" (fun2 -1))      
;
;(printf "~a \n" (fun1 -1))

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


;(for-each 
; (lambda (xyz)
;   (printf "got ~a \n" xyz) 
;   23)
; '(1 2 3 4))

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

;((lambda (x) x) 10)

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