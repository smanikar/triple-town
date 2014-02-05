#lang racket

(define input-list (list 'grass 'bush 'tree 'hut 'crystal 'imperial-bot 'bear 'ninja-bear))

(define expected '(61 15 2 1 2 3 15 1))

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
(define n 100000.0)

(define (incr-list i l)
  (let-values ([(f r) (split-at l i)])
    (flatten (list f (add1 (car r)) (cdr r)))))

(define (make-percent l n)
  (for/list ([i l])
    (/ (* i 100) n)))

(define (get-pos l v i)
  (cond
    [(empty? l) #f]
    [(cons? l)
     (if (symbol=? (first l) v)
         i
         (get-pos (rest l) v (add1 i)))]))

(define (test l n)
  (let loop ([i 0]
             [m (incr-list (get-pos input-list (gen-input) 0) l)])
    (if (< i n)
        (loop (add1 i) (incr-list (get-pos input-list (gen-input) 0) m))
        (make-percent m n))))

(define (diff-list l1 l2)
  (for/list ([i l1]
             [j l2])
    (abs (- i j))))

(define (trunc l)
  (for/list ([i l])
    (/ (floor (* i 100)) 100)))

(define (main-program)
  (let ([l (test (make-list (length input-list) 0) 10000.0)])
    (printf "~a\n" expected)
    (printf "~a\n" l)
    (printf "~a\n" (trunc (diff-list l expected)))))

(main-program)
