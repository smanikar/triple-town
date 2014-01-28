#lang racket

(define input-list (list 'grass 'bush 'tree 'hut 'crystal 'imperial-bot 'bear 'ninja-bear))

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
    

(define (test-gen-input)
  (let ([l (list 0 0 0 0 0 0 0 0)])
    (for ([i (range n)])
      (let ([inp (gen-input)])
        (cond
          [(symbol=? (list-ref input-list 0) inp)
           (set! l (incr-list 0 l))]
          [(symbol=? (list-ref input-list 1) inp)
           (set! l (incr-list 1 l))]
          [(symbol=? (list-ref input-list 2) inp)
           (set! l (incr-list 2 l))]
          [(symbol=? (list-ref input-list 3) inp)
           (set! l (incr-list 3 l))]
          [(symbol=? (list-ref input-list 4) inp)
           (set! l (incr-list 4 l))]
          [(symbol=? (list-ref input-list 5) inp)
           (set! l (incr-list 5 l))]
          [(symbol=? (list-ref input-list 6) inp)
           (set! l (incr-list 6 l))]
          [(symbol=? (list-ref input-list 7) inp)
           (set! l (incr-list 7 l))]
          [else (print "else")])))
    (make-percent l n)))

(test-gen-input)
'(61.01 15.01 2.01 1.01 2.01 3.01 15.01 1.01)