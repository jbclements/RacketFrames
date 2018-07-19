#lang typed/racket

(: args (Vectorof String))
(define args (current-command-line-arguments))

(when (not (= (vector-length args) 2))
    (error "UsageL racket rolling-sum.rkt <size> <iterations>"))

(: size Integer)
(define size (assert (string->number (vector-ref args 0)) exact-integer?))

(: count Integer)
(define count (assert (string->number (vector-ref args 1)) exact-integer?))

(: build-lst (Integer -> (Listof Integer)))
(define (build-lst size)
  (for/list ([i size])
    i))

(: reverse-lst ((Listof Integer) -> (Listof Integer)))
(define (reverse-lst lst)
  (let loop : (Listof Integer) ([lst : (Listof Integer) lst] [lst-reversed : (Listof Integer) '()])
    (if (empty? lst)
        lst-reversed
        (loop (rest lst) (cons (first lst) lst-reversed)))))

(: reverse-iterations (Integer (Listof Integer) -> Void))
(define (reverse-iterations count lst)
  (for ([i count])
    (reverse-lst lst)))

(: main (-> Void))
(define (main)
  (reverse-iterations count (build-lst size)))
