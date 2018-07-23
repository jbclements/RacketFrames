#lang racket

(define args (current-command-line-arguments))

(when (not (= (vector-length args) 2))
    (error "UsageL racket rolling-sum.rkt <size> <iterations>"))

(define size (string->number (vector-ref args 0)))

(define count (string->number (vector-ref args 1)))

(define (build-lst size)
  (for/list ([i size])
    i))

(define (reverse-lst lst)
  (let loop ([lst lst] [lst-reversed '()])
    (if (empty? lst)
        lst-reversed
        (loop (rest lst) (cons (first lst) lst-reversed)))))

(define (reverse-iterations count lst)
  (for ([i count])
    (reverse-lst lst)))

(define (main)
  (reverse-iterations count (build-lst size)))

(main)
