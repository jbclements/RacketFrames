#lang racket

(define args (current-command-line-arguments))

(when (not (= (vector-length args) 2))
    (error "UsageL racket rolling-sum.rkt <start> <end>"))

(define start (string->number (vector-ref args 0)))

(define end (string->number (vector-ref args 1)))

(define (add-up-to dest)
  (if (<= dest 1)
      1
      (+ dest (add-up-to (sub1 dest)))))

(define (roll-average dest)
  (if (<= dest 0)
      0
      (+ dest (add-up-to (sub1 dest)) (roll-average (sub1 dest)))))

(define (main)
  (for ([i (range start (add1 end))])
    (displayln (roll-average i))))