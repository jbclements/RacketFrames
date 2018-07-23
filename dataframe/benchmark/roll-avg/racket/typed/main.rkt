#lang typed/racket

(: args (Vectorof String))
(define args (current-command-line-arguments))

(when (not (= (vector-length args) 2))
    (error "UsageL racket rolling-sum.rkt <start> <end>"))

(: start Integer)
(define start (assert (string->number (vector-ref args 0)) exact-integer?))

(: end Integer)
(define end (assert (string->number (vector-ref args 1)) exact-integer?))

(: add-up-to (Integer -> Integer))
(define (add-up-to dest)
  (if (<= dest 1)
      1
      (+ dest (add-up-to (sub1 dest)))))

(: roll-average (Integer -> Integer))
(define (roll-average dest)
  (if (<= dest 0)
      0
      (+ dest (add-up-to (sub1 dest)) (roll-average (sub1 dest)))))

(: main (-> Void))
(define (main)
  (for ([i (range start (add1 end))])
    (displayln (string-append "rollAverage(" (number->string i) ") = " (number->string (roll-average i))))))

(main)