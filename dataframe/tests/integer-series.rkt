;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/integer-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../main.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************

; integer series tests

; create integer series
(define series-integer (new-ISeries (vector 1 2 3 4)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-integer-2 (new-ISeries (vector 5 6 7 8)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; iseries reference tests
(check-equal? ((iseries-referencer series-integer) 0) 1)

(check-equal? ((iseries-referencer series-integer) 1) 2)

(check-equal? (iseries-iref series-integer (list 0)) (list 1))

(check-equal? (iseries-iref series-integer (list 1)) (list 2))

(check-equal? (iseries-label-ref series-integer 'd) (list 4))

(check-equal? (iseries-label-ref series-integer 'c) (list 3))

; series length
(check-equal? (iseries-length series-integer) 4)

; binop 2 series tests
(check-equal? (ISeries-data (+/is series-integer series-integer-2))
              (vector 6 8 10 12))

(check-equal? (ISeries-data (-/is series-integer series-integer-2))
              (vector -4 -4 -4 -4))

(check-equal? (ISeries-data (*/is series-integer series-integer-2))
              (vector 5 12 21 32))

; currently doing only integer division
(check-equal? (ISeries-data (//is series-integer series-integer-2))
              (vector 0 0 0 0))

(check-equal? (ISeries-data (r/is series-integer series-integer-2))
              (vector 1 2 3 4))

(check-equal? (ISeries-data (%/is series-integer series-integer-2))
              (vector 1 2 3 4))

; binop scalar series tests
(check-equal? (ISeries-data (+./is series-integer 2))
              (vector 3 4 5 6))

(check-equal? (ISeries-data (-./is series-integer 1))
              (vector 0 1 2 3))

(check-equal? (ISeries-data (*./is series-integer 2))
              (vector 2 4 6 8))

(check-equal? (ISeries-data (/./is series-integer 2))
              (vector 0 1 1 2))

(check-equal? (ISeries-data (r./is series-integer 2))
              (vector 1 0 1 0))

(check-equal? (ISeries-data (%./is series-integer 2))
              (vector 1 0 1 0))

; map tests
(check-equal? (ISeries-data (map/is series-integer (λ: ((x : Fixnum)) (unsafe-fx+ x 1))))
              (vector 2 3 4 5))

; iseries filter
(check-equal? (ISeries-data (iseries-filter series-integer even?)) (vector 2 4))

(check-equal? (ISeries-data (iseries-filter series-integer odd?)) (vector 1 3))

(check-equal? (ISeries-data (iseries-filter-not series-integer even?)) (vector 1 3))

(check-equal? (ISeries-data (iseries-filter-not series-integer odd?)) (vector 2 4))

; agg tests
(check-equal? (apply-agg-is 'sum series-integer) 10)

(check-equal? (apply-agg-is 'mean series-integer) 10/4)

(check-equal? (apply-agg-is 'count series-integer) 4)

(check-equal? (apply-agg-is 'min series-integer) 1)

(check-equal? (apply-agg-is 'max series-integer) 4)

; statistics tests
(check-equal? (apply-stat-is 'variance series-integer) 5/4)

(check-equal? (apply-stat-is 'stddev series-integer) 1.118033988749895)

(check-equal? (apply-stat-is 'skewness series-integer) 0.0)

; iseries print
(iseries-print series-integer (current-output-port))