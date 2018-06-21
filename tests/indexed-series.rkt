;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require RacketFrames)

; ***********************************************************
; Test Cases
; ***********************************************************

; Not sure how to check
;(check-equal? (build-index-from-labels (list 'a 'b 'c 'd))
;              (hash 'b 1 'c' 2 'd 3 'a 0))

; checks numerical index of label
(check-equal? (label-index (build-index-from-labels (list 'a 'b 'c 'd)) 'a) (list 0))
(check-equal? (label-index (build-index-from-labels (list 'a 'b 'c 'd)) 'c) (list 2))

; checks numerical idx of LabelIndex
(check-equal? (label->lst-idx (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 'd) (list 3))
(check-equal? (label->lst-idx (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 'c) (list 2))

; checks numerical idx of LabelIndex
(check-equal? (idx->label (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 3) 'd)
(check-equal? (idx->label (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 2) 'c)

; checks to see if we have a labelled index

(check-equal? (is-labeled? (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd)))) #t)
(check-equal? (is-labeled? (LabelIndex #f)) #f)

; generic series tests

; create integer series
(define g-series-integer (new-GSeries (vector 1 2 3 4) (build-index-from-labels (list 'a 'b 'c 'd))))

(define g-series-integer-2 (new-GSeries (vector 1 2 3 4 5) (build-index-from-labels (list 'a 'b 'c 'd 'a))))

; create float series
(define g-series-float (new-GSeries (vector 1.5 2.5 3.5 4.5 5.5) (build-index-from-labels (list 'a 'b 'c 'd 'e))))

; create symbol series
(define g-series-symbol (new-GSeries (vector 'e 'f 'g 'h) (list 'a 'b 'c 'd)))

; point struct
(struct point ([x : Integer] [y : Integer]) #:transparent)

; create point struct series
(define g-series-point (new-GSeries (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)) (build-index-from-labels (list 'a 'b 'c 'd 'e))))

;(gseries-data g-series-point)

; point series ref by index
;(gseries-iref g-series-point (list 2))

; point series ref by label
;(series-ref g-series-point 'd)

;(gseries-data (map/GSeries g-series-point (λ: ((p : point))
;                                (point-x p))))

; integer series ref by index
(check-equal? (gseries-iref g-series-integer (list 2)) (list 3))

; integer series ref by label
(check-equal? (series-ref g-series-integer 'd) (list 4))

(check-equal? (series-ref g-series-integer-2 'a) (list 1 5))

; symbol series ref by index
;(check-equal? (gseries-iref g-series-symbol 2) (list 'g))

; symbol series ref by label
(check-equal? (series-ref g-series-symbol 'd) (list 'h))

; series length
(check-equal? (gseries-length g-series-symbol) 4)

; series map
(check-equal? (GSeries-data (map/GSeries g-series-integer (λ: ((x : Integer))
                                                            (add1 x)))) #(2 3 4 5))
; create struct series
(define g-series-struct (new-GSeries (vector (LabelIndex #f) (LabelIndex #f)) (build-index-from-labels (list 'a 'b))))

; struct series ref by index
;(check-equal? (LabelIndex-index (assert (gseries-iref g-series-struct 1) LabelIndex?)) (LabelIndex-index (LabelIndex #f)))

; integer series ref by label
; checks labeling function which converts labels hash to list
(check-equal? (labeling (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))))
              '((b 1) (c 2) (d 3) (a 0)))

; checks label sorting
(check-equal? (label-sort-lexical (LabelIndex (build-index-from-labels (list 'b 'd 'a 'c))))
              '((a 2) (b 0) (c 3) (d 1)))

; check label sorting by position
; (check-equal? (label-sort-positional (LabelIndex (build-index-from-labels (list 'b 'd 'a 'c))))
;              '((b . 0) (d . 1) (a . 2) (c . 3)))

(build-multi-index-from-list (list (list 'a 'b 'c) (list 1 2 3)))

(build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3)))