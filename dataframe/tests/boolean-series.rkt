;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/boolean-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require "../main.rkt")

; ***********************************************************
; Test Cases
; ***********************************************************

; boolean series tests

; create boolean series
(define series-boolean (new-BSeries (vector #f #t #t #t)
                                    (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-boolean-2 (new-BSeries (vector #f #t #t #f)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; bseries reference tests
(check-equal? ((bseries-referencer series-boolean) 0) #f)

(check-equal? ((bseries-referencer series-boolean) 1) #t)

(check-equal? (bseries-iref series-boolean (list 0)) (list #f))

(check-equal? (bseries-iref series-boolean (list 1)) (list #t))

(check-equal? (bseries-label-ref series-boolean 'd) (list #t))

(check-equal? (bseries-label-ref series-boolean 'c) (list #t))

; series length
(check-equal? (bseries-length series-boolean) 4)

(check-equal? (bseries-data (map/bs series-boolean (lambda (b) (if (not b) #t #f)))) (vector #t #f #f #f))

(check-equal? (bseries-iloc series-boolean 1) #t)

(check-equal? (bseries-data (assert (bseries-iloc series-boolean (list 2 3)) BSeries?)) (vector #t #t))

(check-equal? (bseries-data (assert (bseries-iloc series-boolean (range 4)) BSeries?)) (vector #f #t #t #t))

; bseries-loc
(bseries-data series-boolean)

(bseries-loc series-boolean 'a)

(bseries-data (assert (bseries-loc series-boolean (list 'a 'd)) BSeries?))

(bseries-loc-boolean series-boolean (list #t #f #f #f))

(bseries-data (assert (bseries-loc-boolean series-boolean (list #t #t #f #f)) BSeries?))

(bseries-data (assert (bseries-loc-boolean series-boolean (list #t #f #f #t)) BSeries?))

(bseries-loc series-boolean (list #t #f #f #f))

(bseries-data (assert (bseries-loc series-boolean (list #t #t #f #f)) BSeries?))

(bseries-data (assert (bseries-loc series-boolean (list #t #f #f #t)) BSeries?))
