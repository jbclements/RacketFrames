;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: tests/plot.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

(require typed/rackunit)
(require plot)
(require plot/utils)

(require "../plot/plot.rkt")

;******************
; Test Cases
;******************

(displayln "plotting integer series")

(make-scatter-plot (new-ISeries (vector 1 2 3 4 5) #f))

(make-line-plot (new-ISeries (vector 1 2 3 4 5) #f))

(define float-column (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f)))

(displayln "plotting float columns")

(make-scatter-plot float-column)

(make-line-plot float-column)

;******************
;data-frame-integer
;******************
(define integer-columns
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8 24) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12 24) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame integer-columns))

(displayln "plotting integer data-frame")

(make-scatter-plot data-frame-integer)

(displayln "plotting integer columns")

(make-scatter-plot integer-columns)

;******************
;data-frame-float
;******************
(define float-columns
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5) #f))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5) #f))))

; create new data-frame-float
(define data-frame-float (new-data-frame float-columns))

(displayln "plotting float data-frame")

(make-scatter-plot data-frame-float)

(displayln "plotting float columns")

(make-scatter-plot float-columns)

(displayln "discrete histogram")

(make-discrete-histogram (new-ISeries (vector 1 2 2 3 4 5 5 5) #f))

(make-discrete-histogram (new-GenSeries (vector 1 2 2 1.5 3 4 5 5 'a 5) #f))

(make-discrete-histogram float-column)

(make-discrete-histogram integer-columns)

(make-discrete-histogram data-frame-float)

(get-index-vector (new-ISeries (vector 1 2 3 4 4) #f))

(define series-integer-labeled
  (new-ISeries (vector 1 2 3 4)
               (list 'a 'b 'c 'd)))

(get-index-vector series-integer-labeled)

(make-discrete-histogram-stacked float-column)

(make-discrete-histogram-stacked float-columns)

;******************
;data-frame-integer
;******************
(define integer-columns-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer-2 (new-data-frame integer-columns-2))

(make-discrete-histogram-stacked data-frame-integer-2)

; individual testing does not work due to Racket type checking
;(get-discrete-hist-data (vector 1 2 3 4 5))

;(list-of-vec-from-hist-bin (get-discrete-hist-data (vector 1 2 3 4 5)))
