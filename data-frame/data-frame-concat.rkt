;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-join.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
(require typed/rackunit)

; ***********************************************************
; data-frame-join rough draft
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [data-frame-concat (DataFrame DataFrame [#:col (Listof Symbol)] -> DataFrame)])

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in grip/data/symbol
	  symbol-prefix)
 (only-in "indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "series.rkt"
	  series-complete)
 (only-in "series-description.rkt"
	  SeriesType Series
	  SeriesDescription-type
	  series-type series-length
          series-data)
 (only-in "data-frame.rkt"
	  DataFrame new-data-frame data-frame-names
	  data-frame-series data-frame-cseries data-frame-nseries data-frame-iseries data-frame-explode
	  DataFrameDescription DataFrameDescription-series data-frame-description)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref nseries-label-ref new-NSeries)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "categorical-series-ops.rkt"
	  cseries-append)
 (only-in "numeric-series-ops.rkt"
	  nseries-append)
 (only-in "integer-series-ops.rkt"
	  iseries-append)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "data-frame-print.rkt"
          frame-write-tab))

; This function consumes two DataFrames and produces a
; concatenation of all their series on matching column
; names.

; TR Bug??
(: data-frame-concat (DataFrame DataFrame [#:col (Listof Symbol)] -> DataFrame))
(define (data-frame-concat dfa dfb #:col [cols '()])

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (if (null? cols)
					   (set-intersect cols-a cols-b)
                                           (set-intersect (list->set cols)
                                                          (set-intersect cols-a cols-b))))
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name)))
                                 (if (not (equal? (series-type dfa-series) (series-type dfb-series)))
                                     (error data-frame-concat
                                            "The series types are different, unable to concat.") 
                                     (case (series-type dfa-series)
                                       ('CategoricalSeries (cseries-append (data-frame-cseries dfa name)
                                                                            (data-frame-cseries dfb name)))
                                       ('NumericSeries     (nseries-append (data-frame-nseries dfa name)
                                                                            (data-frame-nseries dfb name)))
                                       ('IntegerSeries     (iseries-append (data-frame-iseries dfa name)
                                                                            (data-frame-iseries dfb name)))
                                       (else (error data-frame-concat
                                                    "Unknown series type ~a."
                                                    (SeriesDescription-type dfa-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa))))))

; Test Cases

(define columns-integer-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-integer-2
(define data-frame-integer-1 (new-data-frame columns-integer-1))

; create new data-frame-integer-3
(define data-frame-integer-2 (new-data-frame columns-integer-2))

(frame-write-tab (data-frame-concat data-frame-integer-1 data-frame-integer-2) (current-output-port))

(define columns-mixed-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 5.2 6.2 7.2 8.2) #f))
   (cons 'col3 (new-CSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-NSeries (flvector 25.3 26.3 27.3 28.3) #f))
   (cons 'col3 (new-CSeries (vector 'e 'f 'g 'h) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-mixed-1
(define data-frame-mixed-1 (new-data-frame columns-mixed-1))

; create new data-frame-mixed-2
(define data-frame-mixed-2 (new-data-frame columns-mixed-2))

(frame-write-tab (data-frame-concat data-frame-mixed-1 data-frame-mixed-2) (current-output-port))
