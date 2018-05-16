;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: series-description.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

; ***********************************************************
; A map of series to label names, represented as a collection
; of columns.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.

(provide
 (struct-out SeriesDescription)
 Series SeriesType)

(provide:
 [series-description (Label Series -> SeriesDescription)]
 [series-type (Series -> SeriesType)]
 [series-length (Series -> Index)]
 [series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol) (Vectorof Fixnum) (Vectorof Boolean)))]
 [series-iref (Series Index -> Any)])

; ***********************************************************

; ***********************************************************

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label)
 (only-in "generic-series.rkt"
          GenericType GenSeries GenSeries? GenSeries-data gen-series-length gen-series-data gen-series-iref)
 (only-in "categorical-series.rkt"
          CSeries CSeries? CSeries-data cseries-length cseries-data cseries-iref)
 (only-in "numeric-series.rkt"
          NSeries NSeries? NSeries-data nseries-length nseries-data nseries-iref)
 (only-in "integer-series.rkt"
	  ISeries ISeries? ISeries-data iseries-length iseries-data iseries-iref)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? BSeries-data bseries-length bseries-data bseries-iref))

; ***********************************************************

; ***********************************************************

(define-type Series (U GenSeries NSeries CSeries ISeries BSeries))

(define-type SeriesType (U 'GenericSeries 'NumericSeries 'CategoricalSeries 'IntegerSeries 'BooleanSeries))

(struct: SeriesDescription ([name : Label]
                            [type : SeriesType]
                            [length : Integer]) #:transparent)

; ***********************************************************

; ***********************************************************

(: series-type (Series -> SeriesType))
(define (series-type series)
  (cond
   ((GenSeries? series) 'GenericSeries)
   ((NSeries? series) 'NumericSeries)
   ((CSeries? series) 'CategoricalSeries)
   ((ISeries? series) 'IntegerSeries)
   ((BSeries? series) 'BooleanSeries)
   (else (error "Unknown Series type in DataFrame"))))

(: series-length (Series -> Index))
(define (series-length series)
  (cond
    [(GenSeries? series) (gen-series-length series)]
    [(NSeries? series) (nseries-length series)]
    [(CSeries? series) (cseries-length series)]    
    [(ISeries? series) (iseries-length series)]
    [(BSeries? series) (bseries-length series)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-description  (Label Series -> SeriesDescription))
(define (series-description name series)
  (SeriesDescription name (series-type series) (series-length series)))

; ***********************************************************

; ***********************************************************
; Get series data

(: series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol) (Vectorof Fixnum) (Vectorof Boolean))))
(define (series-data series)
  (cond
    [(GenSeries? series) (gen-series-data series)]
    [(NSeries? series) (nseries-data series)]    
    [(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (iseries-data series)]
    [(BSeries? series) (bseries-data series)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-iref (Series Index -> Any))
(define (series-iref series idx)
  (cond
    [(GenSeries? series) (gen-series-iref series idx)]
    [(NSeries? series) (nseries-iref series idx)]    
    [(CSeries? series) (cseries-iref series idx)]    
    [(ISeries? series) (iseries-iref series idx)]
    [(BSeries? series) (bseries-iref series idx)]
    [else (error "Unknown Series type in DataFrame")]))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************