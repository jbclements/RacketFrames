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
 [series-data (Series -> (U FlVector (Vectorof Symbol) (Vectorof Fixnum) (Vectorof Boolean)))]
 [series-iref (Series Index -> (U Float Symbol Fixnum Boolean))])

; ***********************************************************

; ***********************************************************

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label
          GSeries GSeries? GSeries-data gseries-length)         
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

(define-type Series (U GSeries NSeries CSeries ISeries BSeries))

(define-type SeriesType (U 'GenericSeries 'NumericSeries 'CategoricalSeries 'IntegerSeries 'BooleanSeries))

(struct: SeriesDescription ([name : Label]
                            [type : SeriesType]
                            [length : Integer]) #:transparent)

; ***********************************************************

; ***********************************************************

(: series-type (Series -> SeriesType))
(define (series-type series)
  (cond
   ((GSeries? series) 'GenericSeries)
   ((NSeries? series) 'NumericSeries)
   ((CSeries? series) 'CategoricalSeries)
   ((ISeries? series) 'IntegerSeries)
   ((BSeries? series) 'BooleanSeries)
   (else (error 'frame-series-description-type "'UnknownSeries: ~a" series))))

(: series-length (Series -> Index))
(define (series-length series)
  (cond
    ;[(GSeries? series) (gseries-length series)]
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

(: series-data (Series -> (U FlVector (Vectorof Symbol) (Vectorof Fixnum) (Vectorof Boolean))))
(define (series-data series)
  (cond
    ;[(GSeries? series) (gseries-length series)]
    [(NSeries? series) (nseries-data series)]    
    [(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (iseries-data series)]
    [(BSeries? series) (bseries-data series)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-iref (Series Index -> (U Float Symbol Fixnum Boolean)))
(define (series-iref series idx)
  (cond
    ;[(GSeries? series) (gseries-length series)]
    [(NSeries? series) (nseries-iref series idx)]    
    [(CSeries? series) (cseries-iref series idx)]    
    [(ISeries? series) (iseries-iref series idx)]
    [(BSeries? series) (bseries-iref series idx)]
    [else (error "Unknown Series type in DataFrame")]))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************