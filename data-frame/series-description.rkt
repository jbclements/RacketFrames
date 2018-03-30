;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: series-description.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
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
 [series-data (Series -> (U FlVector (Vectorof String) (Vectorof Fixnum)))])

; ***********************************************************

; ***********************************************************

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label
          GSeries GSeries? GSeries-data gseries-length)         
 (only-in "categorical-series.rkt"
          CSeries CSeries? CSeries-data cseries-length)
 (only-in "numeric-series.rkt"
          NSeries NSeries? NSeries-data nseries-length nseries-data)
 (only-in "integer-series.rkt"
	  ISeries ISeries? ISeries-data iseries-length iseries-data))

; ***********************************************************

; ***********************************************************

(define-type Series (U GSeries NSeries CSeries ISeries))

(define-type SeriesType (U 'GenericSeries 'NumericSeries 'CategoricalSeries 'IntegerSeries))

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
   (else (error 'frame-series-description-type "'UnknownSeries: ~a" series))))

(: series-length (Series -> Index))
(define (series-length series)
  (cond
    ;[(GSeries? series) (gseries-length series)]
    [(NSeries? series) (nseries-length series)]    
    [(CSeries? series) (cseries-length series)]    
    [(ISeries? series) (iseries-length series)]
    [else (error "Unknown Series type in Frame")]))

(: series-description  (Label Series -> SeriesDescription))
(define (series-description name series)
  (SeriesDescription name (series-type series) (series-length series)))

; ***********************************************************

; ***********************************************************
; Get series data

(: series-data (Series -> (U FlVector (Vectorof String) (Vectorof Fixnum))))
(define (series-data series)
  (cond
    ;[(GSeries? series) (gseries-length series)]
    [(NSeries? series) (nseries-data series)]    
    ;[(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (iseries-data series)]
    [else (error "Unknown Series type in Frame")]))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************