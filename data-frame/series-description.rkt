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
 Series Series? SeriesList SeriesList? SeriesType)

(provide:
 [series-description (Label Series -> SeriesDescription)]
 [series-type (Series -> SeriesType)]
 [series-length (Series -> Index)]
 [series-data (Series -> (U (Vectorof GenericType) FlVector (Vectorof Symbol) (Vectorof Fixnum) (Vectorof Boolean)))]
 [series-iref (Series Index -> Any)]
 [series-loc-boolean (Series (Listof Boolean) -> (U Any Series))]
 [series-loc (Series (U Label (Listof Label) (Listof Boolean)) -> (U Any Series))]
 [series-iloc (Series (U Index (Listof Index)) -> (U Any Series))]
 [set-series-index! (Series SIndex -> Void)])

; ***********************************************************

; ***********************************************************

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label SIndex)
 (only-in "generic-series.rkt"
          GenericType GenSeries GenSeries? GenSeries-data gen-series-length gen-series-data gen-series-iref
          set-GenSeries-index gen-series-loc-boolean gen-series-loc gen-series-iloc gen-series-print)
 (only-in "categorical-series.rkt"
          CSeries CSeries? CSeries-data cseries-length cseries-data cseries-iref
          cseries-iloc)
 (only-in "numeric-series.rkt"
          NSeries NSeries? NSeries-data nseries-length nseries-data nseries-iref
          set-NSeries-index nseries-loc-boolean nseries-loc nseries-iloc nseries-print)
 (only-in "integer-series.rkt"
	  ISeries ISeries? ISeries-data iseries-length iseries-data iseries-iref
          set-ISeries-index iseries-loc-boolean iseries-loc iseries-iloc iseries-print)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? BSeries-data bseries-length bseries-data bseries-iref
          set-BSeries-index bseries-loc-boolean bseries-loc bseries-iloc bseries-print))

; ***********************************************************

; ***********************************************************

(define-type Series (U GenSeries NSeries CSeries ISeries BSeries))

(define-predicate Series? Series)

(define-type SeriesList (Listof Series))

(define-predicate SeriesList? SeriesList)

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
    [(GenSeries? series) (gen-series-iref series (list idx))]
    [(NSeries? series) (nseries-iref series (list idx))]    
    [(CSeries? series) (cseries-iref series (list idx))]    
    [(ISeries? series) (iseries-iref series (list idx))]
    [(BSeries? series) (bseries-iref series (list idx))]
    [else (error "Unknown Series type in DataFrame")]))

(: series-loc-boolean (Series (Listof Boolean) -> (U Any Series)))
(define (series-loc-boolean series boolean-lst)
  (cond
    [(GenSeries? series) (gen-series-loc-boolean series boolean-lst)]
    [(NSeries? series) (nseries-loc-boolean series boolean-lst)]
    ;[(CSeries? series) (cseries-loc series idx)]   
    [(ISeries? series) (iseries-loc-boolean series boolean-lst)]
    [(BSeries? series) (bseries-loc-boolean series boolean-lst)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-loc (Series (U Label (Listof Label) (Listof Boolean)) -> (U Any Series)))
(define (series-loc series label)
  (cond
    [(GenSeries? series) (gen-series-loc series label)]
    [(NSeries? series) (nseries-loc series label)]
    ;[(CSeries? series) (cseries-loc series idx)]   
    [(ISeries? series) (iseries-loc series label)]
    [(BSeries? series) (bseries-loc series label)]
    [else (error "Unknown Series type in DataFrame")]))

(: series-iloc (Series (U Index (Listof Index)) -> (U Any Series)))
(define (series-iloc series idx)
  (cond
    [(GenSeries? series) (gen-series-iloc series idx)]
    [(NSeries? series) (nseries-iloc series idx)]
    [(CSeries? series) (cseries-iloc series idx)]   
    [(ISeries? series) (iseries-iloc series idx)]
    [(BSeries? series) (bseries-iloc series idx)]
    [else (error "Unknown Series type in DataFrame")]))

; ***********************************************************

; ***********************************************************

(: set-series-index! (Series SIndex -> Void))
(define (set-series-index! series index)
  (cond
    [(GenSeries? series) (set! series (set-GenSeries-index (assert series GenSeries?) index))]
    [(NSeries? series) (set! series (set-NSeries-index (assert series NSeries?) index))]    
    ;[(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (set! series (set-ISeries-index (assert series ISeries?) index))]
    [(BSeries? series) (set! series (set-BSeries-index (assert series BSeries?) index))]
    [else (error "Unknown or not supported series type in DataFrame")]))

; ***********************************************************

; ***********************************************************

(: series-print (Series Output-Port -> Void))
(define (series-print series port)
  (cond
    [(GenSeries? series) (gen-series-print (assert series GenSeries?) port)]
    [(NSeries? series) (nseries-print (assert series NSeries?) port)]    
    ;[(CSeries? series) (cseries-data series)]    
    [(ISeries? series) (iseries-print (assert series ISeries?) port)]
    [(BSeries? series) (bseries-print (assert series BSeries?) port)]
    [else (error "Unknown or not supported series type.")]))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************