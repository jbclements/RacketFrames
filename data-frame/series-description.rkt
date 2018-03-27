#lang typed/racket/base

(provide
 (struct-out SeriesDescription)
 Series SeriesType)

(provide:
 [series-description (Label Series -> SeriesDescription)]
 [series-type (Series -> SeriesType)]
 [series-length (Series -> Index)])

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label
          GSeries GSeries? GSeries-data gseries-length)         
 (only-in "categorical-series.rkt"
          CSeries CSeries? CSeries-data cseries-length)
 (only-in "numeric-series.rkt"
          NSeries NSeries? NSeries-data nseries-length)
 (only-in "integer-series.rkt"
	  ISeries ISeries? ISeries-data iseries-length))

(define-type Series (U GSeries NSeries CSeries ISeries))

(define-type SeriesType (U 'GenericSeries 'NumericSeries 'CategoricalSeries 'IntegerSeries))

(struct: SeriesDescription ([name : Label]
                            [type : SeriesType]
                            [length : Integer]) #:transparent)

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
    [(GSeries? series) (gseries-length series)]     
    [(NSeries? series) (nseries-length series)]     
    [(CSeries? series) (cseries-length series)]     
    [(ISeries? series) (iseries-length series)]
    [else (error "Unknown Series type in Frame")]))
      
(: series-description  (Label Series -> SeriesDescription))
(define (series-description name series)
  (SeriesDescription name (series-type series) (series-length series)))
