#lang typed/racket

(provide:
 [series-print (Series Output-Port -> Void)]
 [column-print (Column Output-Port -> Void)])

(require
 (only-in "series-description.rkt"
	  Series)
 (only-in "generic-series.rkt"
          GenSeries? gen-series-print)
 (only-in "categorical-series.rkt"
	  CSeries? cseries-print)
 (only-in "numeric-series.rkt"
	  NSeries? nseries-print)
 (only-in "integer-series.rkt"
	  ISeries? iseries-print)
 (only-in "boolean-series.rkt"
	  BSeries? bseries-print)
 (only-in "data-frame.rkt"
          Column column-heading column-series))


(: series-print (Series Output-Port -> Void))
(define (series-print series port)
  (cond
    ((GenSeries? series)
     (gen-series-print series port))
    ((NSeries? series)
     (nseries-print series port))
    ((CSeries? series)
     (cseries-print series port))
    ((ISeries? series)
     (iseries-print series port))
    ((BSeries? series)
     (bseries-print series port))))

(: column-print (Column Output-Port -> Void))
(define (column-print column port)
  (let ((heading (column-heading column))
        (series (column-series column)))
    (displayln heading)
    (series-print series port)))
