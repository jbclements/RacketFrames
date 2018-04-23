#lang typed/racket

(provide
 SeriesBuilder)

(require
 (only-in "numeric-series-builder.rkt"
          NSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          CSeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder)
 (only-in "boolean-series-builder.rkt"
	  BSeriesBuilder))

(define-type SeriesBuilder (U ISeriesBuilder CSeriesBuilder NSeriesBuilder BSeriesBuilder))

