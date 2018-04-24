#lang typed/racket/base

(provide:
 [as-julian-day-numbers (CSeries -> ISeries)])

(require
 racket/fixnum
 (only-in grip/data/datetime/convert
          date->julian-day-number)
 (only-in grip/data/datetime/parse
	  parse-date)
 (only-in "categorical-series.rkt"
	  CSeries
	  cseries-length cseries-referencer)
 (only-in "integer-series.rkt"
	  ISeries
	  new-ISeries)
 (only-in "integer-series-builder.rkt"
	  new-ISeriesBuilder
	  append-ISeriesBuilder
	  complete-ISeriesBuilder))

(: as-julian-day-numbers (CSeries -> ISeries))
(define (as-julian-day-numbers cseries)
  (let* ((clen (cseries-length cseries))
	 (cref (cseries-referencer cseries))
	 (ibuilder (new-ISeriesBuilder clen)))
    (do: : ISeries ([idx : Fixnum #{0 : Fixnum} (fx+ idx #{1 : Fixnum})])
	((= idx clen) (complete-ISeriesBuilder ibuilder))
      (let ((date (parse-date (symbol->string (cref idx)))))
	(if date
	    (append-ISeriesBuilder ibuilder (assert (date->julian-day-number date) fixnum?))
	    (error 'as-julian-day-numbers "Invalid date: ~s" date))))))
