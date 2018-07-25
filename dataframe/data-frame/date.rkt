#lang typed/racket

(provide:
 [as-julian-day-numbers (CSeries -> ISeries)])

(require
 racket/fixnum
 (only-in "../util/datetime/convert.rkt"
          date->julian-day-number)
 (only-in "../util/datetime/parse.rkt"
	  parse-date parse-date-dashed)
 (only-in "categorical-series.rkt"
	  CSeries new-CSeries cseries-data
	  cseries-length cseries-referencer)
 (only-in "integer-series.rkt"
	  ISeries iseries-data
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
      (let ((date (parse-date-dashed (symbol->string (cref idx)))))
	(if date
            (begin
              (displayln date)
              (displayln (date->julian-day-number date))
	    (append-ISeriesBuilder ibuilder (assert (date->julian-day-number date) fixnum?)))
	    (error 'as-julian-day-numbers "Invalid date: ~s" date))))))

(iseries-data (as-julian-day-numbers (new-CSeries (vector '12-25-2015 '01-01-2016 '05-19-1995 '11-10-1996))))
