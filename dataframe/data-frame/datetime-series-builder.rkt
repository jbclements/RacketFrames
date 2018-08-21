#lang typed/racket

(provide
 (struct-out DateTimeSeriesBuilder))

(provide:
 [new-DTSeriesBuilder (case-> 
		      (-> DateTimeSeriesBuilder)
		      (Index -> DateTimeSeriesBuilder))]
 [append-DTSeriesBuilder   (DateTimeSeriesBuilder (U Fixnum String) -> Void)]
 [complete-DTSeriesBuilder (DateTimeSeriesBuilder -> DateTimeSeriesBuilder)])

(require
 racket/fixnum
 (only-in racket/vector
	  vector-copy)
 (only-in "datetime-series.rkt"
          DateTimeSeries))

(struct: DateTimeSeriesBuilder ([index  : Index]
                                [datetime-data : (Vectorof Datetime)]) #:mutable)

(define base-len 512)

(: new-DateTimeSeriesBuilder (case-> 
		       (-> DateTimeSeriesBuilder)
		       (Index -> DateTimeSeriesBuilder)))
(define (new-DateTimeSeriesBuilder [len base-len])
  (DateTimeSeriesBuilder 0 (make-vector len 0)))

(: append-DateTimeSeriesBuilder (DateTimeSeriesBuilder (U Fixnum String) -> Void))
(define (append-DateTimeSeriesBuilder builder int/str-value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (DateTimeSeriesBuilder-index builder)))
      (set-DateTimeSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (DateTimeSeriesBuilder-data builder))
	   (curr-len (vector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof Fixnum) ((inst make-vector Fixnum) new-len 0)))
	    (do ([idx 0 (add1 idx)])
		([>= idx curr-len] (set-DateTimeSeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (DateTimeSeriesBuilder-index builder)         
         (vector-length (DateTimeSeriesBuilder-data builder)))
      (let ((num (if (string? int/str-value)
		     (let ((num (string->number (string-trim int/str-value))))                      
                       (if num (assert num fixnum?) 0))
		     int/str-value)))
        (vector-set! (DateTimeSeriesBuilder-data builder)
		     (bump-index)
		     num))
      (begin
        (extend-data)       
        (append-DateTimeSeriesBuilder builder int/str-value))))

(: complete-DateTimeSeriesBuilder (DateTimeSeriesBuilder -> DateTimeSeries))
(define (complete-DateTimeSeriesBuilder builder)  
  (let* ((data (DateTimeSeriesBuilder-data builder))
         (len (DateTimeSeriesBuilder-index builder)))
    (DateTimeSeriesBuilder #f (vector-copy data 0 len))))
