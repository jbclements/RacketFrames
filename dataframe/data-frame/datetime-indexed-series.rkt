;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: datetime-indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

(require
  "../util/datetime.rkt"
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex LabelIndex-index
          label-index label->lst-idx
          idx->label is-labeled?)
 (only-in "boolean-series.rkt"
          BSeries))

(struct DateTimeSeries LabelIndex ([date-data : (Vectorof Symbol)] [data : (Vectorof Fixnum)]))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a ISeries
; struct object.
(: new-DateTimeSeries ((Vectorof Symbol) (Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> DateTimeSeries))
(define (new-DateTimeSeries date-data data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (and (eq? (vector-length date-data) (hash-count index)) (eq? (vector-length data) (hash-count index)))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(DateTimeSeries labels date-data data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (DateTimeSeries index date-data data))
	  (DateTimeSeries #f date-data data))))
; ***********************************************************

; ***********************************************************
(: set-DataTimeSeries-index (DateTimeSeries (U (Listof Label) SIndex) -> DateTimeSeries))
(define (set-DataTimeSeries-index datetime-series labels)

  (define date-data (DateTimeSeries-date-data datetime-series))
  (define data (DateTimeSeries-data datetime-series))
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (and (eq? (vector-length date-data) (hash-count index)) (eq? (vector-length data) (hash-count index)))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(DateTimeSeries labels date-data data))
      (let ((index (build-index-from-labels labels)))
        (check-mismatch index)
        (DateTimeSeries index date-data data))))
; ***********************************************************

; ***********************************************************
(: datetime-series-print (DateTimeSeries Output-Port -> Void))
(define (datetime-series-print datetime-series port)
  (define date-v (datetime-series-date-data datetime-series))
  (define v (datetime-series-data datetime-series))
  (let ((len (vector-length v))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "Empty $DateTimeSeries" port)
	(begin
          (displayln "*********" port)
          (displayln "$DateTimeSeries" port)
          (displayln "*********" port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((date (vector-ref date-v i))
                  (num (vector-ref v i)))
              (if (LabelIndex-index datetime-series)                  
                  (display (idx->label datetime-series (assert i index?)) port)
                  (display (assert i index?) port))
              (display " " port)
              (displayln date port)
              (displayln num port)))))))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: datetime-series-referencer (DateTimeSeries -> (Index -> Fixnum)))
(define (datetime-series-referencer datetime-series)
  (let ((data (DateTimeSeries-data datetime-series)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: datetime-series-iref (DateTimeSeries (Listof Index) -> (Listof Fixnum)))
(define (datetime-series-iref datetime-series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (DateTimeSeries-data datetime-series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: datetime-series-date-range (DateTimeSeries Index -> (Vectorof Symbol)))
(define (datetime-series-date-range series pos)
   (vector-take (DateTimeSeries-date-data series) pos))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: datetime-series-range (DateTimeSeries Index -> (Vectorof Fixnum)))
(define (datetime-series-range series pos)
   (vector-take (DateTimeSeries-data series) pos))

; This function consumes an integer series and returns its
; data vector.
(: datetime-series-date-data (DateTimeSeries -> (Vectorof Symbol)))
(define (datetime-series-date-data series)
  (DateTimeSeries-date-data series))

; This function consumes an integer series and returns its
; data vector.
(: datetime-series-data (DateTimeSeries -> (Vectorof Fixnum)))
(define (datetime-series-data series)
  (DateTimeSeries-data series))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: datetime-series-label-ref (DateTimeSeries Label -> (Listof Integer)))
(define (datetime-series-label-ref series label)
  (datetime-series-iref series (label->lst-idx series label)))

; This function consumes an integer series and returns the
; length of that series.
(: datetime-series-length (DateTimeSeries -> Index))
(define (datetime-series-length series)
  (vector-length (DateTimeSeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/datetime-series-date-data (DateTimeSeries (Symbol -> Symbol) -> DateTimeSeries))
(define (map/datetime-series-date-data series fn)
  (let ((old-data (DateTimeSeries-date-data series)))
    (DateTimeSeries #f (build-vector (vector-length old-data)
                              (λ: ((idx : Natural))
                                (fn (vector-ref old-data idx))))
                    (DateTimeSeries-data series))))

(: map/datetime-series-data (DateTimeSeries (Fixnum -> Fixnum) -> DateTimeSeries))
(define (map/datetime-series-data series fn)
  (let ((old-data (DateTimeSeries-data series)))
    (DateTimeSeries #f (DateTimeSeries-date-data series)
                    (build-vector (vector-length old-data)
                                  (λ: ((idx : Natural))
                                    (fn (vector-ref old-data idx)))))))
; ***********************************************************

