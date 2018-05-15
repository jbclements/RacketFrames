#lang typed/racket

(require typed/rackunit)

(require
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex
          label-index label->idx))

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out GenSeries)
 GenericType)

(define-type GenericType Any)

(provide:
 [new-GenSeries ((Vectorof GenericType) (Option (U (Listof Label) SIndex)) -> GenSeries)]
 [gen-series-iref (GenSeries Index -> GenericType)]
 [gen-series-label-ref (GenSeries Label -> GenericType)]
 [gen-series-range (GenSeries Index -> (Vectorof GenericType))]
 [gen-series-length (GenSeries -> Index)]
 [gen-series-referencer (GenSeries -> (Index -> GenericType))]
 [gen-series-data (GenSeries -> (Vectorof GenericType))]
 [map/gen-s (GenSeries (GenericType -> GenericType) -> GenSeries)])

;; Integer series optimized with use of Fixnum.
(struct GenSeries LabelIndex ([data : (Vectorof GenericType)]))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a GenSeries
; struct object.
(: new-GenSeries ((Vectorof GenericType) (Option (U (Listof Label) SIndex)) -> GenSeries))
(define (new-GenSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(GenSeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (GenSeries index data))
	  (GenSeries #f data))))

; ***********************************************************
; This function consumes a generic series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: gen-series-referencer (GenSeries -> (Index -> GenericType)))
(define (gen-series-referencer gen-series)
  (let ((data (GenSeries-data gen-series)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes a generic series and an index and
; returns the value at that index in the series.
(: gen-series-iref (GenSeries Index -> GenericType))
(define (gen-series-iref series idx)
  (vector-ref (GenSeries-data series) idx))

; This function consumes a generic series and an index and
; returns a vector of values in the range [0:index] in the series.
(: gen-series-range (GenSeries Index -> (Vectorof GenericType)))
(define (gen-series-range series pos)
   (vector-take (GenSeries-data series) pos))

; This function consumes a generic series and returns its
; data vector.
(: gen-series-data (GenSeries -> (Vectorof GenericType)))
(define (gen-series-data series)
  (GenSeries-data series))

; This function consumes a generic series and a Label and returns
; the value at that Label in the series.
(: gen-series-label-ref (GenSeries Label -> GenericType))
(define (gen-series-label-ref series label)
  (gen-series-iref series (label->idx series label)))

; This function consumes a generic series and returns the
; length of that series.
(: gen-series-length (GenSeries -> Index))
(define (gen-series-length series)
  (vector-length (GenSeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/gen-s (GenSeries (GenericType -> GenericType) -> GenSeries))
(define (map/gen-s series fn)
  (let ((old-data (GenSeries-data series)))
    (GenSeries #f (build-vector (vector-length old-data)
                              (λ: ((idx : Natural))
                                (fn (vector-ref old-data idx)))))))
; ***********************************************************

; create generic series
(define series-generic (new-GenSeries (vector 1 2.5 'categorical #t)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-generic-2 (new-GenSeries (vector 5 6 7 8)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; gen-series reference tests
(check-equal? ((gen-series-referencer series-generic) 0) 1)

(check-equal? ((gen-series-referencer series-generic) 1) 2.5)

(check-equal? (gen-series-iref series-generic 0) 1)

(check-equal? (gen-series-iref series-generic 1) 2.5)

(check-equal? (gen-series-label-ref series-generic 'd) #t)

(check-equal? (gen-series-label-ref series-generic 'c) 'categorical)

; gen-series length
(check-equal? (gen-series-length series-generic) 4)

; point struct
(struct point ([x : Integer] [y : Integer]) #:transparent)

; create point struct series
(define gen-series-point (new-GenSeries (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)) (build-index-from-labels (list 'a 'b 'c 'd 'e))))

(gen-series-data gen-series-point)

; point series ref by index
(gen-series-iref gen-series-point 2)

; point series ref by label
(gen-series-label-ref gen-series-point 'd)
