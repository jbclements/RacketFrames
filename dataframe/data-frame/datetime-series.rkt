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
	  RFIndex RFIndex? build-index-from-list
          IndexDataType extract-index
          Label LabelIndex-index
          LabelIndex label-index label->lst-idx key->lst-idx
          idx->key is-indexed? ListofIndexDataType?)
 (only-in "boolean-series.rkt"
          BSeries))

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out DatetimeSeries))

(provide:
 [new-DatetimeSeries ((Vectorof Datetime) (Option (U (Listof IndexDataType) RFIndex)) -> DatetimeSeries)]
 [set-DatetimeSeries-index (DatetimeSeries (U (Listof IndexDataType) RFIndex) -> DatetimeSeries)]
 [datetime-series-iref (DatetimeSeries (Listof Index) -> (Listof Datetime))]
 [datetime-series-loc-boolean (DatetimeSeries (Listof Boolean) -> (U Datetime DatetimeSeries))]
 [datetime-series-loc (DatetimeSeries (U Label (Listof Label) (Listof Boolean)) -> (U Datetime DatetimeSeries))]
 [datetime-series-iloc (DatetimeSeries (U Index (Listof Index)) -> (U Datetime DatetimeSeries))]
 [datetime-series-label-ref (DatetimeSeries Label -> (Listof Datetime))]
 [datetime-series-range (DatetimeSeries Index -> (Vectorof Datetime))]
 [datetime-series-length (DatetimeSeries -> Index)]
 [datetime-series-referencer (DatetimeSeries -> (Index -> Datetime))]
 [datetime-series-data (DatetimeSeries -> (Vectorof Datetime))]
 [map/datetime-series-data (DatetimeSeries (Datetime -> Datetime) -> DatetimeSeries)]
 [datetime-series-print (DatetimeSeries Output-Port -> Void)])
; ***********************************************************

(struct DatetimeSeries ([index : (Option RFIndex)] [data : (Vectorof Datetime)]))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a DatetimeSeries
; struct object.
(: new-DatetimeSeries ((Vectorof Datetime) (Option (U (Listof IndexDataType) RFIndex)) -> DatetimeSeries))
(define (new-DatetimeSeries data labels)

  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count (extract-index index)))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (RFIndex? labels)
      (begin
	(check-mismatch (assert labels))
	(DatetimeSeries labels data))
      (if labels
	  (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
	    (check-mismatch index)
	    (DatetimeSeries index data))
	  (DatetimeSeries #f data))))
; ***********************************************************

; ***********************************************************
(: set-DatetimeSeries-index (DatetimeSeries (U (Listof IndexDataType) RFIndex) -> DatetimeSeries))
(define (set-DatetimeSeries-index datetime-series labels)
  (define data (DatetimeSeries-data datetime-series))
  
  (: check-mismatch (RFIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count (extract-index index)))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(DatetimeSeries labels data))
      (let ((index (build-index-from-list (assert labels ListofIndexDataType?))))
        (check-mismatch index)
        (DatetimeSeries index data))))
; ***********************************************************

; ***********************************************************
(: datetime-series-print (DatetimeSeries Output-Port -> Void))
(define (datetime-series-print datetime-series port)
  (define date-v (datetime-series-data datetime-series))
  (define v (datetime-series-data datetime-series))
  (let ((len (vector-length v))
	(out (current-output-port)))
    (if (zero? len)
	(displayln "Empty $DatetimeSeries" port)
	(begin
          (displayln "*********" port)
          (displayln "$DatetimeSeries" port)
          (displayln "*********" port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((date (vector-ref date-v i))
                  (num (vector-ref v i)))
              (if (DatetimeSeries-index datetime-series)                  
                  (display (idx->key (assert (DatetimeSeries-index datetime-series)) (assert i index?)) port)
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
(: datetime-series-referencer (DatetimeSeries -> (Index -> Datetime)))
(define (datetime-series-referencer datetime-series)
  (let ((data (DatetimeSeries-data datetime-series)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: datetime-series-iref (DatetimeSeries (Listof Index) -> (Listof Datetime)))
(define (datetime-series-iref datetime-series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (DatetimeSeries-data datetime-series) idx))
       lst-idx))

; This function consumes a datetime series and returns its
; data vector.
(: datetime-series-data (DatetimeSeries -> (Vectorof Datetime)))
(define (datetime-series-data series)
  (DatetimeSeries-data series))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: datetime-series-range (DatetimeSeries Index -> (Vectorof Datetime)))
(define (datetime-series-range series pos)
   (vector-take (datetime-series-data series) pos))

; This function consumes a series and a Label and returns
; the list of values at that Label in the series.
(: datetime-series-label-ref (DatetimeSeries Label -> (Listof Datetime)))
(define (datetime-series-label-ref series label)
  (datetime-series-iref series (key->lst-idx (assert (DatetimeSeries-index series)) label)))

; This function consumes an integer series and returns the
; length of that series.
(: datetime-series-length (DatetimeSeries -> Index))
(define (datetime-series-length series)
  (vector-length (DatetimeSeries-data series)))

(define-predicate ListofBoolean? (Listof Boolean))
(define-predicate ListofFixnum? (Listof Fixnum))

; indexing

(: build-labels-by-count ((Listof Label) (Listof Integer) -> (Listof Label)))
(define (build-labels-by-count label-lst count-lst)
  (if (null? label-lst)
      null
      (append
       (for/list: : (Listof Label)
         ([i (car count-lst)])
         (car label-lst))
       
       (build-labels-by-count (cdr label-lst) (cdr count-lst)))))

(: convert-to-label-lst ((U Label (Listof Label)) -> (Listof Label)))
(define (convert-to-label-lst label)
  (if (list? label)
      label
      (list label)))

(define-predicate ListofDatetime? (Listof Datetime))

(: datetime-series-loc (DatetimeSeries (U Label (Listof Label) (Listof Boolean)) -> (U Datetime DatetimeSeries)))
(define (datetime-series-loc datetime-series label)
  (unless (DatetimeSeries-index datetime-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "datetime-series must have a label index." k))))

  (if (ListofBoolean? label)
      (datetime-series-loc-boolean datetime-series label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (datetime-series-label-ref datetime-series l))) (convert-to-label-lst label)))
            (vals : (Vectorof Datetime)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (datetime-series-label-ref datetime-series l)) label)) ListofDatetime?))
                 (list->vector (assert (datetime-series-label-ref datetime-series label) ListofDatetime?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-DatetimeSeries vals (build-index-from-list (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: datetime-series-iloc (DatetimeSeries (U Index (Listof Index)) -> (U Datetime DatetimeSeries)))
(define (datetime-series-iloc datetime-series idx)
  (let ((referencer (datetime-series-referencer datetime-series)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-ISeries
      (new-DatetimeSeries
       (for/vector: : (Vectorof Datetime) ([i idx])
         (vector-ref (datetime-series-data datetime-series) i))
       (build-index-from-list (map (lambda ([i : Index]) (idx->key (assert (DatetimeSeries-index datetime-series)) i)) idx)))
      (referencer idx))))


; label based
; for two different use cases:
; a.) Selecting rows by label/index
; b.) Selecting rows with a boolean / conditional lookup

; Valid inputs
; A single label, e.g. 'a'.
; A list or array of labels ['a', 'b', 'c'].
; A boolean array.

(: true? (Boolean -> Boolean))
(define (true? boolean)
  (not (false? boolean)))

(: datetime-series-loc-boolean (DatetimeSeries (Listof Boolean) -> (U Datetime DatetimeSeries)))
(define (datetime-series-loc-boolean datetime-series boolean-lst)
  (: data (Vectorof Datetime))
  (define data (datetime-series-data datetime-series))

  (: new-data (Vectorof Datetime))
  (define new-data (make-vector (length (filter true? boolean-lst)) (Datetime (Date 0 0 0) (Time 0 0 0 0 0))))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty iseries
          (new-DatetimeSeries (vector) #f))

      ; to achieve the single result case
      (for ([b boolean-lst]
            [d data])
        (begin
          (when b
            (begin              
              (vector-set! new-data new-data-idx (vector-ref data data-idx))
              (set! new-data-idx (add1 new-data-idx))))
          (set! data-idx (add1 data-idx)))))

  (if (= (vector-length new-data) 1)
      (vector-ref new-data 0)
      (new-DatetimeSeries new-data #f)))

; ***********************************************************

; ***********************************************************

(: map/datetime-series-data (DatetimeSeries (Datetime -> Datetime) -> DatetimeSeries))
(define (map/datetime-series-data series fn)
  (let ((old-data (DatetimeSeries-data series)))
    (DatetimeSeries #f
                    (build-vector (vector-length old-data)
                                  (λ: ((idx : Natural))
                                    (fn (vector-ref old-data idx)))))))
; ***********************************************************

