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
 [datetime-series-index (DatetimeSeries -> (U False RFIndex))]
 [map/datetime-series-data (DatetimeSeries (Datetime -> Datetime) -> DatetimeSeries)]

 [bop/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Datetime) -> DatetimeSeries)]
 ;[comp/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Boolean) -> BSeries)]
 [+/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries)]
 [-/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries)]
 ;[+./datetime-series (DatetimeSeries Datetime -> DatetimeSeries)]
 ;[-./datetime-series (DatetimeSeries Datetime -> DatetimeSeries)]
 ;[>/datetime-series (DatetimeSeries DatetimeSeries -> BSeries)]
 ;[</is (DatetimeSeries DatetimeSeries -> BSeries)]
 ;[>=/is (DatetimeSeries DatetimeSeries -> BSeries)]
 ;[<=/is (DatetimeSeries DatetimeSeries -> BSeries)]
 ;[=/is (DatetimeSeries DatetimeSeries -> BSeries)]
 ;[!=/is (DatetimeSeries DatetimeSeries -> BSeries)]
 
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
  (new-DatetimeSeries (datetime-series-data datetime-series) labels))
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

; This function consumes a datetime series and returns its
; index.
(: datetime-series-index (DatetimeSeries -> (U False RFIndex)))
(define (datetime-series-index series)
  (DatetimeSeries-index series))

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

(: bop/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Datetime) -> DatetimeSeries))
(define (bop/datetime-series datetime-series-1 datetime-series-2 bop)
  (define v1 (DatetimeSeries-data datetime-series-1))
  (define v2 (DatetimeSeries-data datetime-series-2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'bop/datetime-series "Series must be of equal length."))

  (define: v-bop : (Vectorof Datetime) (make-vector len #{(Datetime (Date 0 0 0) (Time 0 0 0 0 0)) : Datetime}))

  ; Do loop returns DatetimeSeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : DatetimeSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (DatetimeSeries #f v-bop))
       (vector-set! v-bop idx (bop (vector-ref v1 idx)
				   (vector-ref v2 idx)))))

; (: comp/datetime-series (DatetimeSeries DatetimeSeries (Datetime Datetime -> Boolean) -> BSeries))
; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction,
; using datetime+ and datetime- and the bop/datetime-series function defined
; above.

(: datetime+ (Datetime Datetime -> Datetime))
(define (datetime+ datetime-1 datetime-2)
  (let* ([date-1 : Date (datetime-date datetime-1)]
         [date-2 : Date (datetime-date datetime-2)]
         [time-1 : Time (datetime-time datetime-1)]
         [time-2 : Time (datetime-time datetime-2)]
         [year-1 : Integer (date-year date-1)]
         [year-2 : Integer (date-year date-2)]
         [month-1 : Integer (date-month date-1)]
         [month-2 : Integer (date-month date-2)]
         [day-1 : Integer (date-day date-1)]
         [day-2 : Integer (date-day date-2)]
         [offset-1 : Integer (time-offset time-1)]
         [offset-2 : Integer (time-offset time-2)]
         [hour-1 : Integer (time-hour time-1)]
         [hour-2 : Integer (time-hour time-2)]
         [minute-1 : Integer (time-minute time-1)]
         [minute-2 : Integer (time-minute time-2)]
         [second-1 : Integer (time-second time-1)]
         [second-2 : Integer (time-second time-2)]
         [milli-1 : Integer (time-milli time-1)]
         [milli-2 : Integer (time-milli time-2)])

    (Datetime (Date (+ year-1 year-2) (+ month-1 month-2) (+ day-1 day-2))
              (Time (+ offset-1 offset-2) (+ hour-1 hour-2) (+ minute-1 minute-2) (+ second-1 second-2) (+ milli-1 milli-2)))))

(: datetime- (Datetime Datetime -> Datetime))
(define (datetime- datetime-1 datetime-2)
  (let* ([date-1 : Date (datetime-date datetime-1)]
         [date-2 : Date (datetime-date datetime-2)]
         [time-1 : Time (datetime-time datetime-1)]
         [time-2 : Time (datetime-time datetime-2)]
         [year-1 : Integer (date-year date-1)]
         [year-2 : Integer (date-year date-2)]
         [month-1 : Integer (date-month date-1)]
         [month-2 : Integer (date-month date-2)]
         [day-1 : Integer (date-day date-1)]
         [day-2 : Integer (date-day date-2)]
         [offset-1 : Integer (time-offset time-1)]
         [offset-2 : Integer (time-offset time-2)]
         [hour-1 : Integer (time-hour time-1)]
         [hour-2 : Integer (time-hour time-2)]
         [minute-1 : Integer (time-minute time-1)]
         [minute-2 : Integer (time-minute time-2)]
         [second-1 : Integer (time-second time-1)]
         [second-2 : Integer (time-second time-2)]
         [milli-1 : Integer (time-milli time-1)]
         [milli-2 : Integer (time-milli time-2)])

    (Datetime (Date (- year-1 year-2) (- month-1 month-2) (- day-1 day-2))
              (Time (- offset-1 offset-2) (- hour-1 hour-2) (- minute-1 minute-2) (- second-1 second-2) (- milli-1 milli-2)))))

(: +/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries))
(define (+/datetime-series datetime-1 datetime-2)
  (bop/datetime-series datetime-1 datetime-2 datetime+))

(: -/datetime-series (DatetimeSeries DatetimeSeries -> DatetimeSeries))
(define (-/datetime-series datetime-1 datetime-2)
  (bop/datetime-series datetime-1 datetime-2 datetime-))

; ***********************************************************

