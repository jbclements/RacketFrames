#lang typed/racket

(require typed/rackunit)

(require
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex
          label-index label->lst-idx
          idx->label is-labeled?))

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
 [set-GenSeries-index (GenSeries (U (Listof Label) SIndex) -> GenSeries)]
 [gen-series-iref (GenSeries (Listof Index) -> GenericType)]
 [gen-series-label-ref (GenSeries Label -> GenericType)]
 [gen-series-range (GenSeries Index -> (Vectorof GenericType))]
 [gen-series-length (GenSeries -> Index)]
 [gen-series-referencer (GenSeries -> (Index -> GenericType))]
 [gen-series-data (GenSeries -> (Vectorof GenericType))]
 [gen-series-loc-boolean (GenSeries (Listof Boolean) -> (U GenericType GenSeries))]
 [gen-series-loc (GenSeries (U Label (Listof Label) (Listof Boolean)) -> (U GenericType GenSeries))]
 [gen-series-iloc (GenSeries (U Index (Listof Index)) -> (U GenericType GenSeries))]
 [map/gen-s (GenSeries (GenericType -> GenericType) -> GenSeries)])

; ***********************************************************

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

; ***********************************************************
(: set-GenSeries-index (GenSeries (U (Listof Label) SIndex) -> GenSeries))
(define (set-GenSeries-index iseries labels)

  (define data (GenSeries-data iseries))
  
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
      (let ((index (build-index-from-labels labels)))
        (check-mismatch index)
        (GenSeries index data))))
; ***********************************************************

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
(: gen-series-iref (GenSeries (Listof Index) -> (Listof GenericType)))
(define (gen-series-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (GenSeries-data series) idx))
       lst-idx))

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
(: gen-series-label-ref (GenSeries Label -> (Listof GenericType)))
(define (gen-series-label-ref series label)
  (gen-series-iref series (label->lst-idx series label)))

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

; ***********************************************************
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

(define-predicate ListofBoolean? (Listof Boolean))

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

(: gen-series-loc-boolean (GenSeries (Listof Boolean) -> (U GenericType GenSeries)))
(define (gen-series-loc-boolean gen-series boolean-lst)
  (: data (Vectorof GenericType))
  (define data (gen-series-data gen-series))

  (: new-data (Vectorof GenericType))
  (define new-data (make-vector (length (filter true? boolean-lst)) #f))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty GenSeries
          (new-GenSeries (vector) #f))
       
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
      (new-GenSeries new-data #f)))
    
(: gen-series-loc (GenSeries (U Label (Listof Label) (Listof Boolean)) -> (U GenericType GenSeries)))
(define (gen-series-loc gen-series label)
  (unless (is-labeled? gen-series)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "gen-series must have a label index." k))))

  (if (ListofBoolean? label)
      (gen-series-loc-boolean gen-series label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (gen-series-label-ref gen-series l))) (convert-to-label-lst label)))
            (vals : (Vectorof GenericType)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (gen-series-label-ref gen-series l)) label)) ListofBoolean?))
                 (list->vector (assert (gen-series-label-ref gen-series label) ListofBoolean?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-GenSeries vals (build-index-from-labels (build-labels-by-count (convert-to-label-lst label) associated-indices-length)))))))

; index based
(: gen-series-iloc (GenSeries (U Index (Listof Index)) -> (U GenericType GenSeries)))
(define (gen-series-iloc gen-series idx)
  (let ((referencer (gen-series-referencer gen-series)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-GenSeries
      (new-GenSeries
       (for/vector: : (Vectorof GenericType) ([i idx])
         (vector-ref (gen-series-data gen-series) i))
       (build-index-from-labels (map (lambda ([i : Index]) (idx->label gen-series i)) idx)))
      (referencer idx))))

; ***********************************************************

; create generic series
(define series-generic (new-GenSeries (vector 1 2.5 'categorical #t)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-generic-2 (new-GenSeries (vector 5 6 7 8)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; gen-series reference tests
(check-equal? ((gen-series-referencer series-generic) 0) 1)

(check-equal? ((gen-series-referencer series-generic) 1) 2.5)

(check-equal? (gen-series-iref series-generic (list 0)) (list 1))

(check-equal? (gen-series-iref series-generic (list 1)) (list 2.5))

(check-equal? (gen-series-label-ref series-generic 'd) (list #t))

(check-equal? (gen-series-label-ref series-generic 'c) (list 'categorical))

; gen-series length
(check-equal? (gen-series-length series-generic) 4)

; point struct
(struct point ([x : Integer] [y : Integer]) #:transparent)

; create point struct series
(define gen-series-point (new-GenSeries (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)) (build-index-from-labels (list 'a 'b 'c 'd 'e))))

(gen-series-data gen-series-point)

; point series ref by index
(gen-series-iref gen-series-point (list 2))

; point series ref by label
(gen-series-label-ref gen-series-point 'd)
