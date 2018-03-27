;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
(require typed/rackunit)

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both integer (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The series can hold any generic type.

; NOT IMPLEMENTED YET
; Operations between Series (+, -, /, *, **) align values based on their
; associated index values. The result index will be the sorted union of
; the two indexes.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [is-labeled? (LabelIndex -> Boolean)]
 [label-sort-positional (LabelIndex [#:project LabelProjection] -> Labeling)]
 [label-sort-lexical (LabelIndex -> Labeling)]
 [gseries-length (GSeries -> Index)])

(provide
 SIndex Labeling
 Label Label? LabelProjection
 LabelIndex LabelIndex-index
 (struct-out GSeries)
 new-GSeries 
 series-ref series-iref
 map/GSeries 
 build-index-from-labels label-index label->idx)
; ***********************************************************

; ***********************************************************
(require 
 (only-in racket/flonum
          make-flvector flvector? flvector
          flvector-ref flvector-set!
          flvector-length)
 (only-in racket/set
	  set-empty? set-member?
	  list->set))
; ***********************************************************

; ***********************************************************
(define-type Label Symbol)

(define-predicate Label? Label)

(define-type Labeling (Listof (Pair Label Index)))

(define-type SIndex (HashTable Label Index))

(define-type LabelProjection (U (Listof Label) (Setof Label)))

; like in Pandas, it could be dictionary of labels to values or not
; that's what the LabelIndex is for
(struct LabelIndex ([index : (Option SIndex)]))
; ***********************************************************

; ***********************************************************
; This function consumes a list of Labels and produces a
; SIndex which is a HashTable Label to Index.
(: build-index-from-labels ((Listof Label) -> SIndex))
(define (build-index-from-labels labels)
  (let ((index : SIndex (make-hash '())))
    (let loop : SIndex ((idx : Index 0) (labels : (Listof Label) labels))
      (if (null? labels)
          index
          (begin
            (hash-set! index (car labels) idx)
            (loop (assert (+ idx 1) index?) (cdr labels)))))))

(: label-index (SIndex Label -> Integer))
(define (label-index index label)      
  (hash-ref index label))
; ***********************************************************

; ***********************************************************
;; General Series parameterized by A, allows for generic types.
;; Numeric and categorical series will be further optimized.
(struct (A) GSeries LabelIndex ([data : (Vectorof A)]))

; Consumes a Vector of generic type and a list of Labels which
; can come in list form or SIndex form and produces a GSeries
; struct object.
(: new-GSeries (All (A) (Vectorof A) (Option (U (Listof Label) SIndex)) -> (GSeries A)))
(define (new-GSeries data labels)
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
        (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))
  
  (if(hash? labels)
     (begin
       (check-mismatch labels)
       (GSeries labels data))
     (if labels	 
         (let ((index (build-index-from-labels labels)))
           (check-mismatch index)
           (GSeries index data))
         (GSeries #f data))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: is-labeled? (LabelIndex -> Boolean))
(define (is-labeled? series)
  (if (LabelIndex-index series) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: label->idx (LabelIndex Label -> Index))
(define (label->idx series label)
  (let ((index (LabelIndex-index series)))
    (if index
        (hash-ref index label)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and an index and returns
; the value at that index in the series.
(: series-iref (All (A) (GSeries A) Index -> (U Float A)))
(define (series-iref series idx)
  (vector-ref (GSeries-data series) idx))

; This function consumes a series and a Label and returns
; the value at that Label in the series.
(: series-ref (All (A) (GSeries A) Label -> (U A Float)))
(define (series-ref series label)
  (series-iref series (label->idx series label)))

; This function consumes a series and returns the length
; of that series.
(: gseries-length (All (A) (GSeries A) -> Index))
(define (gseries-length series)
  (vector-length (GSeries-data series)))
; ***********************************************************

; ***********************************************************
; This function consumes a series and a function both of generic
; types and applies the function to each member of the series
; returning a new series.
(: map/GSeries (All (A B) (GSeries A) (A -> B) -> (GSeries B)))
(define (map/GSeries series fn)
  (let*: ((old-data : (Vectorof A) (GSeries-data series))
          (new-data : (Vectorof B) (build-vector (vector-length old-data) 
                                                 (λ: ((idx : Integer)) 
                                                   (fn (vector-ref old-data idx))))))
    (GSeries (LabelIndex-index series) new-data)))
; ***********************************************************

; ***********************************************************
; This function consumes a LabelIndex which as long as it is
; a valid SIndex which is a HashTable, it converts it to a
; list of Label Index pairs.
(: labeling (LabelIndex -> (Listof (Pair Label Index))))
(define (labeling lindex)
  (hash->list (assert (LabelIndex-index lindex))))
; ***********************************************************

; ***********************************************************
; This function consumes a LabelIndex and produces a sorted
; Labeling which is a list consisting of Label Index pairs.
; The Labeling is sorted on the Label.
(: label-sort-lexical (LabelIndex -> Labeling))
(define (label-sort-lexical lindex)
  ((inst sort (Pair Label Index) (Pair Label Index))
   (labeling lindex)
   (λ: ((kv1 : (Pair Symbol Index)) 
	(kv2 : (Pair Symbol Index)))
       (string<=? (symbol->string (car kv1))
		  (symbol->string (car kv2))))))
; ***********************************************************

; ***********************************************************
; This function consumes a LabelIndex and LabelProjection produces
; a sorted Labeling which is a list consisting of Label Index pairs.
; The Labeling is sorted on the index of the labels.
(: label-sort-positional (LabelIndex [#:project LabelProjection] -> Labeling))
(define (label-sort-positional lindex #:project [project '()])

  (define: projection : (Setof Label) (if (list? project) (list->set project) project))

  (let ((labels ((inst sort (Pair Symbol Index) (Pair Symbol Index))
		 (labeling lindex)
		 (λ: ((kv1 : (Pair Symbol Index)) 
		      (kv2 : (Pair Symbol Index)))
		     (< (cdr kv1) (cdr kv2))))))

    (if (set-empty? projection)
	labels
	(filter (λ: ((label : (Pair Label Index)))
		    (set-member? projection (car label)))
		labels))))
; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************

; Not sure how to check
;(check-equal? (build-index-from-labels (list 'a 'b 'c 'd))
;              (hash 'b 1 'c' 2 'd 3 'a 0))

; checks numerical index of label
(check-equal? (label-index (build-index-from-labels (list 'a 'b 'c 'd)) 'a) 0)
(check-equal? (label-index (build-index-from-labels (list 'a 'b 'c 'd)) 'c) 2)

; checks numerical idx of LabelIndex
(check-equal? (label->idx (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 'd) 3)
(check-equal? (label->idx (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 'c) 2)

; checks to see if we have a labelled index
(check-equal? (is-labeled? (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd)))) #t)
(check-equal? (is-labeled? (LabelIndex #f)) #f)

; generic series tests
; create integer series
(define g-series-integer (new-GSeries (vector 1 2 3 4) (build-index-from-labels (list 'a 'b 'c 'd))))
; create symbol series
(define g-series-symbol (new-GSeries (vector 'e 'f 'g 'h) (list 'a 'b 'c 'd)))

; integer series ref by index
(check-equal? (series-iref g-series-integer 2) 3)

; integer series ref by label
(check-equal? (series-ref g-series-integer 'd) 4)

; symbol series ref by index
(check-equal? (series-iref g-series-symbol 2) 'g)

; symbol series ref by label
(check-equal? (series-ref g-series-symbol 'd) 'h)

; series length
(check-equal? (gseries-length g-series-symbol) 4)

; series map
(check-equal? (GSeries-data (map/GSeries g-series-integer (λ: ((x : Integer))
                                                            (add1 x)))) #(2 3 4 5))

; checks labeling function which converts labels hash to list
(check-equal? (labeling (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))))
              '((b . 1) (c . 2) (d . 3) (a . 0)))

; checks label sorting
(check-equal? (label-sort-lexical (LabelIndex (build-index-from-labels (list 'b 'd 'a 'c))))
              '((a . 2) (b . 0) (c . 3) (d . 1)))

; check label sorting by position
(check-equal? (label-sort-positional (LabelIndex (build-index-from-labels (list 'b 'd 'a 'c))))
              '((b . 0) (d . 1) (a . 2) (c . 3)))
