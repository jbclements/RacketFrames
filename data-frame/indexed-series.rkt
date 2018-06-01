;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
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
 [gseries-length (GSeries -> Index)]
 [gseries-data (All (A) (GSeries A) -> (Vectorof A))]
 [build-multi-index-from-list ((Listof (Listof GenericType)) -> SIndex)])

(provide
 SIndex Labeling
 Label Label? LabelProjection
 LabelIndex LabelIndex-index
 (struct-out GSeries)
 new-GSeries 
 series-ref gseries-iref
 map/GSeries 
 build-index-from-labels label-index label->lst-idx idx->label)
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

(define-type Labeling (Listof (Pair Label (Listof Index))))

(define-type SIndex (HashTable Label (Listof Index)))

(define-type LabelProjection (U (Listof Label) (Setof Label)))

; like in Pandas, it could be dictionary of labels to values or not
; that's what the LabelIndex is for
(struct LabelIndex ([index : (Option SIndex)]) #:mutable)
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
            ;(hash-set! index (car labels) idx)

            (hash-update! index (car labels)
			      (λ: ((lst-index : (Listof Index)))
				  (append lst-index (list idx)))
			      (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr labels)))))))

(: label-index (SIndex Label -> (Listof Index)))
(define (label-index index label)      
  (hash-ref index label))
; ***********************************************************

; ***********************************************************
; General Series parameterized by A, allows for generic types.
; Numeric and categorical series will be further optimized.
(struct (A) GSeries LabelIndex ([data : (Vectorof A)]))

(: gseries-data (All (A) (GSeries A) -> (Vectorof A)))
(define (gseries-data gseries)
  (GSeries-data gseries))

(: get-total-index-value-count (SIndex -> Integer))
(define (get-total-index-value-count index)
  (define value-count 0)

  (for ([hash-val (hash-values index)])
    (set! value-count (+ value-count (length hash-val))))

  value-count)
  
; Consumes a Vector of generic type and a list of Labels which
; can come in list form or SIndex form and produces a GSeries
; struct object.
(: new-GSeries (All (A) (Vectorof A) (Option (U (Listof Label) SIndex)) -> (GSeries A)))
(define (new-GSeries data labels)
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (get-total-index-value-count index))
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
(: label->lst-idx (LabelIndex Label -> (Listof Index)))
(define (label->lst-idx series label)
  (let ((index (LabelIndex-index series)))
    (if index
        (hash-ref index label)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))


; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->label (LabelIndex Index -> Label))
(define (idx->label series idx)
  (let ((index (LabelIndex-index series)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Label (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and an Listof Index and returns
; the value at that index in the series.
(: gseries-iref (All (A) (GSeries A) (Listof Index) -> (U (Listof Float) (Listof A))))
(define (gseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (GSeries-data series) idx))
       lst-idx))

; This function consumes a series and a Label and returns
; the value at that Label in the series.
(: series-ref (All (A) (GSeries A) Label -> (U (Listof A) (Listof Float))))
(define (series-ref series label)
  (gseries-iref series (label->lst-idx series label)))

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
(: labeling (LabelIndex -> (Listof (Pair Label (Listof Index)))))
(define (labeling lindex)
  (hash->list (assert (LabelIndex-index lindex))))
; ***********************************************************

; ***********************************************************
; This function consumes a LabelIndex and produces a sorted
; Labeling which is a list consisting of Label Index pairs.
; The Labeling is sorted on the Label.
(: label-sort-lexical (LabelIndex -> Labeling))
(define (label-sort-lexical lindex)
  ((inst sort (Pair Label (Listof Index)) (Pair Label (Listof Index)))
   (labeling lindex)
   (λ: ((kv1 : (Pair Label (Listof Index)))
	(kv2 : (Pair Label (Listof Index))))
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
  
  (let ((labels ((inst sort (Pair Symbol (Listof Index)) (Pair Symbol (Listof Index)))
                 (labeling lindex)
                 (λ: ((kv1 : (Pair Symbol (Listof Index)))
                      (kv2 : (Pair Symbol (Listof Index))))
                   ; just comapres first of the list of index
                   (< (car (cdr kv1)) (car (cdr kv2)))))))

    (if (set-empty? projection)
        labels
        (filter (λ: ((label : (Pair Symbol (Listof Index))))
		    (set-member? projection (car label)))
		labels))))
; ***********************************************************

(define-type GenericType Any)

(define key-delimiter "\t")

; This function consumes a Listof IndexableSeries and builds key
; string from the columns of a frame and a given set of col labels to use.
; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...
(: key-fn-list ((Listof (Listof GenericType)) -> (Index -> String)))
(define (key-fn-list lsts)
  (λ: ((row-id : Index))
    (let ((outp (open-output-string)))
      (for ([lst (in-list lsts)])
        (let*: ((seg : GenericType (list-ref lst row-id))
                (seg-str : String (cond
                                    [(symbol? seg) (symbol->string seg)]
                                    [(number? seg) (number->string seg)]
                                    ; pretty-format anything else
                                    [else (pretty-format seg)])))
          (display seg-str outp)
          (display key-delimiter outp)))
      (get-output-string outp))))

; This function consumes a Listof Listof Label and creates
; a MultiIndex.
(: build-multi-index-from-list ((Listof (Listof GenericType)) -> SIndex))
(define (build-multi-index-from-list nested-label-lst)

  ; Get length of one of the IndexableSeries
  (define len (length (car nested-label-lst)))
  (define: series-key : (Index -> String) (key-fn-list nested-label-lst))

  (let ((index : SIndex (make-hash '())))

    (let loop ([i 0])
      (if (>= i len)
          index
          (let: ((i : Index (assert i index?)))
            (let ((key (series-key i)))              
              (hash-update! index (string->symbol key)
                            (λ: ((idx : (Listof Index)))
                              (append idx (list i)))
                            (λ () (list))))
            (loop (add1 i)))))))

(build-multi-index-from-list (list (list 'a 'b 'c) (list 1 2 3)))

(build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3)))

; ***********************************************************
; Test Cases
; ***********************************************************

; Not sure how to check
;(check-equal? (build-index-from-labels (list 'a 'b 'c 'd))
;              (hash 'b 1 'c' 2 'd 3 'a 0))

; checks numerical index of label
(check-equal? (label-index (build-index-from-labels (list 'a 'b 'c 'd)) 'a) (list 0))
(check-equal? (label-index (build-index-from-labels (list 'a 'b 'c 'd)) 'c) (list 2))

; checks numerical idx of LabelIndex
(check-equal? (label->lst-idx (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 'd) (list 3))
(check-equal? (label->lst-idx (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 'c) (list 2))

; checks numerical idx of LabelIndex
(check-equal? (idx->label (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 3) 'd)
(check-equal? (idx->label (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))) 2) 'c)

; checks to see if we have a labelled index

(check-equal? (is-labeled? (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd)))) #t)
(check-equal? (is-labeled? (LabelIndex #f)) #f)

; generic series tests

; create integer series
(define g-series-integer (new-GSeries (vector 1 2 3 4) (build-index-from-labels (list 'a 'b 'c 'd))))

(define g-series-integer-2 (new-GSeries (vector 1 2 3 4 5) (build-index-from-labels (list 'a 'b 'c 'd 'a))))

; create float series
(define g-series-float (new-GSeries (vector 1.5 2.5 3.5 4.5 5.5) (build-index-from-labels (list 'a 'b 'c 'd 'e))))

; create symbol series
(define g-series-symbol (new-GSeries (vector 'e 'f 'g 'h) (list 'a 'b 'c 'd)))

; point struct
(struct point ([x : Integer] [y : Integer]) #:transparent)

; create point struct series
(define g-series-point (new-GSeries (vector (point 1 2) (point 3 4) (point 5 6) (point 7 8) (point 9 10)) (build-index-from-labels (list 'a 'b 'c 'd 'e))))

;(gseries-data g-series-point)

; point series ref by index
;(gseries-iref g-series-point (list 2))

; point series ref by label
;(series-ref g-series-point 'd)

;(gseries-data (map/GSeries g-series-point (λ: ((p : point))
;                                (point-x p))))

; integer series ref by index
(check-equal? (gseries-iref g-series-integer (list 2)) (list 3))

; integer series ref by label
(check-equal? (series-ref g-series-integer 'd) (list 4))

(check-equal? (series-ref g-series-integer-2 'a) (list 1 5))

; symbol series ref by index
;(check-equal? (gseries-iref g-series-symbol 2) (list 'g))

; symbol series ref by label
(check-equal? (series-ref g-series-symbol 'd) (list 'h))

; series length
(check-equal? (gseries-length g-series-symbol) 4)

; series map
(check-equal? (GSeries-data (map/GSeries g-series-integer (λ: ((x : Integer))
                                                            (add1 x)))) #(2 3 4 5))
; create struct series
(define g-series-struct (new-GSeries (vector (LabelIndex #f) (LabelIndex #f)) (build-index-from-labels (list 'a 'b))))

; struct series ref by index
;(check-equal? (LabelIndex-index (assert (gseries-iref g-series-struct 1) LabelIndex?)) (LabelIndex-index (LabelIndex #f)))

; integer series ref by label
; checks labeling function which converts labels hash to list
(check-equal? (labeling (LabelIndex (build-index-from-labels (list 'a 'b 'c 'd))))
              '((b 1) (c 2) (d 3) (a 0)))

; checks label sorting
(check-equal? (label-sort-lexical (LabelIndex (build-index-from-labels (list 'b 'd 'a 'c))))
              '((a 2) (b 0) (c 3) (d 1)))

; check label sorting by position
; (check-equal? (label-sort-positional (LabelIndex (build-index-from-labels (list 'b 'd 'a 'c))))
;              '((b . 0) (d . 1) (a . 2) (c . 3)))
