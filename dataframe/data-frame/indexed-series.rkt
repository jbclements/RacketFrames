;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: indexed-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket

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

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [is-labeled? (LabelIndex -> Boolean)]
 [label-sort-positional (LabelIndex [#:project LabelProjection] -> Labeling)]
 [label-sort-lexical (LabelIndex -> Labeling)]
 ;[gseries-length (GSeries -> Index)]
 ;[gseries-data (All (A) (GSeries A) -> (Vectorof A))]
 [build-multi-index-from-list ((Listof (Listof GenericType)) -> SIndex)]
 [build-index-from-list ((Listof IndexDataType) -> RFIndex)]
 [labeling (LabelIndex -> (Listof (Pair Label (Listof Index))))]
 [extract-index (RFIndex -> IndexType)]
 [is-indexed? (RFIndex -> Boolean)])

(provide
 SIndex IIndex FIndex DTIndex Labeling ListofLabel? ListofIndexDataType?
 Label Label? LabelProjection LabelIndex? RFIndex?
 RFIndex IndexDataType
 LabelIndex LabelIndex-index
 FIndex FloatIndex
 ;(struct-out GSeries)
 ;new-GSeries 
 ;gseries-iref
 ;map/GSeries 
 build-index-from-labels label-index key->lst-idx label->lst-idx idx->key idx->label)
; ***********************************************************

; ***********************************************************
(require
  "../util/datetime.rkt"
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

(define-predicate Datetime? Datetime)

(define-predicate ListofLabel? (Listof Label))

(define-predicate ListofInteger? (Listof Integer))

(define-predicate ListofFloat? (Listof Float))

(define-predicate ListofDatetime? (Listof Datetime))

(define-type Labeling (Listof (Pair Label (Listof Index))))

(define-type SIndex (HashTable Label (Listof Index)))

; Range Index
(define-type IIndex (HashTable Integer (Listof Index)))

(define-type FIndex (HashTable Float (Listof Index)))

(define-type DTIndex (HashTable Datetime (Listof Index)))

(define-type IndexDataType (U Label Integer Float Datetime))

(define-predicate ListofIndexDataType? (Listof IndexDataType))

(define-type LabelProjection (U (Listof Label) (Setof Label)))

; like in Pandas, it could be dictionary of labels to values or not
; that's what the LabelIndex is for
(struct LabelIndex ([index : SIndex]) #:mutable)
(struct IntegerIndex ([index : IIndex]) #:mutable)
(struct FloatIndex ([index : FIndex]) #:mutable)
(struct DatetimeIndex ([index : DTIndex]) #:mutable)

(define-type IndexType (U SIndex IIndex FIndex DTIndex))

; add RangeIndex later
(define-type RFIndex (U LabelIndex IntegerIndex FloatIndex DatetimeIndex))

(define-predicate RFIndex? RFIndex)
; ***********************************************************

; ***********************************************************
; This function consumes a list of Labels and produces a
; SIndex which is a HashTable Label to Index.
(: build-index-from-list ((Listof IndexDataType) -> RFIndex))
(define (build-index-from-list lst)
  (cond
    [(ListofLabel? lst) (LabelIndex (build-index-from-labels lst))]
    [(ListofInteger? lst) (IntegerIndex (build-index-from-integers lst))]
    [(ListofFloat? lst) (FloatIndex (build-index-from-floats lst))]
    [(ListofDatetime? lst) (DatetimeIndex (build-index-from-datetimes lst))]
    [else (error "Unsupported index datatype.")]))

(: get-index (RFIndex IndexDataType -> (Listof Index)))
(define (get-index index item)
  (cond
    [(and (LabelIndex? index) (Label? item)) (label-index (LabelIndex-index index) item)]
    [(and (IntegerIndex? index) (exact-integer? item)) (integer-index (IntegerIndex-index index) item)]
    [(and (FloatIndex? index) (flonum? item)) (float-index (FloatIndex-index index) item)]
    [(and (DatetimeIndex? index) (Datetime? item)) (datetime-index (DatetimeIndex-index index) item)]
    [else (error "Unsupported index datatype.")]))

(: extract-index (RFIndex -> IndexType))
(define (extract-index index)
  (cond
    [(LabelIndex? index) (LabelIndex-index index)]
    [(IntegerIndex? index) (IntegerIndex-index index)]
    [(FloatIndex? index) (FloatIndex-index index)]
    [(DatetimeIndex? index) (DatetimeIndex-index index)]
    [else (error "Unsupported index datatype.")]))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: is-indexed? (RFIndex -> Boolean))
(define (is-indexed? index)
  (cond
    [(LabelIndex? index) (is-labeled? index)]
    [(IntegerIndex? index) (has-integer-index? index)]
    [(FloatIndex? index) (has-float-index? index)]
    [(DatetimeIndex? index) (has-datetime-index? index)]
    [else (error "Unsupported index datatype.")]))

(: key->lst-idx (RFIndex IndexDataType -> (Listof Index)))
(define (key->lst-idx index item)
  (cond
    [(LabelIndex? index) (label->lst-idx index (assert item Label?))]
    [(IntegerIndex? index) (integer->lst-idx index (assert item exact-integer?))]
    [(FloatIndex? index) (float->lst-idx index (assert item flonum?))]
    [(DatetimeIndex? index) (datetime->lst-idx index (assert item Datetime?))]
    [else (error "Unsupported index datatype.")]))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->key (RFIndex Index -> IndexDataType))
(define (idx->key index idx)
  (cond
    [(LabelIndex? index) (idx->label index idx)]
    [(IntegerIndex? index) (idx->integer index idx)]
    [(FloatIndex? index) (idx->float index idx)]
    [(DatetimeIndex? index) (idx->datetime index idx)]
    [else (error "Unsupported index datatype.")]))

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
; This function consumes a list of Labels and produces a
; SIndex which is a HashTable Label to Index.
(: build-index-from-integers ((Listof Integer) -> IIndex))
(define (build-index-from-integers integers)
  (let ((index : IIndex (make-hash '())))
    (let loop : IIndex ((idx : Index 0) (integers : (Listof Integer) integers))
      (if (null? integers)
          index
          (begin
            (hash-update! index (car integers)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr integers)))))))

(: integer-index (IIndex Integer -> (Listof Index)))
(define (integer-index index integer)      
  (hash-ref index integer))
; ***********************************************************

; ***********************************************************
; This function consumes a list of Floats and produces a
; FIndex which is a HashTable Label to Index.
(: build-index-from-floats ((Listof Float) -> FIndex))
(define (build-index-from-floats floats)
  (let ((index : FIndex (make-hash '())))
    (let loop : FIndex ((idx : Index 0) (floats : (Listof Float) floats))
      (if (null? floats)
          index
          (begin
            (hash-update! index (car floats)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr floats)))))))

(: float-index (FIndex Float -> (Listof Index)))
(define (float-index index float)      
  (hash-ref index float))
; ***********************************************************

; ***********************************************************
; This function consumes a list of Datetimes and produces a
; DTIndex which is a HashTable Label to Index.
(: build-index-from-datetimes ((Listof Datetime) -> DTIndex))
(define (build-index-from-datetimes datetimes)
  (let ((index : DTIndex (make-hash '())))
    (let loop : DTIndex ((idx : Index 0) (datetimes : (Listof Datetime) datetimes))
      (if (null? datetimes)
          index
          (begin
            (hash-update! index (car datetimes)
                          (λ: ((lst-index : (Listof Index)))
                            (append lst-index (list idx)))
                          (λ () (list)))

            
            (loop (assert (+ idx 1) index?) (cdr datetimes)))))))

(: datetime-index (DTIndex Datetime -> (Listof Index)))
(define (datetime-index index datetime)      
  (hash-ref index datetime))
; ***********************************************************

; ***********************************************************
#|
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
         (GSeries #f data)))) |#
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
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: has-integer-index? (IntegerIndex -> Boolean))
(define (has-integer-index? index)
  (if (extract-index index) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: integer->lst-idx (IntegerIndex Integer -> (Listof Index)))
(define (integer->lst-idx index integer)
  (let ((index : IIndex (IntegerIndex-index index)))
    (if index
        (hash-ref index integer)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->integer (IntegerIndex Index -> Integer))
(define (idx->integer index idx)
  (let ((index : IIndex (IntegerIndex-index index)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Integer (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: has-float-index? (FloatIndex -> Boolean))
(define (has-float-index? index)
  (if (extract-index index) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: float->lst-idx (FloatIndex Float -> (Listof Index)))
(define (float->lst-idx index float)
  (let ((index : FIndex (FloatIndex-index index)))
    (if index
        (hash-ref index float)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->float (FloatIndex Index -> Float))
(define (idx->float index idx)
  (let ((index : FIndex (FloatIndex-index index)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Float (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

; ***********************************************************
; This function consumes a series and returns a boolean
; indicating whether series is a SIndex or not.
(: has-datetime-index? (DatetimeIndex -> Boolean))
(define (has-datetime-index? index)
  (if (extract-index index) #t #f))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: datetime->lst-idx (DatetimeIndex Datetime -> (Listof Index)))
(define (datetime->lst-idx index datetime)
  (let ((index : DTIndex (DatetimeIndex-index index)))
    (if index
        (hash-ref index datetime)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

; This function consumes LabelIndex and Label and returns the
; numerical Index of the Label in the LabelIndex. The index
; must be a SIndex else an exception is raised.
(: idx->datetime (DatetimeIndex Index -> Datetime))
(define (idx->datetime index idx)
  (let ((index : DTIndex (DatetimeIndex-index index)))
    (if index
        (car (car (filter (lambda ([pair : (Pair Datetime (Listof Index))]) (member idx (cdr pair))) (hash->list index))))
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))
; ***********************************************************

#|
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
|#

; ***********************************************************
; This function consumes a LabelIndex which as long as it is
; a valid SIndex which is a HashTable, it converts it to a
; list of Label Index pairs.
(: labeling (LabelIndex -> (Listof (Pair Label (Listof Index)))))
(define (labeling lindex)
  (hash->list (LabelIndex-index lindex)))
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

;(build-multi-index-from-list (list (list 'a 'b 'c) (list 1 2 3)))

;(build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3)))
; ***********************************************************