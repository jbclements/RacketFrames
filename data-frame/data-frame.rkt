;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
(require typed/rackunit)

; ***********************************************************
; A map of series to label names, represented as a collection
; of columns.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [column (Label Series -> Column)]
 [column-heading (Column -> Label)]
 [column-series (Column -> Series)]
 [data-frame-rename (DataFrame Label Label -> DataFrame)]
 [data-frame-project (DataFrame LabelProjection -> DataFrame)]
 [data-frame-drop (DataFrame Label -> DataFrame)]
 [data-frame-remove (DataFrame LabelProjection -> DataFrame)]
 [data-frame-explode (DataFrame [#:project LabelProjection] -> Columns)]
 [data-frame-replace (DataFrame Column -> DataFrame)]
 [data-frame-extend  (DataFrame (U Column Columns DataFrame) -> DataFrame)]
 [data-frame-description (DataFrame [#:project LabelProjection] -> DataFrameDescription)]
 [show-data-frame-description (DataFrameDescription -> Void)])

(provide
 DataFrame Column Columns
 (struct-out DataFrameDescription)
 data-frame-series
 data-frame-names data-frame-dim 
 data-frame-cseries data-frame-nseries
 data-frame-series
 new-data-frame)

; ***********************************************************

; ***********************************************************
(require 
 (only-in racket/vector
	  vector-copy)
 (only-in racket/set
	  set-empty? set-member? set-subtract
	  list->set)
 (only-in "types.rkt"
          Dim Dim-rows Dim-cols)
 (only-in "indexed-series.rkt"
	  label-sort-positional
          Label LabelProjection LabelIndex LabelIndex-index
          GSeries SIndex
          build-index-from-labels label-index)
 (only-in "series-description.rkt"
	  series-description series-length series-type
          Series 
          SeriesDescription SeriesDescription-name
          SeriesDescription-type SeriesDescription-length)
 (only-in "categorical-series.rkt"
          CSeries CSeries?
          CSeries-data
          new-CSeries)
 (only-in "numeric-series.rkt"
          NSeries NSeries? 
          NSeries-data
          new-NSeries)
 (only-in "integer-series.rkt"
	  ISeries ISeries?
	  ISeries-data
	  new-ISeries))

; ***********************************************************

; ***********************************************************
(define-type Column  (Pair Label Series))
(define-type Columns (Listof Column))

;; A DataFrame is map of series.
(struct: DataFrame LabelIndex ([series : (Vectorof Series)]))

(struct: DataFrameDescription ([dimensions : Dim]
                           [series : (Listof SeriesDescription)]))

; ***********************************************************

; ***********************************************************
(: column (Label Series -> Column))
(define (column label series)
  (cons label series))

(: column-heading (Column -> Label))
(define (column-heading col)
  (car col))

(: column-series (Column -> Series))
(define (column-series col)
  (cdr col))

; ***********************************************************

; ***********************************************************
(: new-data-frame (Columns -> DataFrame))
(define (new-data-frame cols)
  
  (define (check-equal-length)
    (when  (pair? cols)
	   (let ((len (if (null? cols) 
			  0 
			  (series-length (cdr (car cols))))))
	     (unless (andmap (λ: ((s : (Pair Symbol Series)))
				 (eq? len (series-length (cdr s))))
			     (cdr cols))
		     (error 'new-frame "Frame must have equal length series: ~a" 
			    (map (λ: ((s : (Pair Symbol Series)))
				     (series-description (car s) (cdr s)))
				 cols))))))
  
  (check-equal-length)
  (let ((index (build-index-from-labels ((inst map Label Column)
                                         (inst car Label Series) cols)))
        (data (apply vector ((inst map Series Column) cdr cols))))
    (DataFrame index data)))

; ***********************************************************

; ***********************************************************

(: data-frame-rename (DataFrame Label Label -> DataFrame))
(define (data-frame-rename data-frame from to)
  (let ((index (LabelIndex-index data-frame)))
    (if index
	(let: ((col-idx : (Option Index) (hash-ref index from (λ () #f))))
	      (if col-idx
		  (let ((new-index (hash-copy index)))
		    (hash-remove! new-index from)
		    (hash-set! new-index to col-idx)
		    (DataFrame new-index (vector-copy (DataFrame-series data-frame))))
		  data-frame))
	data-frame)))

; ***********************************************************

; ***********************************************************

(: data-frame-drop (DataFrame Label -> DataFrame))
(define (data-frame-drop data-frame label)
  (new-data-frame (filter (λ: ((col : Column))
			 (not (eq? (car col) label)))
		     (data-frame-explode data-frame))))

; ***********************************************************

; ***********************************************************
(: data-frame-series (DataFrame Symbol -> Series))
(define (data-frame-series data-frame col)
  (vector-ref (DataFrame-series data-frame)
              (label-index (assert (LabelIndex-index data-frame)) col)))

(: data-frame-cseries (DataFrame Symbol -> CSeries))
(define (data-frame-cseries data-frame name)
  (assert (data-frame-series data-frame name) CSeries?))

(: data-frame-nseries (DataFrame Symbol -> NSeries))
(define (data-frame-nseries data-frame name)
  (assert (data-frame-series data-frame name) NSeries?))

(: DataFrame-iseries (DataFrame Symbol -> ISeries))
(define (DataFrame-iseries data-frame name)
  (assert (data-frame-series data-frame name) ISeries?))

(: data-frame-labels (DataFrame -> (Listof (Pair Symbol Index))))
(define (data-frame-labels data-frame)
  (hash->list (assert (LabelIndex-index data-frame))))

; ***********************************************************

; ***********************************************************

(: data-frame-names (DataFrame -> (Listof Symbol)))
(define (data-frame-names data-frame)  
  (map (λ: ((kv : (Pair Symbol Integer)))
	   (car kv))
       ((inst sort (Pair Symbol Index) (Pair Symbol Index))
        (data-frame-labels data-frame)
        (λ: ((kv1 : (Pair Symbol Index)) 
             (kv2 : (Pair Symbol Index)))
	    (< (cdr kv1) (cdr kv2))))))

; ***********************************************************

; ***********************************************************

(: data-frame-dim (DataFrame -> Dim))
(define (data-frame-dim data-frame)
  (let ((cols (length (hash-keys (assert (LabelIndex-index data-frame))))))
    (if (zero? cols)
        (Dim 0 0)
        (let ((rows (let ((series (vector-ref (DataFrame-series data-frame) 0)))
                      (series-length series))))                      
          (Dim rows cols)))))

; ***********************************************************

; ***********************************************************

(: projection-set (LabelProjection -> (Setof Label)))
  (define (projection-set project)
    (if (list? project) 
	(list->set project) 
	project))

(: data-frame-all-labels-projection-set (DataFrame -> (Setof Label)))
(define (data-frame-all-labels-projection-set data-frame)
  (projection-set (map (inst car Symbol Any) (label-sort-positional data-frame))))
  
(: projection-filter (All (A) (Listof A) (A -> Symbol) LabelProjection -> (Listof A)))
(define (projection-filter lst sym-fn project)  
  (define projection (projection-set project))  
  (if (set-empty? projection)
      lst
      (filter (λ: ((a : A))
		  (set-member? projection (sym-fn a)))
	      lst)))

; ***********************************************************

; ***********************************************************

(: data-frame-description (DataFrame [#:project LabelProjection] -> DataFrameDescription))
(define (data-frame-description data-frame #:project [project '()])
  
  (let ((names (data-frame-names data-frame)))
    (let: loop : DataFrameDescription ((names : (Listof Label) names) 
				   (descs : (Listof SeriesDescription) '()))
	  (if (null? names)
	      (let ((dim (data-frame-dim data-frame))
		    (cols (projection-filter descs 
					     (λ: ((sd : SeriesDescription))
						 (SeriesDescription-name sd))
					     project)))
		(DataFrameDescription (Dim (Dim-rows dim)
				       (length cols))
				  (reverse cols)))
	      (let* ((name (car names))
		     (series (data-frame-series data-frame name)))
		(loop (cdr names) (cons (SeriesDescription name 
							   (series-type series) 
							   (series-length series))
					descs)))))))

;; Really need to enumerate a minimal set of generic functions, such as `show'
(: show-data-frame-description (DataFrameDescription -> Void))
(define (show-data-frame-description fdesc)
  
  (: print-series-description (SeriesDescription -> Void))
  (define (print-series-description sdesc)
    (displayln (format "  - ~a: ~a"
		       (SeriesDescription-name sdesc)
		       (SeriesDescription-type sdesc))))                      
  
  (let ((dim (DataFrameDescription-dimensions fdesc)))
    (displayln (format "DataFrame::(Cols: ~a, Rows: ~a)" (Dim-cols dim) (Dim-rows dim)))
    (for-each print-series-description (DataFrameDescription-series fdesc))))

; ***********************************************************

; ***********************************************************

(: data-frame-explode (DataFrame [#:project LabelProjection] -> Columns))
(define (data-frame-explode data-frame #:project [project '()])  
  (let ((labeling (label-sort-positional data-frame))
	(series (DataFrame-series data-frame)))
    (projection-filter (for/list: : Columns
				  ([label labeling])
				  (cons (car label)
					(vector-ref series (cdr label))))
		       (λ: ((l-s : Column))
			   (car l-s))
		       project)))

; ***********************************************************

; ***********************************************************

(: data-frame-remove (DataFrame LabelProjection -> DataFrame))
(define (data-frame-remove data-frame drop-projection)
  (define all-labels (data-frame-all-labels-projection-set data-frame))
  (define drop-labels (projection-set drop-projection))
  (define keep-labels (set-subtract all-labels drop-labels))
  (new-data-frame (data-frame-explode data-frame #:project keep-labels)))

; ***********************************************************

; ***********************************************************

(: data-frame-project (DataFrame LabelProjection -> DataFrame))
(define (data-frame-project data-frame projection)
  (new-data-frame (data-frame-explode data-frame #:project (projection-set projection))))

; ***********************************************************

; ***********************************************************

(: data-frame-replace (DataFrame Column -> DataFrame))
(define (data-frame-replace data-frame new-col)
  (define name (column-heading new-col))
  (new-data-frame (for/list ([col (data-frame-explode data-frame)])
		       (if (eq? name (column-heading col))
			   new-col
			   col))))

; ***********************************************************

; ***********************************************************
(: data-frame-extend (DataFrame (U Column Columns DataFrame) -> DataFrame))
(define (data-frame-extend data-frame cols)
  (cond 
   ((DataFrame? cols)
    (new-data-frame (append (data-frame-explode data-frame) (data-frame-explode cols))))
   ((list? cols)
    (new-data-frame (append (data-frame-explode data-frame) cols)))
   (else
    (new-data-frame (append (data-frame-explode data-frame) (list cols))))))

; ***********************************************************