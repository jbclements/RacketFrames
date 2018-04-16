;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
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
 data-frame-cseries data-frame-nseries data-frame-iseries
 new-data-frame)

; ***********************************************************

; ***********************************************************

(require
 racket/flonum
 racket/set
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
	  series-description series-length series-type series-data
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
; Data Structure Definitions

(define-type Column  (Pair Label Series))
(define-type Columns (Listof Column))

;; A DataFrame is map of series.
(struct: DataFrame LabelIndex ([series : (Vectorof Series)]))

(struct: DataFrameDescription ([dimensions : Dim]
                               [series : (Listof SeriesDescription)]))

; ***********************************************************

; ***********************************************************

; This function consumes a Label and Series and defines a
; Column;
(: column (Label Series -> Column))
(define (column label series)
  (cons label series))

; This function consumes a Column and just retrieves the
; first element of pair which is the label heading.
(: column-heading (Column -> Label))
(define (column-heading col)
  (car col))

; This function consumes a Column and just retrieves the
; second element of pair which is the actual series.
(: column-series (Column -> Series))
(define (column-series col)
  (cdr col))

; ***********************************************************

; ***********************************************************

; This function consumes a Listof Column and constructs a
; DataFrame from it. The function checks that all columns
; are of the same length and builds a LabelIndex and a
; Vectorof Series.
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
               (error 'new-data-frame "Frame must have equal length series: ~a" 
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

; This function consumes a DataFrame and two labels. It locates
; the index of the column in the DataFrame LabelIndex and creates
; a new LabelIndex with the new column name mapped to this index.
; Then a new DataFrame is created and returned.
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

; This function consumes a DataFrame and a Label for the column
; to drop from the DataFrame. Using the data-frame-explode
; function, this column is filtered out of the DataFrame, and a
; new DataFrame is returned.
(: data-frame-drop (DataFrame Label -> DataFrame))
(define (data-frame-drop data-frame label)
  (new-data-frame (filter (λ: ((col : Column))
			 (not (eq? (car col) label)))
		     (data-frame-explode data-frame))))

; ***********************************************************

; ***********************************************************
; This function consumes a DataFrame and a Symbol representing
; the column name and returns the Series of that column.
(: data-frame-series (DataFrame Symbol -> Series))
(define (data-frame-series data-frame col)
  (vector-ref (DataFrame-series data-frame)
              (label-index (assert (LabelIndex-index data-frame)) col)))

; This function uses the above function and the assert function
; to ensure the series returned is a CSeries.
(: data-frame-cseries (DataFrame Symbol -> CSeries))
(define (data-frame-cseries data-frame name)
  (assert (data-frame-series data-frame name) CSeries?))

; This function uses the above function and the assert function
; to ensure the series returned is a NSeries.
(: data-frame-nseries (DataFrame Symbol -> NSeries))
(define (data-frame-nseries data-frame name)
  (assert (data-frame-series data-frame name) NSeries?))

; This function uses the above function and the assert function
; to ensure the series returned is an ISeries.
(: data-frame-iseries (DataFrame Symbol -> ISeries))
(define (data-frame-iseries data-frame name)
  (assert (data-frame-series data-frame name) ISeries?))

; This function consumes a DataFrame and returns a Listof pairs
; consisting of the column name and its associated index in the
; DataFrame.
(: data-frame-labels (DataFrame -> (Listof (Pair Symbol Index))))
(define (data-frame-labels data-frame)
  (hash->list (assert (LabelIndex-index data-frame))))

; ***********************************************************

; ***********************************************************

; (inst) example usage
; (map (inst cons Symbol Integer) '(a b c d) '(1 2 3 4))
; - : (Listof (Pairof Symbol Integer))
; '((a . 1) (b . 2) (c . 3) (d . 4))

; This function consumes a DataFrame and returns of Listof
; Symbol representing all the column names.
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

; This function consumes a DataFrame and calculates the dimensions.
; It does so by calculating the length of a column series, thus
; getting the number of rows and the total number of columns. It
; then stores these two values in the Dim struct and returns it.
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
; This function consumes a LabelProjection which may be a
; Listof Label or a Setof Label and ensure that it is a Setof
; Label and returns this set.
(: projection-set (LabelProjection -> (Setof Label)))
(define (projection-set project)
  (if (list? project) 
      (list->set project) 
      project))

; This function consumes a DataFrame and using the project-set
; function above returns a sorted set of all the column names
; in the DataFrame.
(: data-frame-all-labels-projection-set (DataFrame -> (Setof Label)))
(define (data-frame-all-labels-projection-set data-frame)
  (projection-set (map (inst car Symbol Any) (label-sort-positional data-frame))))

; This function consumes a List of type A and a function which
; consumes an element of type A and returns a Symbol as well as
; a LabelProjection which is a Listof Label or Setof Label and
; returns a Listof A which is filtered based on the labels present
; in the given LabelProjection. Thus only the items that are desired
; to be projected are projected.
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

; This function consumes a DataFrame and an optional LabelProjection
; adn constructs a DataFrameDescription struct which contains the
; DataFrame Dim and a list of SeriesDecriptions for each series of
; each column.
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

; This function consumes a DataFrameDescription struct and displays
; the DataFrame Dim and Series Descriptions in formated form.
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

; This function consumes a DataFrame and an optional LabelProjection
; which indicates which columns to project and returns a Listof Column
; If no LabelProjection is given, all columns are projected. The
; label-sort-positional returns a labeling, which is a (Listof (Pair Label Index)).
; This list is looped through and only the items to project are filtered for.
; In the end an appropriate Listof Column is returned.
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

; This function consumes a DataFrame and LabelProjection indicating
; which columns to remove from the DataFrame. The column names in
; the DataFrame are put into a set called all-labels. The drop-projection
; is converted to a set as well. The labels to be kept are calculated
; by substracting the drop-projection from all of the possible labels.
; Then the DataFrame is exploded retaining only the columns to keep.
; A new data frame is constructed fomr these column and returned.
(: data-frame-remove (DataFrame LabelProjection -> DataFrame))
(define (data-frame-remove data-frame drop-projection)
  (define all-labels (data-frame-all-labels-projection-set data-frame))
  (define drop-labels (projection-set drop-projection))
  (define keep-labels (set-subtract all-labels drop-labels))
  (new-data-frame (data-frame-explode data-frame #:project keep-labels)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and LabelProjection and
; creates a new DataFrame only containing the columns to project.
(: data-frame-project (DataFrame LabelProjection -> DataFrame))
(define (data-frame-project data-frame projection)
  (new-data-frame (data-frame-explode data-frame #:project (projection-set projection))))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrame and Column and replaces
; the old column with the same column heading with this new
; column.
(: data-frame-replace (DataFrame Column -> DataFrame))
(define (data-frame-replace data-frame new-col)
  (define name (column-heading new-col))

  (new-data-frame (for/list ([col (data-frame-explode data-frame)])
		       (if (eq? name (column-heading col))
			   new-col
			   col))))

; ***********************************************************

; ***********************************************************

; This functions consumes a DataFrame and either a Column,
; Listof Column or a DataFrame adn extends the given DataFrame.
; If a DataFrame is passed for the second argument it is first
; exploded and appended on to the exploded given data frame. Then
; a new DataFrame is constructed and returned. The other cases
; are self-explanatory.
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

; ***********************************************************
; Test Cases
; ***********************************************************
; data frame tests

;******************
;data-frame-integer
;******************
; will define parse to automatically build this columns structure
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (build-index-from-labels (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (build-index-from-labels (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (build-index-from-labels (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

;******************
;data-frame-float
;******************
; will define parse to automatically build this columns structure
(define columns-float
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5)
                            (build-index-from-labels (list 'a 'b 'c 'd))))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5)
                            (build-index-from-labels (list 'e 'f 'g 'h))))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5)
                            (build-index-from-labels (list 'i 'j 'k 'l))))))

; create new data-frame-float
(define data-frame-float (new-data-frame columns-float))

;******************
;data-frame-categorical
;******************
; will define parse to automatically build this columns structure
(define columns-categorical
  (list 
   (cons 'col1 (new-CSeries (vector 'hello 'world)))
   (cons 'col2 (new-CSeries (vector 'fizz 'buzz)))
   (cons 'col3 (new-CSeries (vector 'foo 'bar)))))

; create new data-frame-categorical
(define data-frame-categorical (new-data-frame columns-categorical))

;******************
;data-frame-mix
;******************
; will define parse to automatically build this columns structure
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (vector 1 2 3 4)
                            (build-index-from-labels (list 'a 'b 'c 'd))))
   (cons 'float-col (new-NSeries (flvector 1.5 2.5 3.5 4.5)
                            (build-index-from-labels (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

; ************************
; data-frame-integer tests
; ************************

(check-equal? (series-data (data-frame-series data-frame-integer 'col1))
              (vector 1 2 3 4))

(check-equal? (series-data (data-frame-series data-frame-integer 'col2))
              (vector 5 6 7 8))

(set! data-frame-integer (data-frame-rename data-frame-integer 'col1 'col-one))

(check-equal? (data-frame-names data-frame-integer) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series data-frame-integer 'col-one))
              (vector 1 2 3 4))

(check-equal? (data-frame-labels data-frame-integer)
              (list (cons 'col2 1) (cons 'col3 2) (cons 'col-one 0)))

(check-equal? (projection-set (list 'col-one 'col2 'col3))
              (set 'col-one 'col2 'col3))

(check-equal? (data-frame-all-labels-projection-set data-frame-integer)
              (set 'col-one 'col2 'col3))

; check error
;(data-frame-series data-frame-integer 'col1)

(set! data-frame-integer (data-frame-drop data-frame-integer 'col-one))

; check col-one is gone

; ************************
; data-frame-float tests
; ************************

(check-equal? (series-data (data-frame-series data-frame-float 'col1))
              (flvector 1.5 2.5 3.5 4.5))

(check-equal? (series-data (data-frame-series data-frame-float 'col2))
              (flvector 5.5 6.5 7.5 8.5))

(set! data-frame-float (data-frame-rename data-frame-float 'col1 'col-one))

(check-equal? (data-frame-names data-frame-float) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series data-frame-float 'col-one))
              (flvector 1.5 2.5 3.5 4.5))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-float (data-frame-drop data-frame-float 'col-one))

; check col-one is gone

; ************************
; data-frame-categorical tests
; ************************

(check-equal? (series-data (data-frame-series data-frame-categorical 'col1))
              (vector 'hello 'world))

(check-equal? (series-data (data-frame-series data-frame-categorical 'col2))
              (vector 'fizz 'buzz))

(set! data-frame-categorical (data-frame-rename data-frame-categorical 'col1 'col-one))

(check-equal? (data-frame-names data-frame-categorical) (list 'col-one 'col2 'col3))

(check-equal? (series-data (data-frame-series data-frame-categorical 'col-one))
              (vector 'hello 'world))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-categorical (data-frame-drop data-frame-categorical 'col-one))

; check col-one is gone
(check-equal? (data-frame-names data-frame-categorical) (list 'col2 'col3))

; ************************
; data-frame-mix tests
; ************************

(check-equal? (series-data (data-frame-series data-frame-mix 'integer-col))
              (vector 1 2 3 4))

(check-equal? (series-data (data-frame-series data-frame-mix 'float-col))
              (flvector 1.5 2.5 3.5 4.5))

(check-equal? (series-data (data-frame-series data-frame-mix 'categorical-col))
              (vector 'hello 'world 'fizz 'buzz))

(set! data-frame-mix (data-frame-rename data-frame-mix 'float-col 'float-column))

(check-equal? (data-frame-names data-frame-mix) (list 'integer-col 'float-column 'categorical-col))

(check-equal? (series-data (data-frame-series data-frame-mix 'float-column))
              (flvector 1.5 2.5 3.5 4.5))

; check error
;(data-frame-series data-frame-float 'col1)

(set! data-frame-mix (data-frame-drop data-frame-mix 'float-column))

; check float-column is gone

(check-equal? (data-frame-names data-frame-mix) (list 'integer-col 'categorical-col))

; data-frame-explode tests
(data-frame-explode data-frame-integer)

(data-frame-description data-frame-integer)

(show-data-frame-description (data-frame-description data-frame-integer))
