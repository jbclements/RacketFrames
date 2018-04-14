;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-join.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
(require typed/rackunit)

; ***********************************************************
; data-frame-join rough draft
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.
(provide:
 [data-frame-join-left (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame)])

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in grip/data/symbol
	  symbol-prefix)
 (only-in "indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "series.rkt"
	  series-complete)
 (only-in "series-description.rkt"
	  SeriesType Series
	  SeriesDescription-type
	  series-type series-length
          series-data)
 (only-in "data-frame.rkt"
	  DataFrame new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode
	  DataFrameDescription DataFrameDescription-series data-frame-description)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref nseries-label-ref new-NSeries)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "categorical-series-ops.rkt"
	  cseries-append)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "data-frame-print.rkt"
          frame-write-tab))

; ***********************************************************

; ***********************************************************

(define-type Column (Pair Label Series))
(define-type Key String)
(define-type JoinHash (HashTable Key (Listof Index)))
(define-type IndexableSeries (U CSeries ISeries))

(define key-delimiter "\t")

; ***********************************************************

; ***********************************************************

; This function consumes a Column and returns the series of
; the Column which is just the second element of the list.
(: column-series (Column -> Series))
(define (column-series scol)
  (cdr scol))

; This function consumes a Column, Setof Label and String
; and checks if the column name of Column is a member of the
; given Setof Label, and if it is, it prepends the prefix
; to the column name and returns that new value. Used for
; join column names.
(: join-column-name (Column (Setof Label) String -> Symbol))
(define (join-column-name column common-cols prefix)
  (let ((colname (car column)))
    (if (set-member? common-cols colname)
	(symbol-prefix colname prefix)
	colname)))

; ***********************************************************

; ***********************************************************

; This function consumes a DataFrameDescription and an Index
; and returns new default series builders of the the given
; length. There will be as many series as there are in the
; DataFrameDecsription.
(: dest-mapping-series-builders (DataFrameDescription Index -> (Listof SeriesBuilder)))
(define (dest-mapping-series-builders data-frame-description len)
  (for/list: : (Listof SeriesBuilder)
	     ([series (DataFrameDescription-series data-frame-description)])
	     (case (SeriesDescription-type series)
	       ((CategoricalSeries) (new-CSeriesBuilder len))
	       ((NumericSeries)     (new-NSeriesBuilder len))
	       ((IntegerSeries)     (new-ISeriesBuilder len))
	       (else (error 'dest-mapping-series-builders
			    "Unknown series type ~a."
			    (SeriesDescription-type series))))))

; ***********************************************************

; ***********************************************************

; This function consumes a Listof Columns and alphabetically
; sorts it on the column name and returns new sorted list.
(: key-cols-sort-lexical ((Listof Column) -> (Listof Column)))
(define (key-cols-sort-lexical cols)
  ((inst sort Column Column)
   cols
   (λ: ((kc1 : Column) (kc2 : Column))
       (string<=? (symbol->string (car kc1))
		  (symbol->string (car kc2))))))

; This function consumes a Listof Column and filteres it for
; only columns of CSeries or ISeries and returns those series
; in list form.
(: key-cols-series ((Listof Column) -> (Listof IndexableSeries)))
(define (key-cols-series cols)
  (filter (λ: ((s : Series)) (or (CSeries? s)
				 (ISeries? s)))
	  (map column-series cols)))

; This function consumes a Listof IndexableSeries and builds key
; string from the columns of a frame and a given set of col labels to use.
; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...
(: key-fn ((Listof IndexableSeries) -> (Index -> Key)))
(define (key-fn cols)
  (let: ((col-refs : (Listof (Index -> (U Label Integer)))
		   (for/list ([col (in-list cols)])
			     (if (CSeries? col)
				 (cseries-referencer col)
				 (iseries-referencer col)))))
	(λ: ((row-id : Index))
	    (let ((outp (open-output-string)))
	      (for ([col-ref (in-list col-refs)])
		   (let*: ((seg : (U Symbol Integer) (col-ref row-id))
			   (seg-str : String (if (symbol? seg)
						 (symbol->string seg)
						 (number->string seg))))
			  (display seg-str outp)
			  (display key-delimiter outp)))
	      (get-output-string outp)))))

; ***********************************************************

; ***********************************************************

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will representa JoinHash.
(: make-index (-> JoinHash))
(define (make-index)
  (make-hash))

; This function consumes a Listof IndexableSeries and creates
; a JoinHash
(: index ((Listof IndexableSeries) -> JoinHash))
(define (index cols)

  (define: index : JoinHash (make-index))

  ; Get length of one of the IndexablSeries
  (define len (series-length (car cols)))
  (define: series-key : (Index -> String) (key-fn cols))

  (let loop ([i 0])
    (if (unsafe-fx>= i len)
	index
	(let: ((i : Index (assert i index?)))
	      (let ((key (series-key i)))
		(hash-update! index key
			      (λ: ((idx : (Listof Index)))
				  (cons i idx))
			      (λ () (list))))
	      (loop (add1 i))))))

; ***********************************************************

; ***********************************************************

; This function consumes a CSeries and a CSeriesBuilder and
; returns a function which consumes an index which indexes into
; the CSeries and retrieves the item to append onto the
; CSeriesBuilder.
(: cseries-copy-fn (CSeries CSeriesBuilder -> (Index -> Void)))
(define (cseries-copy-fn series builder)
  (let ((cseries-ref (cseries-referencer series)))
    (λ: ((i : Index))
	(append-CSeriesBuilder builder (cseries-ref i)))))

; This function is self explanatory, returns a formated error
; on a copy column row error.
(: copy-column-row-error (Series Integer -> Void))
(define (copy-column-row-error series col)
  (error 'data-frame-join-left "Invalid target builder for data-frame column series ~s at ~s"
	 (series-type series) col))

; This functions consumes a Vectorof Series and Vectorof SeriesBuilder
; and an Index and does not return any value. It copies an entire row
; from the given Vectorof Series into the given Vectorof SeriesBuilders.
(: copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void))
(define (copy-column-row src-series dest-builders row-id)
;;  (when (zero? (modulo row-id 10000))
;;	(displayln (format "Copy row: ~a" row-id)))
  (for ([col (in-range (vector-length src-series))])
    ; Loop through each column and get the associated series and series builder.
       (let ((series (vector-ref src-series col))
	     (builder (vector-ref dest-builders col)))
         ; Copy specific row values into correct series builders. If series is
         ; a NSeries then associated value will be appended onto NSeriesBuilder,
         ; and same goes for ISeries and CSeries.
         (cond
	  ((NSeries? series)
	   (let: ((num : Float (nseries-iref series row-id)))
		 (if (NSeriesBuilder? builder)
		     (append-NSeriesBuilder builder num)
		     (copy-column-row-error series col))))
	  ((CSeries? series)
	   (let: ((nom : Label (cseries-iref series row-id)))
		 (if (CSeriesBuilder? builder)
		     (append-CSeriesBuilder builder nom)
		     (copy-column-row-error series col))))
	  ((ISeries? series)
	   (let: ((num : Fixnum (iseries-iref series row-id)))
		 (if (ISeriesBuilder? builder)
		     (append-ISeriesBuilder builder num)
		     (copy-column-row-error series col))))))))

; This functions consumes a Vectorof Series and Vectorof SeriesBuilder
; and an Index and does not return any value. It copies an entire row
; from the given Vectorof Series into the given Vectorof SeriesBuilders.
(: copy-null-to-row ((Vectorof Series) (Vectorof SeriesBuilder) -> Void))
(define (copy-null-to-row src-series dest-builders)
;;  (when (zero? (modulo row-id 10000))
;;	(displayln (format "Copy row: ~a" row-id)))
  (for ([col (in-range (vector-length src-series))])
    ; Loop through each column and get the associated series and series builder.
       (let ((series (vector-ref src-series col))
	     (builder (vector-ref dest-builders col)))
         ; Copy specific row values into correct series builders. If series is
         ; a NSeries then associated value will be appended onto NSeriesBuilder,
         ; and same goes for ISeries and CSeries.
         (cond
	  ((NSeries? series)
           (if (NSeriesBuilder? builder)
               (append-NSeriesBuilder builder -1.0)
               (copy-column-row-error series col)))
	  ((CSeries? series)
           (if (CSeriesBuilder? builder)
               (append-CSeriesBuilder builder 'null)
               (copy-column-row-error series col)))
	  ((ISeries? series)
           (if (ISeriesBuilder? builder)
               (append-ISeriesBuilder builder 1)
               (copy-column-row-error series col)))))))

; ***********************************************************

; ***********************************************************

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-join-build-left/right ((Vectorof Series) (Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) JoinHash -> Void))
(define (do-join-build-left/right a-cols b-cols b-cols-match a-builders b-builders dfa-key-fn join-hash)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: dfa-len   : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))

  (for ((dfa-row (in-range dfa-len)))
       (let*: ((dfa-row : Index (assert dfa-row index?))
	       (dfa-key : Key (dfa-key-fn dfa-row)))
	      (let ((dfb-rows (hash-ref join-hash dfa-key (λ () '()))))
                (displayln (format "Hash join: ~s ~s, ~s" dfa-row dfa-key dfb-rows))
                (if (null? dfb-rows)
                    (begin (copy-column-row a-cols a-builders dfa-row)
                    ; Copy nans into fb
                    (copy-null-to-row b-cols b-builders))
                    ;(copy-null-to-row b-cols-match b-builders))
                    (for ([dfb-row dfb-rows])
                      ; maps possible multiple rows from b to row in a
                      (copy-column-row a-cols a-builders dfa-row)
                      (copy-column-row b-cols b-builders (assert dfb-row index?))))))))

; ***********************************************************

; ***********************************************************

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-join-build-inner ((Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) JoinHash -> Void))
(define (do-join-build-inner a-cols b-cols a-builders b-builders dfa-key-fn join-hash)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: dfa-len   : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))

  (for ((dfa-row (in-range dfa-len)))
       (let*: ((dfa-row : Index (assert dfa-row index?))
	       (dfa-key : Key (dfa-key-fn dfa-row)))
	      (let ((dfb-rows (hash-ref join-hash dfa-key (λ () '()))))
                (displayln (format "Hash join: ~s ~s, ~s" dfa-row dfa-key dfb-rows))                
                ;(copy-null-to-row b-cols-match b-builders))
                (for ([dfb-row dfb-rows])
                  ; maps possible multiple rows from b to row in a
                  (copy-column-row a-cols a-builders dfa-row)
                  (copy-column-row b-cols b-builders (assert dfb-row index?)))))))

; ***********************************************************

; ***********************************************************

; This function consumes two Vectorof Series and two Vectorof
; SeriesBuilder. The types of Series and SeriesBuilder must
; match in the respective indicies.
(: do-join-build-outer ((Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) (Index -> Key) JoinHash JoinHash -> Void))
(define (do-join-build-outer a-cols b-cols a-builders b-builders dfa-key-fn dfb-key-fn join-hash-a join-hash-b)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: dfa-len   : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))
  (define: dfb-len   : Fixnum (series-length (vector-ref b-cols #{0 : Index} )))

  (for ((dfa-row (in-range dfa-len)))
       (let*: ((dfa-row : Index (assert dfa-row index?))
	       (dfa-key : Key (dfa-key-fn dfa-row)))
	      (let ((dfb-rows (hash-ref join-hash-b dfa-key (λ () '()))))
                ;(displayln (format "Hash join: ~s ~s, ~s" dfa-row dfa-key dfb-rows))
                (if (null? dfb-rows)                    
                    (begin
                      ; copy a value but null for b
                      (copy-column-row a-cols a-builders dfa-row)
                      (copy-null-to-row b-cols b-builders))
                    (for ([dfb-row dfb-rows])
                      ; maps possible multiple rows from b to row in a
                      (copy-column-row a-cols a-builders dfa-row)
                      (copy-column-row b-cols b-builders (assert dfb-row index?)))))))

  ; do vice versa for b
  (for ((dfb-row (in-range dfb-len)))
       (let*: ((dfb-row : Index (assert dfb-row index?))
	       (dfb-key : Key (dfb-key-fn dfb-row)))
	      (let ((dfa-rows (hash-ref join-hash-a dfb-key (λ () '()))))
                ;(displayln (format "Hash join: ~s ~s, ~s" dfa-row dfa-key dfb-rows))
                (if (null? dfa-rows)                    
                    (begin
                      ; copy a value but null for b
                      (copy-column-row b-cols b-builders dfb-row)
                      (copy-null-to-row a-cols a-builders))
                    (for ([dfa-row dfa-rows])
                      ; maps possible multiple rows from b to row in a
                      (copy-column-row a-cols a-builders (assert dfa-row index?))
                      (copy-column-row b-cols b-builders dfb-row)))))))

; ***********************************************************

; ***********************************************************

; pass in the matched columns as well for display purposes

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; This function does a left join on dfa to dfb.
; Currently this function only supports joining on one column.
(: data-frame-join-left (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-left dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> (Listof Column)))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series ((Listof Column) -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  ;(when (null? join-cols)
  ;(error "No common columns between data-frames to join on."))

  ; The column of fb that are not in the join set.
  (define: non-key-dfb : (Setof Label) (set-subtract cols-b join-cols))

  ; get all dfa-cols regardless of join intersection
  (define: dfa-cols : (Listof Column) (data-frame-cols dfa '()))
  ; only get dfb-cols not in join intersection
  (define: dfb-cols : (Listof Column) (data-frame-cols dfb non-key-dfb))
  ; only get dfb-cols which match for display purposes
  (define: dfb-cols-match : (Listof Column) (data-frame-cols dfb (set-intersect cols-a cols-b)))

  ; Create index on fb dataframe on join-cols.
  (define: dfb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfb join-cols))))
      (index (key-cols-series cols))))

  (define: dfa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfa join-cols)))))

  ; Get series builders of default length 10 for all columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfa) 10)))

  ; Get series builders of default length 10 for only non-key-fb columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfb) 10)))
  
  (do-join-build-left/right (src-series dfa-cols) (src-series dfb-cols) (src-series dfb-cols-match)
		 dest-builders-a dest-builders-b
		 dfa-keyfn dfb-index)

  (define: new-a-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

;; right outer join, just reverse fa and fb operations

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; Currently this function only supports joining on one column.
(: data-frame-join-right (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-right dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> (Listof Column)))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series ((Listof Column) -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-b cols-a)
					 (set-intersect (list->set cols)
							(set-intersect cols-b cols-a))))

  ;(when (null? join-cols)
  ;(error "No common columns between data-frames to join on."))

  ; The column of fa that are not in the join set.
  (define: non-key-dfa : (Setof Label) (set-subtract cols-a join-cols))

  ; get all fb-cols regardless of join intersection
  (define: dfb-cols : (Listof Column) (data-frame-cols dfb '()))
  ; only get fa-cols not in join intersection
  (define: dfa-cols : (Listof Column) (data-frame-cols dfa non-key-dfa))

  ; only get dfb-cols which match for display purposes
  (define: dfa-cols-match : (Listof Column) (data-frame-cols dfa (set-intersect cols-a cols-b)))

  ; Create index on dfa dataframe on join-cols.
  (define: dfa-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfa join-cols))))
      (index (key-cols-series cols))))

  (define: dfb-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfb join-cols)))))

  ; Get series builders of default length 10 for all columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfb) 10)))

  ; Get series builders of default length 10 for only non-key-fb columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfa #:project non-key-dfa) 10)))

  (do-join-build-left/right (src-series dfb-cols) (src-series dfa-cols) (src-series dfa-cols-match)
		 dest-builders-b dest-builders-a
		 dfb-keyfn dfa-index)

  (define: new-a-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; This function does a left join on dfa to dfb.
; Currently this function only supports joining on one column.
(: data-frame-join-inner (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-inner dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> (Listof Column)))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series ((Listof Column) -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  (when (null? join-cols)
	(error "No common columns between data-frames to join on."))

  ; The column of fb that are not in the join set.
  (define: non-key-dfb : (Setof Label) (set-subtract cols-b join-cols))

  ; get all dfa-cols regardless of join intersection
  (define: dfa-cols : (Listof Column) (data-frame-cols dfa '()))
  ; only get dfb-cols not in join intersection
  (define: dfb-cols : (Listof Column) (data-frame-cols dfb '()))

  ; Create index on fb dataframe on join-cols.
  (define: dfb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfb join-cols))))
      (index (key-cols-series cols))))

  (define: dfa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfa join-cols)))))

  ; Get series builders of default length 10 for all columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfa) 10)))

  ; Get series builders of default length 10 for only non-key-fb columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfb) 10)))

  (do-join-build-inner (src-series dfa-cols) (src-series dfb-cols)
		 dest-builders-a dest-builders-b
		 dfa-keyfn dfb-index)

  (define: new-a-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

; This function consumes two DataFrames to join and an optional
; on argument which contains a Listof column names to join on.
; This function does a left join on dfa to dfb.
; Currently this function only supports joining on one column.
(: data-frame-join-outer (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-join-outer dfa dfb #:on [cols '()])

  ; This function consumes a DataFrame and LabelProjection and
  ; projects those columns.
  (: data-frame-cols (DataFrame LabelProjection -> (Listof Column)))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  ; This function consumes a Listof Column and returns a Vectorof
  ; Series contained in those columns.
  (: src-series ((Listof Column) -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  ; Get the common cols between fa and fb
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  (when (null? join-cols)
	(error "No common columns between data-frames to join on."))

  ; The column of fb that are not in the join set.
  (define: non-key-dfb : (Setof Label) (set-subtract cols-b join-cols))

  ; get all dfa-cols regardless of join intersection
  (define: dfa-cols : (Listof Column) (data-frame-cols dfa '()))
  ; only get dfb-cols not in join intersection
  (define: dfb-cols : (Listof Column) (data-frame-cols dfb '()))

  ; Create index on fb dataframe on join-cols.
  (define: dfa-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfa '()))))
      (index (key-cols-series cols))))
  
  ; Create index on fb dataframe on join-cols.
  (define: dfb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols dfb '()))))
      (index (key-cols-series cols))))

  (define: dfa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfa '())))))

  (define: dfb-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols dfb '())))))

  ; Get series builders of default length 10 for all columns in fa.
  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description dfa) 10)))

  ; Get series builders of default length 10 for only non-key-fb columns in fb.
  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description dfb) 10)))

  ; (define (do-join-build-outer a-cols b-cols a-builders b-builders dfa-key-fn dfb-key-fn join-hash-a join-hash-b)

  (do-join-build-outer (src-series dfa-cols) (src-series dfb-cols)
		 dest-builders-a dest-builders-b
		 dfa-keyfn dfb-keyfn dfa-index dfb-index)

  (define: new-a-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list dfa-cols)])
	      (cons (join-column-name col cols-a "dfa-")
		    (series-complete builder))))

  (define: new-b-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list dfb-cols)])
	      (cons (join-column-name col cols-b "dfb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********
; Test Cases
; ***********

(define integer-col-1 (cons 'integer-col-1 (new-ISeries (vector 1 2 3 4 5) #f)))

(define integer-col-2 (cons 'integer-col-2 (new-ISeries (vector 6 7 8 9 10) #f)))

(define float-col-1 (cons 'float-col-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5) #f)))

(define categorical-col-1 (cons' categorical-col-1 (new-CSeries (vector 'a 'b 'c 'd 'e))))

; opaque
;(check-equal? (column-series integer-col-1)
;              (new-ISeries (vector 1 2 3 4 5) #f))

(check-equal? (join-column-name integer-col-1 (set 'integer-col-1 'integer-col-2) "prefix")
              'prefixinteger-col-1)

; (: dest-mapping-series-builders (DataFrameDescription Index -> (Listof SeriesBuilder)))

(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))))

(define columns-categorical
  (list 
   (cons 'col1 (new-CSeries (vector 'a 'b 'c 'd 'e)))
   (cons 'col2 (new-CSeries (vector 'e 'f 'g 'h 'i)))
   (cons 'col3 (new-CSeries (vector 'j 'k 'l 'm 'n)))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

; create new data-frame-categorical
(define data-frame-categorical (new-data-frame columns-categorical))

; unable to protect opaque value
;(check-equal?
 ;(dest-mapping-series-builders (data-frame-description data-frame-integer) 4)
 ;(list (new-ISeriesBuilder 4) (new-ISeriesBuilder 4) (new-ISeriesBuilder 4)))

; Unable to protect opaque
;(check-equal? (key-cols-sort-lexical (list integer-col-2 integer-col-1))
              ;(list (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
                    ;(cons 'col3 (new-ISeries (vector 9 10 11 12) #f))))

; Unable to protect opaque
;(check-equal? (key-cols-series (list integer-col-2 integer-col-1 float-col-1))
              ;(list (column-series integer-col-2) (column-series integer-col-1)))

(check-equal?
 ((key-fn (key-cols-series (list integer-col-2 integer-col-1 float-col-1 categorical-col-1))) 2)
 "8\t3\tc\t")

; build hash join
;(check-equal?
; (index (key-cols-series (list integer-col-2 integer-col-1 float-col-1 categorical-col-1)))
; (hash "6\t1\ta\t" (list 0)
;       "7\t2\tb\t" (list 1)
;       "8\t3\tc\t" (list 2)
;       "10\t5\t3\t" (list 4)
;       "9\t4\td\t" (list 3)))

(index (key-cols-series (list integer-col-2 integer-col-1 float-col-1 categorical-col-1)))

(define cseries-1 (new-CSeries (vector 'a 'b 'c 'd 'e)))

(define iseries-1 (new-ISeries (vector 1 2 3 4 5) #f))

(define nseries-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5) #f))

(define cseries-2 (new-CSeries (vector 'a 'b 'c 'd 'l)))

(define iseries-2 (new-ISeries (vector 1 2 3 4 5) #f))

(define nseries-2 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5) #f))

(define cseries-builder-1 (new-CSeriesBuilder))

(define cseries-copy-fn-1 (cseries-copy-fn cseries-1 cseries-builder-1))

(cseries-copy-fn-1 1)

(define cseries-builder-1-complete (complete-CSeriesBuilder cseries-builder-1))

(check-equal? (cseries-iref cseries-builder-1-complete 0) 'b)

;(copy-column-row-error cseries-1 3)

(define cseries-builder-2 (new-CSeriesBuilder))

(define iseries-builder-1 (new-ISeriesBuilder))

(define nseries-builder-1 (new-NSeriesBuilder))

;(: copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void))

(copy-column-row (vector cseries-1 iseries-1 nseries-1)
                 (vector cseries-builder-2 iseries-builder-1 nseries-builder-1)
                 2)

(check-equal? (cseries-iref (complete-CSeriesBuilder cseries-builder-2) 0) 'c)

(check-equal? (iseries-iref (complete-ISeriesBuilder iseries-builder-1) 0) 3)

(check-equal? (nseries-iref (complete-NSeriesBuilder nseries-builder-1) 0) 3.5)

; (: do-join-build-left/right ((Vectorof Series) (Vectorof Series)
;		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
;		  (Index -> Key) JoinHash -> Void))

(define fa-key-fn (key-fn (list cseries-1 iseries-1 cseries-2 iseries-2)))

(fa-key-fn 1)

(define cseries-builder-a (new-CSeriesBuilder))
(define iseries-builder-a (new-ISeriesBuilder))

(define cseries-builder-b (new-CSeriesBuilder))
(define iseries-builder-b (new-ISeriesBuilder))


;(series-data (complete-CSeriesBuilder cseries-builder-a))

;(series-data (complete-CSeriesBuilder cseries-builder-b))

;(frame-write-tab (data-frame-join-left data-frame-integer data-frame-categorical) (current-output-port))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-3
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 101 201 301 401) #f))))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

; create new data-frame-integer-3
(define data-frame-integer-3 (new-data-frame columns-integer-3))

(frame-write-tab (data-frame-join-left data-frame-integer-2 data-frame-integer-3 #:on (list 'col1)) (current-output-port))

(frame-write-tab (data-frame-join-left data-frame-integer-2 data-frame-integer-3 #:on (list 'col2)) (current-output-port))

(frame-write-tab (data-frame-join-right data-frame-integer-2 data-frame-integer-3 #:on (list 'col1)) (current-output-port))

(frame-write-tab (data-frame-join-right data-frame-integer-2 data-frame-integer-3 #:on (list 'col2)) (current-output-port))

(define columns-integer-4
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-5
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 101 201 301 401) #f))))

; create new data-frame-integer-4
(define data-frame-integer-4 (new-data-frame columns-integer-4))

; create new data-frame-integer-5
(define data-frame-integer-5 (new-data-frame columns-integer-5))

(frame-write-tab (data-frame-join-left data-frame-integer-4 data-frame-integer-5 #:on (list 'col1 'co3)) (current-output-port))

(frame-write-tab (data-frame-join-inner data-frame-integer-2 data-frame-integer-3 #:on (list 'col1)) (current-output-port))

(frame-write-tab (data-frame-join-inner data-frame-integer-4 data-frame-integer-5 #:on (list 'col2)) (current-output-port))

(frame-write-tab (data-frame-join-outer data-frame-integer-4 data-frame-integer-5 #:on (list 'col3)) (current-output-port))
