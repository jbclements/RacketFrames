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
 [data-frame-append (DataFrame DataFrame [#:col (Listof Symbol)] -> DataFrame)]
 [data-frame-merge (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame)])

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

(: column-series (Column -> Series))
(define (column-series scol)
  (cdr scol))

(: join-column-name (Column (Setof Label) String -> Symbol))
(define (join-column-name column common-cols prefix)
  (let ((colname (car column)))
    (if (set-member? common-cols colname)
	(symbol-prefix colname prefix)
	colname)))

; ***********************************************************

; ***********************************************************

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

(: key-cols-sort-lexical ((Listof Column) -> (Listof Column)))
(define (key-cols-sort-lexical cols)
  ((inst sort Column Column)
   cols
   (λ: ((kc1 : Column) (kc2 : Column))
       (string<=? (symbol->string (car kc1))
		  (symbol->string (car kc2))))))

(: key-cols-series ((Listof Column) -> (Listof IndexableSeries)))
(define (key-cols-series cols)
  (filter (λ: ((s : Series)) (or (CSeries? s)
				 (ISeries? s)))
	  (map column-series cols)))

;; Build key string from the columns of a frame and a given set of col labels to use.
;; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...

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

(: make-index (-> JoinHash))
(define (make-index)
  (make-hash))

(: index ((Listof IndexableSeries) -> JoinHash))
(define (index cols)

  (define: index : JoinHash (make-index))

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

(: cseries-copy-fn (CSeries CSeriesBuilder -> (Index -> Void)))
(define (cseries-copy-fn series builder)
  (let ((cseries-ref (cseries-referencer series)))
    (λ: ((i : Index))
	(append-CSeriesBuilder builder (cseries-ref i)))))

(: copy-column-row-error (Series Integer -> Void))
(define (copy-column-row-error series col)
  (error 'data-frame-merge "Invalid target builder for data-frame column series ~s at ~s"
	 (series-type series) col))

(: copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void))
(define (copy-column-row src-series dest-builders row-id)
;;  (when (zero? (modulo row-id 10000))
;;	(displayln (format "Copy row: ~a" row-id)))
  (for ([col (in-range (vector-length src-series))])
       (let ((series (vector-ref src-series col))
	     (builder (vector-ref dest-builders col)))
	 (cond
	  ((NSeries? series)
	   (let: ((num : Float (nseries-iref series row-id)))
		 (if (NSeriesBuilder? builder)
		     (append-NSeriesBuilder builder num)
		     (copy-column-row-error series col))))
	  ((CSeries? series)
	   (let: ((nom : Label (cseries-iref series row-id)))
		 (if (CSeriesBuilder? builder)
		     (append-CSeriesBuilder builder  nom)
		     (copy-column-row-error series col))))
	  ((ISeries? series)
	   (let: ((num : Fixnum (iseries-iref series row-id)))
		 (if (ISeriesBuilder? builder)
		     (append-ISeriesBuilder builder num)
		     (copy-column-row-error series col))))))))

; ***********************************************************

; ***********************************************************

(: do-join-build ((Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) JoinHash -> Void))
(define (do-join-build a-cols b-cols a-builders b-builders fa-key-fn join-hash)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: fa-len    : Fixnum (series-length (vector-ref a-cols #{0 : Index} )))

  (for ((fa-row (in-range fa-len)))
       (let*: ((fa-row : Index (assert fa-row index?))
	       (fa-key : Key (fa-key-fn fa-row)))
	      (let ((fb-rows (hash-ref join-hash fa-key (λ () '()))))
                (displayln (format "Hash join: ~s ~s, ~s" fa-row fa-key fb-rows))
		(for ([fb-row fb-rows])
		     (copy-column-row a-cols a-builders fa-row)
		     (copy-column-row b-cols b-builders (assert fb-row index?)))))))

; ***********************************************************

; ***********************************************************

;; FIXME RPR - Currently only doing a left-outer join on fa to fb
;; Smart pick which to index, which to drive from sequentially, between DataFrame fa and DataFrame fb.
(: data-frame-merge (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(define (data-frame-merge fa fb #:on [cols '()])

  ;;  Directly using frame-explode with an internal define doesn't work.  TR BUG
  (: data-frame-cols (DataFrame LabelProjection -> (Listof Column)))
  (define (data-frame-cols data-frame project)
    (data-frame-explode data-frame #:project project))

  (: src-series ((Listof Column) -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (data-frame-names fa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names fb)))
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  (define: non-key-common : (Setof Label) (set-subtract (set-intersect cols-a cols-b) join-cols))

  (when (null? join-cols)
	(error "No common columns between data-frames to join on."))

  (define: non-key-fb : (Setof Label) (set-subtract cols-b join-cols))

  (define: fa-cols : (Listof Column) (data-frame-cols fa '()))
  (define: fb-cols : (Listof Column) (data-frame-cols fb non-key-fb))

  (define: fb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (data-frame-cols fb join-cols))))
      (index (key-cols-series cols))))

  (define: fa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (data-frame-cols fa join-cols)))))

  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (data-frame-description fa) 10)))

  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (data-frame-description fb #:project non-key-fb) 10)))

  ;; side-effects into the builders
  (do-join-build (src-series fa-cols) (src-series fb-cols)
		 dest-builders-a dest-builders-b
		 fa-keyfn fb-index)

  (define: new-a-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list fa-cols)])
	      (cons (join-column-name col non-key-common "fa-")
		    (series-complete builder))))

  (define: new-b-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list fb-cols)])
	      (cons (join-column-name col non-key-common "fb-")
		    (series-complete builder))))

  (new-data-frame (append new-a-series new-b-series)))

; ***********************************************************

; ***********************************************************

;; Append common columns
(: data-frame-append (DataFrame DataFrame [#:col (Listof Symbol)] -> DataFrame))
(define (data-frame-append fa fb #:col [cols '()])

  (define: cols-a    : (Setof Label) (list->set (data-frame-names fa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names fb)))
  (define: append-cols : (Setof Label) (if (null? cols)
					   (set-intersect cols-a cols-b)
					   (set-intersect (list->set cols)
							  (set-intersect cols-a cols-b))))
  (new-data-frame (map (λ: ((name : Label))
		      (cons name (cseries-append (data-frame-cseries fa name)
						 (data-frame-cseries fb name))))
		  (filter (λ: ((name : Label))
			      (set-member? append-cols name))
			  (data-frame-names fa)))))

; ***********************************************************

; ***********
; Test Cases
; ***********

(define integer-col-1 (cons 'integer-col-1 (new-ISeries (vector 1 2 3 4 5) #f)))

(define integer-col-2 (cons 'integer-col-2 (new-ISeries (vector 6 7 8 9 10) #f)))

(define float-col-1 (cons 'float-col-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5) #f)))

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

(dest-mapping-series-builders (data-frame-description data-frame-integer) 4)

(key-cols-sort-lexical (list integer-col-2 integer-col-1))

(key-cols-series (list integer-col-2 integer-col-1 float-col-1))

((key-fn (key-cols-series (list integer-col-2 integer-col-1 float-col-1))) 2)

; build hash join
(index (key-cols-series (list integer-col-2 integer-col-1 float-col-1)))

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

; (: do-join-build ((Vectorof Series) (Vectorof Series)
;		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
;		  (Index -> Key) JoinHash -> Void))

(define fa-key-fn (key-fn (list cseries-1 iseries-1 cseries-2 iseries-2)))

(define cseries-builder-a (new-CSeriesBuilder))
(define iseries-builder-a (new-ISeriesBuilder))

(define cseries-builder-b (new-CSeriesBuilder))
(define iseries-builder-b (new-ISeriesBuilder))

(do-join-build
 (vector cseries-1 iseries-1)
 (vector cseries-2 iseries-2)
 (vector cseries-builder-a iseries-builder-a)
 (vector cseries-builder-b iseries-builder-b)
 fa-key-fn
(index (list cseries-1 iseries-1 cseries-2 iseries-2)))

(series-data (complete-CSeriesBuilder cseries-builder-a))

(frame-write-tab (data-frame-merge data-frame-integer data-frame-categorical) (current-output-port))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))))

(define columns-integer-3
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

; create new data-frame-integer-3
(define data-frame-integer-3 (new-data-frame columns-integer-3))

(frame-write-tab (data-frame-merge data-frame-integer-2 data-frame-integer-3 (list 'col1)) (current-output-port))
