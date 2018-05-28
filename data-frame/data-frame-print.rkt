;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: data-frame-print.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)
(require racket/struct)
(require racket/pretty)

; ***********************************************************
; This module provides data frame printing functionality.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [frame-write-tab (DataFrame Output-Port [#:heading Boolean] -> Void)]
 [data-frame-head (case-> (DataFrame -> Void)
		     (DataFrame (Option Index) -> Void))])

; ***********************************************************

; ***********************************************************

(require
 racket/match
 (only-in grip/data/format
	  ~a ~r)
 (only-in "types.rkt"
	  Dim Dim-rows)
 (only-in "indexed-series.rkt"
	  Label)
 (only-in "series-description.rkt"
	  Series series-type)
 (only-in "data-frame.rkt"
	  DataFrame new-data-frame data-frame-names
	  Column Columns column-heading column-series
	  data-frame-cseries data-frame-explode data-frame-dim
	  DataFrameDescription DataFrameDescription-series
	  show-data-frame-description data-frame-description)
(only-in "generic-series.rkt"
	  new-GenSeries GenSeries GenSeries? gen-series-iref)
 (only-in "integer-series.rkt"
	  new-ISeries ISeries ISeries? iseries-iref)
 (only-in "boolean-series.rkt"
	  new-BSeries BSeries BSeries? bseries-iref)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries?))

; ***********************************************************

; ***********************************************************

(define WIDTH 15)

(define-type DataFrameRowFormatter (DataFrame Natural -> String))

; ***********************************************************

; ***********************************************************

(: display-heading (Columns -> Void))
(define (display-heading cols)

  (: format-heading (Symbol -> String))
  (define (format-heading heading)
    (~a (symbol->string heading)
	#:width WIDTH
	#:align 'center))

  (for ([col cols])
       (let ((heading (column-heading col))
	     (series  (column-series col)))
	 (cond
           ((GenSeries? series)
            (display (format-heading heading)))
           ((NSeries? series)
            (display (format-heading heading)))
           ((CSeries? series)
            (display (format-heading heading)))
           ((ISeries? series)
            (display (format-heading heading)))
           ((BSeries? series)
            (display (format-heading heading)))
	  (else
	   (error 'data-frame-head "Heading for unknown series types ~s"
		  (series-type series)))))
       (display " "))

  (newline))

; ***********************************************************

; ***********************************************************

(: format-gen-series (GenSeries Index -> String))
(define (format-gen-series gen-series row)
  (let ((data (gen-series-iref gen-series (list row))))
    (cond
      [(symbol? data)
          (~a (symbol->string data)
              #:width WIDTH
              #:align 'left)]
      [(integer? data)
          (~r data
              #:precision 0
              #:min-width WIDTH)]
      [(rational? data)
       (~r data
           #:precision '(= 4)
           #:min-width WIDTH)]
      [(boolean? data)
       (~a (if data
               "#t"
               "#f")
           #:width WIDTH
           #:align 'left)]
      ; pretty-format struct
      [else (pretty-format data)])))

(: format-cseries (CSeries Index -> String))
(define (format-cseries cseries row)
  (~a (symbol->string (car (cseries-iref cseries (list row))))
      #:width WIDTH
      #:align 'left))

(: format-nseries (NSeries Index -> String))
(define (format-nseries nseries row)
  (let ((n (nseries-iref nseries (list row))))
    (if (rational? n)
	(~r n
	    #:precision '(= 4)
	    #:min-width WIDTH)
	"+nan.0")))

(: format-iseries (ISeries Index -> String))
(define (format-iseries iseries row)
  (~r (car (iseries-iref iseries (list row)))
      #:precision 0
      #:min-width WIDTH))

(: format-bseries (BSeries Index -> String))
(define (format-bseries bseries row)
  (~a (if (bseries-iref bseries (list row))
          "#t"
          "#f")
      #:width WIDTH
      #:align 'left))

; ***********************************************************

; ***********************************************************

(: display-data-frame-row ((Vectorof Series) (Sequenceof Index) -> Void))
(define (display-data-frame-row series rows)
  ;;  (define: cols : (Sequenceof  (in-range (vector-length series)))
  (for: ([row : Index rows])
	(for ([col (in-range (vector-length series))])
	     (let ((a-series (vector-ref series col)))
	       (cond
                 ((GenSeries? a-series)
                  (display (format-gen-series a-series row)))
                 ((NSeries? a-series)
                  (display (format-nseries a-series row)))
                 ((CSeries? a-series)
                  (display (format-cseries a-series row)))
                 ((ISeries? a-series)
                  (display (format-iseries a-series row)))
                 ((BSeries? a-series)
                  (display (format-bseries a-series row)))
                 (else
                  (error 'data-frame-head "Unknown series types ~s"
                         (series-type a-series)))))
          (display " "))
        (newline)))

(define default-head-rows 10)

(: data-frame-head (case-> (DataFrame -> Void)
		      (DataFrame (Option Index) -> Void)))
(define (data-frame-head data-frame [count #f])
  (define: cols     : Columns (data-frame-explode data-frame))
  (define: headings : (Listof Label) (map column-heading cols))
  (define: series   : (Vectorof Series) (list->vector (map column-series cols)))

  ;; (show-frame-description (frame-description frame))

  (display-heading cols)
  (let ((count (min (Dim-rows (data-frame-dim data-frame))
                    (if (not count) default-head-rows count))))
    (display-data-frame-row series (in-range count))))

(: frame-write-tab (DataFrame Output-Port [#:heading Boolean] -> Void))
(define (frame-write-tab data-frame outp #:heading [heading #t])

  (define: cols     : Columns (data-frame-explode data-frame))
  (define: headings : (Listof Label) (map column-heading cols))
  (define: series   : (Vectorof Series) (list->vector (map column-series cols)))
  (define: row-num  : Index (Dim-rows (data-frame-dim data-frame)))
  (define: col-num  : Index (vector-length series))

  (: write-frame-row (Index -> Void))
  (define (write-frame-row row)
    (for ([col (in-range col-num)])
	 (unless (zero? col)
		 (display "\t" outp))
	 (let ((a-series (vector-ref series col)))
	   (cond
             ((GenSeries? a-series)
              (display (gen-series-iref a-series (list row)) outp))
             ((NSeries? a-series)
              (let ((n (nseries-iref a-series (list row))))
                (display n outp)))
             ((CSeries? a-series)
              (display (cseries-iref a-series (list row)) outp))
             ((ISeries? a-series)
              (display (iseries-iref a-series (list row)) outp))
             ((BSeries? a-series)
              (display (bseries-iref a-series (list row)) outp))
             (else
              (error 'frame-head "Unknown series types ~s"
                     (series-type a-series)))))))

  (: write-heading (Columns -> Void))
  (define (write-heading cols)
    (match cols
	   ((list-rest col1 cols)
	    (display (car col1) outp)
	    (for ([col cols])
		 (display "\t" outp)
		 (display (car col) outp))
	    (newline outp))
	   (else (void))))

  (when heading
	(write-heading cols))

  (for ([row (in-range row-num)])
       (write-frame-row (assert row index?))
       (newline outp)))

; Test Cases

;******************
;data-frame-integer
;******************
; will define parse to automatically build this columns structure
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (list 'a 'b 'c 'd)))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (list 'e 'f 'g 'h)))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (list 'i 'j 'k 'l)))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(frame-write-tab data-frame-integer (current-output-port))
