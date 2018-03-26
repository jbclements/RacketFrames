;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: numeric-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
(require typed/rackunit)
(require math/statistics)

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both integer (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The integer series is optimized for working with
; integers.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [nseries-ref (NSeries Index -> Float)]
 [nseries-label-ref (NSeries Label -> Float)]
 [nseries-length (NSeries -> Index)] 
 [map/ns (NSeries (Float -> Float) -> NSeries)]
 [bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries)]
 [+/ns (NSeries NSeries -> NSeries)]
 [-/ns (NSeries NSeries -> NSeries)]
 [*/ns (NSeries NSeries -> NSeries)]
 [//ns (NSeries NSeries -> NSeries)]
 [+./ns (NSeries Float -> NSeries)]
 [-./ns (NSeries Float -> NSeries)]
 [*./ns (NSeries Float -> NSeries)]
 [/./ns (NSeries Float -> NSeries)]
 [apply-agg (Symbol NSeries -> Real)]
 [apply-stat (Symbol NSeries -> Real)])

(provide
 flvector-print
 (struct-out NSeries)
 new-NSeries)

; ***********************************************************

; ***********************************************************

(require
 racket/unsafe/ops
 racket/flonum
 (only-in "settings.rkt"
	  Settings-decimals
	  Settings-max-output
	  settings)
 (only-in "indexed-series.rkt"
	  label-index label->idx
	  build-index-from-labels
	  Label SIndex
	  GSeries GSeries-data
	  LabelIndex LabelIndex-index))

; ***********************************************************

; ***********************************************************
(: flvector-print (FlVector Output-Port -> Void))
(define (flvector-print flv port)
  (let ((len (flvector-length flv))
	(out (current-output-port))
	(decs (Settings-decimals (settings)))
	(max-output (Settings-max-output (settings))))
    (if (zero? len)
	(displayln "[ ]" port)
	(begin
	  (display "[ " port)
	  (do ((i 0 (add1 i)))
	      ((>= i len) (void))
	    (let ((num (flvector-ref flv i)))
	      (if (eqv? num +nan.0)
		  (display num port)
		  (display (real->decimal-string num decs) port)))
	    (display " " port))))
    (display "]" port)))

; ***********************************************************

; ***********************************************************
(: writer-NSeries (NSeries Output-Port Boolean -> Void))
(define (writer-NSeries series port mode)
  (let* ([data (NSeries-data series)]
	 [len (flvector-length data)])
    (displayln (format "(NSeries #:length ~s)" len) port)))

; ***********************************************************

; ***********************************************************

;; An NSeries is an optimized Series for computation over vectors of Float
;; i.e., NSeries should be faster then (Series Float)
(struct: NSeries LabelIndex ([data : FlVector])
	 ;;  #:methods gen:custom-write [(define write-proc writer-NSeries)]
	 )

(: new-NSeries (FlVector (Option (U (Listof Label) SIndex)) -> NSeries))
(define (new-NSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (flvector-length data) (hash-count index))
	    (let ((k (current-continuation-marks)))
	      (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(NSeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (NSeries index data))
	  (NSeries #f data))))

; ***********************************************************

; ***********************************************************

; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: nseries-referencer (NSeries -> (Index -> Float)))
(define (nseries-referencer series)
  (let ((data (NSeries-data series)))
    (λ: ((idx : Index))
	(flvector-ref data idx))))

(: nseries-ref (NSeries Index -> Float))
(define (nseries-ref series idx)
  (flvector-ref (NSeries-data series) idx))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: nseries-iref (NSeries Index -> Float))
(define (nseries-iref series idx)
  (flvector-ref (NSeries-data series) idx))

(: nseries-label-ref (NSeries Label -> Float))
(define (nseries-label-ref series label)
  (nseries-ref series (label->idx series label)))

(: nseries-length (NSeries -> Index))
(define (nseries-length nseries)
  (flvector-length (NSeries-data nseries)))

(: flvector->list (FlVector Fixnum -> (Listof Float)))
(define (flvector->list flvec idx)
  (cond
    [(= idx (flvector-length flvec)) null]
    [else (cons (flvector-ref flvec idx) (flvector->list flvec (unsafe-fx+ idx 1)))]))

; ***********************************************************

; ***********************************************************
;; Map a function

(: map/ns (NSeries (Float -> Float) -> NSeries))
(define (map/ns series fn)
  (let ((old-data (NSeries-data series))
	(new-data (make-flvector (flvector-length (NSeries-data series)))))
    (let ((len (flvector-length old-data)))
      (let loop ((idx 0))
	(if (< idx len)
	    (begin
	      (flvector-set! new-data idx (fn (flvector-ref old-data idx)))
	      (loop (add1 idx)))
	    (void))))
    (NSeries (LabelIndex-index series) new-data)))

; ***********************************************************

; ***********************************************************

;; Binary NSeries Ops

(: bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries))
(define (bop/ns ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (NSeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error '+/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector(make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx)
				 (flvector-ref v2 idx)))))

(: +/ns (NSeries NSeries -> NSeries))
(define (+/ns ns1 ns2)
  (bop/ns ns1 ns2 fl+))

(: -/ns (NSeries NSeries -> NSeries))
(define (-/ns ns1 ns2)
  (bop/ns ns1 ns2 fl-))

(: */ns (NSeries NSeries -> NSeries))
(define (*/ns ns1 ns2)
  (bop/ns ns1 ns2 fl*))

(: //ns (NSeries NSeries -> NSeries))
(define (//ns ns1 ns2)
  (bop/ns ns1 ns2 fl/))

; ***********************************************************

; ***********************************************************
;; Scalar NSeries Ops

(: bop./ns (Float NSeries (Float Float -> Float) -> NSeries))
(define (bop./ns fl ns bop)
  (define v1 (NSeries-data ns))
  (define: len : Index (flvector-length v1))
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx) fl))))

(: +./ns (NSeries Float -> NSeries))
(define (+./ns ns fl)
  (bop./ns fl ns fl+))

(: -./ns (NSeries Float -> NSeries))
(define (-./ns ns fl )
  (bop./ns fl ns fl-))

(: *./ns (NSeries Float -> NSeries))
(define (*./ns ns fl)
  (bop./ns fl ns fl*))

(: /./ns (NSeries Float -> NSeries))
(define (/./ns ns fl)
  (bop./ns fl ns fl/))

; ***********************************************************

; ***********************************************************
;; NSeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg (Symbol NSeries -> Real))
(define (apply-agg function-name series)
  (cond 
    [(eq? function-name 'sum) (apply + (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'mean) (mean (flvector->list (NSeries-data series) 0))]
    ;[(eq? function-name 'median) (median (flvector->list (ISeries-data series)))]
    [(eq? function-name 'count) (nseries-length series)]
    ;[(eq? function-name 'min) (flvector-argmin (lambda (x) x) (ISeries-data series))]
    ;[(eq? function-name 'max) (flvector-argmax (lambda (x) x) (ISeries-data series))]
    [else (error 'apply-agg "Unknown aggregate function.")]))

; ***********************************************************

; ***********************************************************
;; NSeries stat ops

(: apply-stat (Symbol NSeries -> Real))
(define (apply-stat function-name series)
  (cond 
    [(eq? function-name 'variance) (variance (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'stddev) (stddev (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'skewness) (skewness (flvector->list (NSeries-data series) 0))]
    [else (error 'apply-stat "Unknown stat function.")]))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************
; float series tests

; create float series
(define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; iseries reference tests
(check-equal? ((nseries-referencer series-float) 0) 1.5)

(check-equal? ((nseries-referencer series-float) 1) 2.4)

(check-equal? (nseries-iref series-float 0) 1.5)

(check-equal? (nseries-iref series-float 1) 2.4)

(check-equal? (nseries-label-ref series-float 'd) 4.1)

(check-equal? (nseries-label-ref series-float 'c) 3.6)

; series length
(check-equal? (nseries-length series-float) 4)

; binop 2 series tests
(check-equal? (NSeries-data (+/ns series-float series-float-2))
              (flvector 6.5 8.4 10.6 12.1))

(check-equal? (NSeries-data (-/ns series-float series-float-2))
              (flvector -3.5 -3.6 -3.4 -3.9000000000000004))

(check-equal? (NSeries-data (*/ns series-float series-float-2))
              (flvector 7.5 14.399999999999999 25.2 32.8))

; currently doing only integer division
(check-equal? (NSeries-data (//ns series-float series-float-2))
              (flvector 0.3 0.39999999999999997 0.5142857142857143 0.5125))

; binop scalar series tests
(check-equal? (NSeries-data (+./ns series-float 2.0))
              (flvector 3.5 4.4 5.6 6.1))

(check-equal? (NSeries-data (-./ns series-float 1.0))
              (flvector 0.5 1.4 2.6 3.0999999999999996))

(check-equal? (NSeries-data (*./ns series-float 2.0))
              (flvector 3.0 4.8 7.2 8.2))

(check-equal? (NSeries-data (/./ns series-float 2.0))
              (flvector 0.75 1.2 1.8 2.05))

; map tests
(check-equal? (NSeries-data (map/ns series-float (λ: ((x : Float)) (fl+ x 1.0))))
              (flvector 2.5 3.4 4.6 5.1))

; agg tests
(check-equal? (apply-agg 'sum series-float) 11.6)

(check-equal? (apply-agg 'mean series-float) 2.9)

(check-equal? (apply-agg 'count series-float) 4)

; statistics tests
(check-equal? (apply-stat 'variance series-float) 1.035)

(check-equal? (apply-stat 'stddev series-float) 1.0173494974687902)

(check-equal? (apply-stat 'skewness series-float) -0.18946647505895)

