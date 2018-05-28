;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: numeric-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
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
 [set-NSeries-index (NSeries (U (Listof Label) SIndex) -> NSeries)]
 [nseries-iref (NSeries (Listof Index) -> (Listof Float))]
 [nseries-label-ref (NSeries Label -> (Listof Float))]
 [nseries-range (NSeries Index -> FlVector)]
 [nseries-referencer (NSeries -> (Index -> Float))]
 [nseries-length (NSeries -> Index)]
 [nseries-data (NSeries -> FlVector)]
 [map/ns (NSeries (Float -> Float) -> NSeries)]
 [bop/ns (NSeries NSeries (Float Float -> Float) -> NSeries)]
 [+/ns (NSeries NSeries -> NSeries)]
 [-/ns (NSeries NSeries -> NSeries)]
 [*/ns (NSeries NSeries -> NSeries)]
 [//ns (NSeries NSeries -> NSeries)]
 [>/ns (NSeries NSeries -> BSeries)]
 [</ns (NSeries NSeries -> BSeries)]
 [>=/ns (NSeries NSeries -> BSeries)]
 [<=/ns (NSeries NSeries -> BSeries)]
 [=/ns (NSeries NSeries -> BSeries)]
 [!=/ns (NSeries NSeries -> BSeries)]
 [+./ns (NSeries Float -> NSeries)]
 [-./ns (NSeries Float -> NSeries)]
 [*./ns (NSeries Float -> NSeries)]
 [/./ns (NSeries Float -> NSeries)]
 [+/ns/is (NSeries ISeries -> NSeries)]
 [-/ns/is (NSeries ISeries -> NSeries)]
 [*/ns/is (NSeries ISeries -> NSeries)]
 [//ns/is (NSeries ISeries -> NSeries)]
 [+/is/ns (ISeries NSeries -> NSeries)]
 [-/is/ns (ISeries NSeries -> NSeries)]
 [*/is/ns (ISeries NSeries -> NSeries)]
 [//is/ns (ISeries NSeries -> NSeries)]
 [>/ns/is (NSeries ISeries -> BSeries)]
 [</ns/is (NSeries ISeries -> BSeries)]
 [>=/ns/is (NSeries ISeries -> BSeries)]
 [<=/ns/is (NSeries ISeries -> BSeries)]
 [=/ns/is (NSeries ISeries -> BSeries)]
 [!=/ns/is (NSeries ISeries -> BSeries)]
 [>/is/ns (ISeries NSeries -> BSeries)]
 [</is/ns (ISeries NSeries -> BSeries)]
 [>=/is/ns (ISeries NSeries -> BSeries)]
 [<=/is/ns (ISeries NSeries -> BSeries)]
 [=/is/ns (ISeries NSeries -> BSeries)]
 [!=/is/ns (ISeries NSeries -> BSeries)]
 [apply-agg-ns (Symbol NSeries -> Real)]
 [apply-stat-ns (Symbol NSeries -> Real)]
 [flvector->list (FlVector Fixnum -> (Listof Float))]
 [list->flvector ((Listof Float) -> FlVector)])

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
	  label-index label->lst-idx
	  build-index-from-labels
	  Label SIndex
	  LabelIndex LabelIndex-index)
 (only-in "boolean-series.rkt"
          BSeries BSeries-data)
 (only-in "integer-series.rkt"
          ISeries ISeries-data))

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

(struct: Summary ([mean : Float]
                  [variance : Float]
                  [min : Float]
                  [max : Float]
                  [count : Natural]
                  [nans : Natural]))

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
(: set-NSeries-index (NSeries (U (Listof Label) SIndex) -> NSeries))
(define (set-NSeries-index nseries labels)

  (define data (NSeries-data nseries))
  
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
      (let ((index (build-index-from-labels labels)))
        (check-mismatch index)
        (NSeries index data))))
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

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: nseries-iref (NSeries (Listof Index) -> (Listof Float)))
(define (nseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (flvector-ref (NSeries-data series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
; No flvector-take function available, so for loop was used.
(: nseries-range (NSeries Index -> FlVector))
(define (nseries-range series pos)
  (define flvector-ranged (make-flvector pos))

  (for [(idx (range pos))]
    (flvector-set! flvector-ranged idx (flvector-ref (NSeries-data series) idx)))
  
  flvector-ranged)

(: nseries-label-ref (NSeries Label -> (Listof Float)))
(define (nseries-label-ref series label)
  (nseries-iref series (label->lst-idx series label)))

; This function consumes a numeric series and returns its
; data vector.
(: nseries-data (NSeries -> FlVector))
(define (nseries-data series)
  (NSeries-data series))

(: nseries-length (NSeries -> Index))
(define (nseries-length nseries)
  (flvector-length (NSeries-data nseries)))

(: flvector->list (FlVector Fixnum -> (Listof Float)))
(define (flvector->list flvec idx)
  (cond
    [(= idx (flvector-length flvec)) null]
    [else (cons (flvector-ref flvec idx) (flvector->list flvec (unsafe-fx+ idx 1)))]))

(: list->flvector ((Listof Float) -> FlVector))
(define (list->flvector float-list)
  (define len : Index (length float-list))

  (define result-flvector (make-flvector len))

  (for([flo float-list]
     [i (in-range len)])
    (flvector-set! result-flvector i flo))

  result-flvector)

(check-equal? (list->flvector (list 1.5 2.5 3.5)) (flvector 1.5 2.5 3.5))

; ***********************************************************

; ***********************************************************
;; Map a function

; This function consumes a series and a function both of Float
; types and applies the function to each member of the series
; returning a new series.
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
  
  (define: v-bop : FlVector (make-flvector len))

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

;; Binary NSeries ISeries Ops

(: bop/ns/is (NSeries ISeries (Float Float -> Float) -> NSeries))
(define (bop/ns/is ns1 ns2 bop)
  (define v1 (NSeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'bop/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (flvector-ref v1 idx)
				 (exact->inexact (vector-ref v2 idx))))))

; caller functions
(: +/ns/is (NSeries ISeries -> NSeries))
(define (+/ns/is ns is)
  (bop/ns/is ns is fl+))

(: -/ns/is (NSeries ISeries -> NSeries))
(define (-/ns/is ns is)
  (bop/ns/is ns is fl-))

(: */ns/is (NSeries ISeries -> NSeries))
(define (*/ns/is ns is)
  (bop/ns/is ns is fl*))

(: //ns/is (NSeries ISeries -> NSeries))
(define (//ns/is ns is)
  (bop/ns/is ns is fl/))

; tests
(NSeries-data (+/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(NSeries-data (-/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(NSeries-data (*/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(NSeries-data (//ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

; ***********************************************************

; ***********************************************************

;; Binary ISeries NSeries Ops

(: bop/is/ns (ISeries NSeries (Float Float -> Float) -> NSeries))
(define (bop/is/ns ns1 ns2 bop)
  (define v1 (ISeries-data ns1))
  (define v2 (NSeries-data ns2))

  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'bop/ns "Series must be of equal length."))
  
  (define: v-bop : FlVector (make-flvector len))

  (do: : NSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (NSeries #f v-bop))
       (flvector-set! v-bop idx (bop (exact->inexact (vector-ref v1 idx))
				 (flvector-ref v2 idx)))))

; caller functions
(: +/is/ns (ISeries NSeries -> NSeries))
(define (+/is/ns is ns)
  (bop/is/ns is ns fl+))

(: -/is/ns (ISeries NSeries -> NSeries))
(define (-/is/ns is ns)
  (bop/is/ns is ns fl-))

(: */is/ns (ISeries NSeries -> NSeries))
(define (*/is/ns is ns)
  (bop/is/ns is ns fl*))

(: //is/ns (ISeries NSeries -> NSeries))
(define (//is/ns is ns)
  (bop/is/ns is ns fl/))

; tests
(NSeries-data (+/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(NSeries-data (-/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(NSeries-data (*/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(NSeries-data (//is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

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
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/ns (NSeries NSeries (Float Float -> Boolean) -> BSeries))
(define (comp/ns ns1 ns2 comp)
  (define v1 (NSeries-data ns1))
  (define v2 (NSeries-data ns2))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'comp/ns "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (BSeries #f v-comp))
       (vector-set! v-comp idx (comp (flvector-ref v1 idx)
				   (flvector-ref v2 idx)))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/ns (NSeries NSeries -> BSeries))
(define (>/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl>))

(: </ns (NSeries NSeries -> BSeries))
(define (</ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl<))

(: >=/ns (NSeries NSeries -> BSeries))
(define (>=/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl>=))

(: <=/ns (NSeries NSeries -> BSeries))
(define (<=/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl<=))

(: =/ns (NSeries NSeries -> BSeries))
(define (=/ns ns1 ns2)
  (comp/ns ns1 ns2 unsafe-fl=))

(: !=/ns (NSeries NSeries -> BSeries))
(define (!=/ns ns1 ns2)
  (comp/ns ns1 ns2 (lambda ([a : Float] [b : Float]) (not (unsafe-fl= a b)))))

; ***********************************************************


; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/ns/is (NSeries ISeries (Float Float -> Boolean) -> BSeries))
(define (comp/ns/is ns is comp)
  (define v1 (NSeries-data ns))
  (define v2 (ISeries-data is))
  (define: len : Index (flvector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error 'comp/ns/is "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (BSeries #f v-comp))
       (vector-set! v-comp idx (comp (flvector-ref v1 idx)
				   (exact->inexact (vector-ref v2 idx))))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/ns/is (NSeries ISeries -> BSeries))
(define (>/ns/is ns is)
  (comp/ns/is ns is unsafe-fl>))

(: </ns/is (NSeries ISeries -> BSeries))
(define (</ns/is ns is)
  (comp/ns/is ns is unsafe-fl<))

(: >=/ns/is (NSeries ISeries -> BSeries))
(define (>=/ns/is ns is)
  (comp/ns/is ns is unsafe-fl>=))

(: <=/ns/is (NSeries ISeries -> BSeries))
(define (<=/ns/is ns is)
  (comp/ns/is ns is unsafe-fl<=))

(: =/ns/is (NSeries ISeries -> BSeries))
(define (=/ns/is ns is)
  (comp/ns/is ns is unsafe-fl=))

(: !=/ns/is (NSeries ISeries -> BSeries))
(define (!=/ns/is ns is)
  (comp/ns/is ns is (lambda ([a : Float] [b : Float]) (not (unsafe-fl= a b)))))

(BSeries-data (>/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(BSeries-data (</ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(BSeries-data (>=/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(BSeries-data (<=/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(BSeries-data (=/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

(BSeries-data (=/ns/is (NSeries #f (flvector 2.0 2.0 2.0 2.0 2.0)) (ISeries #f (vector 2 2 2 2 2))))

(BSeries-data (!=/ns/is (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5)) (ISeries #f (vector 2 2 2 2 2))))

; ***********************************************************

; ***********************************************************
;; Binary NSeries comp

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Boolean result for comparison.
; This function is applied to each value in the 2 series at the same
; index resulting in a new boolean point and at the end of the loop
; a new data vector. This data vector is the data of the new ISeries
; which is returned.
(: comp/is/ns (ISeries NSeries (Float Float -> Boolean) -> BSeries))
(define (comp/is/ns is ns comp)
  (define v1 (ISeries-data is))
  (define v2 (NSeries-data ns))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (flvector-length v2))
	  (error 'comp/is/ns "Series must be of equal length."))
  
  (define: v-comp : (Vectorof Boolean) (make-vector len #f))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : BSeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (BSeries #f v-comp))
       (vector-set! v-comp idx (comp (exact->inexact (vector-ref v1 idx))
				   (flvector-ref v2 idx)))))

; ***********************************************************

; ***********************************************************
; These functions apply various comparison operators using
; unsafe-fx and the bop./is function defined above.

(: >/is/ns (ISeries NSeries -> BSeries))
(define (>/is/ns is ns)
  (comp/is/ns is ns unsafe-fl>))

(: </is/ns (ISeries NSeries -> BSeries))
(define (</is/ns is ns)
  (comp/is/ns is ns unsafe-fl<))

(: >=/is/ns (ISeries NSeries -> BSeries))
(define (>=/is/ns is ns)
  (comp/is/ns is ns unsafe-fl>=))

(: <=/is/ns (ISeries NSeries -> BSeries))
(define (<=/is/ns is ns)
  (comp/is/ns is ns unsafe-fl<=))

(: =/is/ns (ISeries NSeries -> BSeries))
(define (=/is/ns is ns)
  (comp/is/ns is ns unsafe-fl=))

(: !=/is/ns (ISeries NSeries -> BSeries))
(define (!=/is/ns is ns)
  (comp/is/ns is ns (lambda ([a : Float] [b : Float]) (not (unsafe-fl= a b)))))

; tests
(BSeries-data (>/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(BSeries-data (</is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(BSeries-data (>=/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(BSeries-data (<=/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(BSeries-data (=/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

(BSeries-data (=/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 2.0 2.0 2.0 2.0 2.0))))

(BSeries-data (!=/is/ns (ISeries #f (vector 2 2 2 2 2)) (NSeries #f (flvector 1.5 2.5 3.5 4.5 5.5))))

; ***********************************************************

; ***********************************************************
;; NSeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg-ns (Symbol NSeries -> Real))
(define (apply-agg-ns function-name series)
  (cond 
    [(eq? function-name 'sum) (apply + (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'mean) (mean (flvector->list (NSeries-data series) 0))]
    ;[(eq? function-name 'median) (median (flvector->list (ISeries-data series)))]
    [(eq? function-name 'count) (nseries-length series)]
    ;[(eq? function-name 'min) (flvector-argmin (lambda (x) x) (ISeries-data series))]
    ;[(eq? function-name 'max) (flvector-argmax (lambda (x) x) (ISeries-data series))]
    [else (error 'apply-agg-ns "Unknown aggregate function.")]))

; ***********************************************************

; ***********************************************************
;; NSeries stat ops

(: apply-stat-ns (Symbol NSeries -> Real))
(define (apply-stat-ns function-name series)
  (cond 
    [(eq? function-name 'variance) (variance (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'stddev) (stddev (flvector->list (NSeries-data series) 0))]
    [(eq? function-name 'skewness) (skewness (flvector->list (NSeries-data series) 0))]
    [else (error 'apply-stat-ns "Unknown stat function.")]))

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

(check-equal? (nseries-iref series-float (list 0)) (list 1.5))

(check-equal? (nseries-iref series-float (list 1)) (list 2.4))

(check-equal? (nseries-label-ref series-float 'd) (list 4.1))

(check-equal? (nseries-label-ref series-float 'c) (list 3.6))

; series length
(check-equal? (nseries-length series-float) 4)

; binop 2 series tests
(check-equal? (NSeries-data (+/ns series-float series-float-2))
              (flvector 6.5 8.4 10.6 12.1))

(check-equal? (NSeries-data (-/ns series-float series-float-2))
              (flvector -3.5 -3.6 -3.4 -3.9000000000000004))

(check-equal? (NSeries-data (*/ns series-float series-float-2))
              (flvector 7.5 14.399999999999999 25.2 32.8))

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
(check-equal? (apply-agg-ns 'sum series-float) 11.6)

(check-equal? (apply-agg-ns 'mean series-float) 2.9)

(check-equal? (apply-agg-ns 'count series-float) 4)

; statistics tests
(check-equal? (apply-stat-ns 'variance series-float) 1.035)

(check-equal? (apply-stat-ns 'stddev series-float) 1.0173494974687902)

(check-equal? (apply-stat-ns 'skewness series-float) -0.18946647505895)
