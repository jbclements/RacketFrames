;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: integer-series.rkt
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
(provide
 (struct-out ISeries))

(provide:
 [new-ISeries ((Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> ISeries)]
 [iseries-iref (ISeries Index -> Fixnum)]
 [iseries-length (ISeries -> Index)]
 [iseries-referencer (ISeries -> (Index -> Integer))]
 [iseries-data (ISeries -> (Vectorof Fixnum))]
 [map/is (ISeries (Fixnum -> Fixnum) -> ISeries)]
 [bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)]
 [+/is (ISeries ISeries -> ISeries)]
 [-/is (ISeries ISeries -> ISeries)]
 [*/is (ISeries ISeries -> ISeries)]
 [//is (ISeries ISeries -> ISeries)]
 [%/is (ISeries ISeries -> ISeries)]
 [r/is (ISeries ISeries -> ISeries)]
 [+./is (ISeries Fixnum -> ISeries)]
 [-./is (ISeries Fixnum -> ISeries)]
 [*./is (ISeries Fixnum -> ISeries)]
 [/./is (ISeries Fixnum -> ISeries)]
 [%./is (ISeries Fixnum -> ISeries)]
 [r./is (ISeries Fixnum -> ISeries)]
 [apply-agg (Symbol ISeries -> Real)]
 [apply-stat (Symbol ISeries -> Real)])
; ***********************************************************

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex
          label-index label->idx))
; ***********************************************************

; ***********************************************************
; racket/fixnum library provides operations like fx+ that
; consumes and produce only fixnums. The operations in this
; library are meant to be safe versions of unsafe operations
; like unsafe-fx+. These safe operations are generally no
; faster than using generic primitives like +. But they are
; slower than the unsafe versions, with the benefit of being
; safer. This library will be using unsafe operations for
; speed improvement.

;; Integer series optimized with use of Fixnum.
(struct ISeries LabelIndex ([data : (Vectorof Fixnum)]))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a ISeries
; struct object.
(: new-ISeries ((Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> ISeries))
(define (new-ISeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(ISeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (ISeries index data))
	  (ISeries #f data))))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: iseries-referencer (ISeries -> (Index -> Integer)))
(define (iseries-referencer cseries)
  (let ((data (ISeries-data cseries)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: iseries-iref (ISeries Index -> Fixnum))
(define (iseries-iref series idx)
  (vector-ref (ISeries-data series) idx))

; This function consumes an integer series and returns its
; data vector.
(: iseries-data (ISeries -> (Vectorof Fixnum)))
(define (iseries-data series)
  (ISeries-data series))

; This function consumes a series and a Label and returns
; the value at that Label in the series.
(: iseries-label-ref (ISeries Label -> Integer))
(define (iseries-label-ref series label)
  (iseries-iref series (label->idx series label)))

; This function consumes an integer series and returns the
; length of that series.
(: iseries-length (ISeries -> Index))
(define (iseries-length series)
  (vector-length (ISeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/is (ISeries (Fixnum -> Fixnum) -> ISeries))
(define (map/is series fn)
  (let ((old-data (ISeries-data series)))
    (ISeries #f (build-vector (vector-length old-data)
                              (λ: ((idx : Natural))
                                (fn (vector-ref old-data idx)))))))
; ***********************************************************

; ***********************************************************
;; Binary ISeries bops

; This function consumes 2 integer series and a function which
; consumes 2 Fixnum's and produces a Fixnum result. This function
; is applied to each value in the 2 series at the same index
; resulting in a new data point and at the end of the loop a new
; data vector. This data vector is the data of the new ISeries
; which is returned.
(: bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries))
(define (bop/is ns1 ns2 bop)
  (define v1 (ISeries-data ns1))
  (define v2 (ISeries-data ns2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error '+/is "Series must be of equal length."))
  
  (define: v-bop : (Vectorof Fixnum) (make-vector len #{0 : Fixnum}))

  ; Do loop returns ISeries, idx to 0 and increments by 1 Fixnum on
  ; each iteration (this is the step-exprs). When the loop has gone
  ; through the whole vector, the resulting new ISeries is returned
  ; which the v-bop as the data.
  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx #{1 : Fixnum})])
       ((= idx len) (ISeries #f v-bop))
       (vector-set! v-bop idx (bop (vector-ref v1 idx)
				   (vector-ref v2 idx)))))
; ***********************************************************

; ***********************************************************
; Functions provided by racket/unsafe/ops are unsafe. They
; have certain constraints, but the constraints are not
; checked, which allows the system to generate and execute
; faster code. If arguments violate an unsafe function’s
; constraints, the function’s behavior and result is
; unpredictable, and the entire system can crash or become
; corrupted.
; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction, multiplication
; and division using unsafe-fx and the bop/is function defined
; above.

(: +/is (ISeries ISeries -> ISeries))
(define (+/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx+))

(: -/is (ISeries ISeries -> ISeries))
(define (-/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx-))

(: */is (ISeries ISeries -> ISeries))
(define (*/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fx*))

(: //is (ISeries ISeries -> ISeries))
(define (//is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxquotient))

(: r/is (ISeries ISeries -> ISeries))
(define (r/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxremainder))

(: %/is (ISeries ISeries -> ISeries))
(define (%/is ns1 ns2)
  (bop/is ns1 ns2 unsafe-fxmodulo))

; ***********************************************************

; ***********************************************************
;; Scalar ISeries bops

; This function consumes a Fixnum and an integer series and
; a binary operation function which
; consumes 2 Fixnum's and produces a Fixnum result. This function
; is applied to each value in the 2 series at the same index
; resulting in a new data point and at the end of the loop a new
; data vector. This data vector is the data of the new ISeries
; which is returned.

(: bop./is (Fixnum ISeries (Fixnum Fixnum -> Fixnum) -> ISeries))
(define (bop./is fx is bop)
  (define: v1 : (Vectorof Fixnum) (ISeries-data is))
  (define: len : Index (vector-length v1))
  (define: v-bop : (Vectorof Fixnum) ((inst make-vector Fixnum) len #{0 : Fixnum}))

  (do: : ISeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
       ((= idx len) (ISeries #f v-bop))
       (vector-set! v-bop idx (bop #{(vector-ref v1 idx) : Fixnum} fx))))

; ***********************************************************

; ***********************************************************
; These functions apply addition, subtraction, multiplication
; and division using unsafe-fx and the bop./is function defined
; above.

(: +./is (ISeries Fixnum -> ISeries))
(define (+./is is fx)
  (bop./is fx is unsafe-fx+))

(: -./is (ISeries Fixnum -> ISeries))
(define (-./is is fx)
  (bop./is fx is unsafe-fx-))

(: *./is (ISeries Fixnum -> ISeries))
(define (*./is is fx)
  (bop./is fx is unsafe-fx*))

(: /./is (ISeries Fixnum -> ISeries))
(define (/./is is fx)
  (bop./is fx is unsafe-fxquotient))

(: r./is (ISeries Fixnum -> ISeries))
(define (r./is is fx)
  (bop./is fx is unsafe-fxremainder))

(: %./is (ISeries Fixnum -> ISeries))
(define (%./is is fx)
  (bop./is fx is unsafe-fxmodulo))

; ***********************************************************

; ***********************************************************
;; ISeries agg ops

; Applies the aggregate function specificed by function-name to the values in
; the column-name column. Currently supports 3: sum, avg, count.
(: apply-agg (Symbol ISeries -> Real))
(define (apply-agg function-name series)
  (cond 
    [(eq? function-name 'sum) (apply + (vector->list (ISeries-data series)))]
    [(eq? function-name 'mean) (mean (vector->list (ISeries-data series)))]
    ;[(eq? function-name 'median) (median (vector->list (ISeries-data series)))]
    [(eq? function-name 'count) (iseries-length series)]
    ;[(eq? function-name 'min) (vector-argmin (lambda (x) x) (ISeries-data series))]
    ;[(eq? function-name 'max) (vector-argmax (lambda (x) x) (ISeries-data series))]
    [else (error 'apply-agg "Unknown aggregate function.")]))

; ***********************************************************

; ***********************************************************
;; ISeries stat ops

(: apply-stat (Symbol ISeries -> Real))
(define (apply-stat function-name series)
  (cond 
    [(eq? function-name 'variance) (variance (vector->list (ISeries-data series)))]
    [(eq? function-name 'stddev) (stddev (vector->list (ISeries-data series)))]
    [(eq? function-name 'skewness) (skewness (vector->list (ISeries-data series)))]
    [else (error 'apply-stat "Unknown stat function.")]))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************

; integer series tests

; create integer series
(define series-integer (new-ISeries (vector 1 2 3 4)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-integer-2 (new-ISeries (vector 5 6 7 8)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; iseries reference tests
(check-equal? ((iseries-referencer series-integer) 0) 1)

(check-equal? ((iseries-referencer series-integer) 1) 2)

(check-equal? (iseries-iref series-integer 0) 1)

(check-equal? (iseries-iref series-integer 1) 2)

(check-equal? (iseries-label-ref series-integer 'd) 4)

(check-equal? (iseries-label-ref series-integer 'c) 3)

; series length
(check-equal? (iseries-length series-integer) 4)

; binop 2 series tests
(check-equal? (ISeries-data (+/is series-integer series-integer-2))
              (vector 6 8 10 12))

(check-equal? (ISeries-data (-/is series-integer series-integer-2))
              (vector -4 -4 -4 -4))

(check-equal? (ISeries-data (*/is series-integer series-integer-2))
              (vector 5 12 21 32))

; currently doing only integer division
(check-equal? (ISeries-data (//is series-integer series-integer-2))
              (vector 0 0 0 0))

(check-equal? (ISeries-data (r/is series-integer series-integer-2))
              (vector 1 2 3 4))

(check-equal? (ISeries-data (%/is series-integer series-integer-2))
              (vector 1 2 3 4))

; binop scalar series tests
(check-equal? (ISeries-data (+./is series-integer 2))
              (vector 3 4 5 6))

(check-equal? (ISeries-data (-./is series-integer 1))
              (vector 0 1 2 3))

(check-equal? (ISeries-data (*./is series-integer 2))
              (vector 2 4 6 8))

(check-equal? (ISeries-data (/./is series-integer 2))
              (vector 0 1 1 2))

(check-equal? (ISeries-data (r./is series-integer 2))
              (vector 1 0 1 0))

(check-equal? (ISeries-data (%./is series-integer 2))
              (vector 1 0 1 0))

; map tests
(check-equal? (ISeries-data (map/is series-integer (λ: ((x : Fixnum)) (unsafe-fx+ x 1))))
              (vector 2 3 4 5))

; agg tests
(check-equal? (apply-agg 'sum series-integer) 10)

(check-equal? (apply-agg 'mean series-integer) 10/4)

(check-equal? (apply-agg 'count series-integer) 4)

; statistics tests
(check-equal? (apply-stat 'variance series-integer) 5/4)

(check-equal? (apply-stat 'stddev series-integer) 1.118033988749895)

(check-equal? (apply-stat 'skewness series-integer) 0.0)
