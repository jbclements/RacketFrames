;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: categorical-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base
(require typed/rackunit)
(require math/statistics)

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both integer (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The categorical series hold String values.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out CSeries)
 new-CSeries)
;;writer-CSeries)

(provide:
 [CSeries->SIndex    (CSeries -> SIndex)]
 [cseries-length      (CSeries -> Index)]
 [cseries-iref        (CSeries Fixnum -> String)]
 [cseries-referencer (CSeries -> (Fixnum -> String))]
 [map/cs (CSeries (String -> String) -> CSeries)]
 [bop/cs (CSeries CSeries (String String -> String) -> CSeries)]
 [bop./cs (String CSeries (String String -> String) -> CSeries)])
; ***********************************************************

; ***********************************************************
(require
  racket/unsafe/ops
  (only-in "indexed-series.rkt"
           SIndex Label
           LabelIndex LabelIndex-index))
; ***********************************************************

; ***********************************************************
;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer
;; See 12.8 Printer Extensions in Racket doc.

(: writer-CSeries (CSeries Output-Port Boolean -> Void))
(define (writer-CSeries series port mode)
  (let* ([data (CSeries-data series)]
	 [nominals (CSeries-nominals series)]
	 [len (vector-length data)])
    (do ([i 0 (add1 i)])
	((>= i len) (void))
      (displayln  (vector-ref nominals (vector-ref data i))))))

; ***********************************************************

; ***********************************************************
(struct CSeries LabelIndex ([data : (Vectorof Index)]
			    [nominals : (Vectorof String)]))

; This function consumes a vector of Strings which represent
; the nominals of the categorical series and creates a new
; categorical series with the correct encoding.
(: new-CSeries ((Vectorof String) -> CSeries))
(define (new-CSeries nominals)

  (: nominal-code (HashTable String Index))
  (define nominal-code (make-hash))

  (define len (vector-length nominals))

  (: data (Vectorof Index))
  (define data (make-vector len 0))

  (: make-nominal-vector (-> (Vectorof String)))
  (define (make-nominal-vector)
    (define nominals (make-vector (hash-count nominal-code) ""))
    (hash-for-each nominal-code
		   (λ: ((nom : String) (idx : Index))
		       (vector-set! nominals idx nom)))
    nominals)

  (let loop : CSeries ((idx : Natural 0) (code : Index 0))
	(if (>= idx len)
	    (CSeries #f data (make-nominal-vector))
	    (let ((nom (vector-ref nominals idx)))
	      (if (hash-has-key? nominal-code nom)
		  (begin
		    (vector-set! data idx (hash-ref nominal-code nom))
		    (loop (add1 idx) code))
		  (begin
		    (hash-set! nominal-code nom code)
		    (vector-set! data idx code)
		    (loop (add1 idx) (assert (add1 code) index?))))))))

; ***********************************************************

; ***********************************************************
; convert CSeries to SIndex, categorical series is like
; SIndex, it loops through the the noms and assigns an
; index incrementing from 0 to the nominal values in order.

(: CSeries->SIndex (CSeries -> SIndex))
(define (CSeries->SIndex cseries)

  (: sindex SIndex)
  (define sindex (make-hash))

  (let* ((noms (CSeries-nominals cseries))
	 (len (vector-length noms)))
    (do ([idx 0 (add1 idx)])
	([>= idx len] sindex)
      (when (index? idx)
	    (hash-set! sindex (string->symbol (vector-ref noms idx)) idx)))))

; ***********************************************************

; ***********************************************************
;; Map a function

; This function consumes a series and a function both of Float
; types and applies the function to each member of the series
; returning a new series.
(: map/cs (CSeries (String -> String) -> CSeries))
(define (map/cs series fn)
  (let ((old-nominals (CSeries-nominals series))
        (old-data (CSeries-data series))
	(new-nominals : (Vectorof String) (make-vector (vector-length (CSeries-nominals series)) ""))
        (new-data : (Vectorof Index) (make-vector (vector-length (CSeries-data series)))))
    (let ((len (vector-length old-nominals)))
      (let loop ((idx 0))
	(if (< idx len)
	    (begin
	      (vector-set! new-nominals idx (fn (vector-ref old-nominals idx)))
	      (loop (add1 idx)))
	    (void))))
    (CSeries (LabelIndex-index series) new-data new-nominals)))

; ***********************************************************

; ***********************************************************
;; Binary CSeries Ops

(: bop/cs (CSeries CSeries (String String -> String) -> CSeries))
(define (bop/cs cs1 cs2 bop)
  (define v1 (CSeries-nominals cs1))
  (define v2 (CSeries-nominals cs2))
  (define: len : Index (vector-length v1))
  
  (unless (eqv? len (vector-length v2))
	  (error '+/ns "Series must be of equal length."))

  (define: v-data : (Vectorof Index) (CSeries-data cs1))
  (define: v-nominals : (Vectorof String) (make-vector len ""))

  (do: : CSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
    ((= idx len) (CSeries #f v-data v-nominals))
    (vector-set! v-nominals idx (bop (vector-ref v1 idx) (vector-ref v2 idx)))))

; ***********************************************************

; ***********************************************************
;; Scalar CSeries Ops

(: bop./cs (String CSeries (String String -> String) -> CSeries))
(define (bop./cs str cs bcop)
  (define v1 (CSeries-nominals cs))
  (define: len : Index (vector-length v1))

  (define: v-data : (Vectorof Index) (CSeries-data cs))
  (define: v-nominals : (Vectorof String) (make-vector len ""))

  (do: : CSeries ([idx : Fixnum 0 (unsafe-fx+ idx 1)])
    ((= idx len) (CSeries #f v-data v-nominals))
    (vector-set! v-nominals idx (bcop (vector-ref v1 idx) str))))

; ***********************************************************

; ***********************************************************

(: cseries-referencer (CSeries -> (Fixnum -> String)))
(define (cseries-referencer cseries)
  (let ((data (CSeries-data cseries))
	(noms (CSeries-nominals cseries)))
    (λ: ((idx : Fixnum))
	(let ((code (vector-ref data idx)))
	  (vector-ref noms code)))))

(: cseries-iref (CSeries Fixnum -> String))
(define (cseries-iref cseries idx)
  (vector-ref (CSeries-nominals cseries)
	      (vector-ref (CSeries-data cseries) idx)))

(: cseries-length (CSeries -> Index))
(define (cseries-length series)
  (vector-length (CSeries-data series)))

; ***********************************************************

; ***********************************************************
; Test Cases
; ***********************************************************
; categorical series tests

; define new CSeries
(define series-categorical (new-CSeries (vector "hello" "world")))

(define series-categorical-2 (new-CSeries (vector "foo" "bar")))

(check-equal? (CSeries-data series-categorical) (vector 0 1))

(check-equal? (CSeries-nominals series-categorical) (vector "hello" "world"))

; need to make map function more robust
;(map/cs series-categorical string-length)

(check-equal? (CSeries-nominals (bop/cs series-categorical series-categorical-2 string-append))
              (vector "hellofoo" "worldbar"))

(check-equal? (CSeries-data (bop/cs series-categorical series-categorical-2 string-append))
              (vector 0 1))

(check-equal? (CSeries-nominals (bop./cs "test" series-categorical string-append))
              (vector "hellotest" "worldtest"))

(check-equal? (CSeries-data (bop./cs "test" series-categorical string-append))
              (vector 0 1))

; CSeries->SIndex tests
(check-equal? (hash-ref (CSeries->SIndex series-categorical) 'hello) 0)

(check-equal? (hash-ref (CSeries->SIndex series-categorical) 'world) 1)

(check-equal? ((cseries-referencer series-categorical) 1) "world")

(check-equal? (cseries-iref series-categorical 1) "world")

(check-equal? (cseries-length series-categorical) 2)
