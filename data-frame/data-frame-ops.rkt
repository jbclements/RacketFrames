#lang typed/racket

; ***********************************************************
; Provide functions in this file to other files.

(provide:
 [data-frame+ (DataFrame DataFrame -> DataFrame)]
 [data-frame- (DataFrame DataFrame -> DataFrame)]
 [data-frame* (DataFrame DataFrame -> DataFrame)]
 [data-frame/ (DataFrame DataFrame -> DataFrame)]
 [data-frame% (DataFrame DataFrame -> DataFrame)]
 [data-frame-r (DataFrame DataFrame -> DataFrame)]
 [data-frame= (DataFrame DataFrame -> DataFrame)]
 [data-frame!= (DataFrame DataFrame -> DataFrame)]
 [data-frame>= (DataFrame DataFrame -> DataFrame)]
 [data-frame<= (DataFrame DataFrame -> DataFrame)]
 [data-frame> (DataFrame DataFrame -> DataFrame)]
 [data-frame< (DataFrame DataFrame -> DataFrame)]
 [data-frame-abs (DataFrame -> DataFrame)])
; ***********************************************************

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
	  DataFrame Columns new-data-frame data-frame-names
	  data-frame-series data-frame-cseries data-frame-nseries data-frame-iseries data-frame-explode
	  DataFrameDescription DataFrameDescription-series data-frame-description column-series)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-iref nseries-label-ref new-NSeries
          +/ns -/ns */ns //ns >/ns </ns
          >=/ns <=/ns =/ns !=/ns +./ns
          -./ns *./ns /./ns +/ns/is -/ns/is
          */ns/is //ns/is +/is/ns -/is/ns */is/ns
          //is/ns >/ns/is </ns/is >=/ns/is <=/ns/is 
          =/ns/is !=/ns/is >/is/ns </is/ns >=/is/ns 
          <=/is/ns =/is/ns !=/is/ns)
 (only-in "numeric-series-ops.rkt"
          nseries-abs)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer +/is -/is */is //is
          %/is r/is >/is </is >=/is <=/is =/is !=/is)
 (only-in "integer-series-ops.rkt"
          iseries-abs)
 (only-in "boolean-series.rkt"
	  BSeries BSeries? bseries-iref new-BSeries
	  bseries-referencer)
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
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "boolean-series-builder.rkt"
	  BSeriesBuilder BSeriesBuilder?
	  append-BSeriesBuilder complete-BSeriesBuilder
	  new-BSeriesBuilder)
 (only-in "data-frame-print.rkt"
          frame-write-tab))


;(: data-frame+ (DataFrame DataFrame -> DataFrame))
(: data-frame+ (DataFrame DataFrame -> DataFrame))
(define (data-frame+ dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (+/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (+/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (+/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (+/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame+
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

; Test Cases

(define columns-integer-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-integer-1
(define data-frame-integer-1 (new-data-frame columns-integer-1))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

(displayln "data-frame+ Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame+ data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame- (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame- (DataFrame DataFrame -> DataFrame))
(define (data-frame- dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (-/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (-/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (-/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (-/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame-
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame- Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame- data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame* (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame* (DataFrame DataFrame -> DataFrame))
(define (data-frame* dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (*/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (*/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (*/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (*/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame*
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame* Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame* data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame/ (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame/ (DataFrame DataFrame -> DataFrame))
(define (data-frame/ dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (//ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (//is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (//is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (//ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame/
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame/ Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame/ data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame% (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame% (DataFrame DataFrame -> DataFrame))
(define (data-frame% dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))                                                                      
                                   ((list 'IntegerSeries 'IntegerSeries)    (%/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))                                   
                                   (else (error 'data-frame%
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame% Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame% data-frame-integer-1 data-frame-integer-2) (current-output-port))

(: data-frame-r (DataFrame DataFrame -> DataFrame))
(define (data-frame-r dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: append-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))                                                                      
                                   ((list 'IntegerSeries 'IntegerSeries)    (r/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))                                   
                                   (else (error 'data-frame-r
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? append-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame-r Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame-r data-frame-integer-1 data-frame-integer-2) (current-output-port))


;(: data-frame= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame= (DataFrame DataFrame -> DataFrame))
(define (data-frame= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame= Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame= data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame!= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame!= (DataFrame DataFrame -> DataFrame))
(define (data-frame!= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (!=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (!=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (!=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (!=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame!=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame!= Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame!= data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame< (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame< (DataFrame DataFrame -> DataFrame))
(define (data-frame< dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (</ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (</is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (</is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (</ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame< Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame< data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame> (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame> (DataFrame DataFrame -> DataFrame))
(define (data-frame> dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (>/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (>/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (>/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (>/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame>
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame> Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame> data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame<= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame<= (DataFrame DataFrame -> DataFrame))
(define (data-frame<= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (<=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (<=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (<=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (<=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame<=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame<= Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame<= data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame>= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame>= (DataFrame DataFrame -> DataFrame))
(define (data-frame>= dfa dfb)
  (define: cols-a    : (Setof Label) (list->set (data-frame-names dfa)))
  (define: cols-b    : (Setof Label) (list->set (data-frame-names dfb)))
  (define: matching-cols : (Setof Label) (set-intersect cols-a cols-b))
  
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((dfa-series (data-frame-series dfa name))
                                     (dfb-series (data-frame-series dfb name))) 
                                 (match (list (series-type dfa-series) (series-type dfb-series))
                                   ((list 'NumericSeries 'IntegerSeries)     (>=/ns/is (data-frame-nseries dfa name)
                                                                                      (data-frame-iseries dfb name)))
                                   ((list 'IntegerSeries 'NumericSeries)    (>=/is/ns (data-frame-iseries dfa name)
                                                                                     (data-frame-nseries dfb name)))
                                   ((list 'IntegerSeries 'IntegerSeries)    (>=/is (data-frame-iseries dfa name)
                                                                                  (data-frame-iseries dfb name)))
                                   ((list 'NumericSeries 'NumericSeries)    (>=/ns (data-frame-nseries dfa name)
                                                                                  (data-frame-nseries dfb name)))
                                   (else (error 'data-frame>=
                                                "Incorrect series type ~a ~a, must be Integer or Numeric."
                                                (series-type dfa-series) (series-type dfb-series)))))))
                         (filter (λ: ((name : Label))
                                   (set-member? matching-cols name))
                                 (data-frame-names dfa)))))

(displayln "data-frame>= Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame>= data-frame-integer-1 data-frame-integer-2) (current-output-port))

;(: data-frame-abs (DataFrame [#:on (Listof Symbol)] -> DataFrame))
(: data-frame-abs (DataFrame -> DataFrame))
(define (data-frame-abs df)
  (new-data-frame (map (λ: ((name : Label))
                         (cons name
                               (let ((df-series (data-frame-series df name))) 
                                 (match (series-type df-series)
                                   ('NumericSeries (nseries-abs (data-frame-nseries df name)))
                                   ('IntegerSeries (iseries-abs (data-frame-iseries df name)))
                                   (else (error 'data-frame-abs
                                                "Incorrect series type ~a, must be Integer or Numeric."
                                                (series-type df-series)))))))
                       (data-frame-names df))))

(displayln "data-frame-abs Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(define columns-integer-3
  (list 
   (cons 'col1 (new-ISeries (vector -1 -2 -3 -4) #f))
   (cons 'col2 (new-ISeries (vector -5 -6 -7 -8) #f))
   (cons 'col3 (new-ISeries (vector -9 -10 -11 -12) #f))
   (cons 'col4 (new-ISeries (vector -21 -22 -23 -24) #f))))

; create new data-frame-integer-3
(define data-frame-integer-3 (new-data-frame columns-integer-3))

(frame-write-tab data-frame-integer-3 (current-output-port))

(frame-write-tab (data-frame-abs data-frame-integer-1) (current-output-port))

(frame-write-tab (data-frame-abs data-frame-integer-3) (current-output-port))

; DataFrame.apply(func[, axis, broadcast, ...])	Applies function along input axis of DataFrame.
; (: data-frame-apply func)

; DataFrame.applymap(func)	Apply a function to a DataFrame that is intended to operate elementwise, i.e.
; (: data-frame-applymap func)

; DataFrame.aggregate(func[, axis])	Aggregate using callable, string, dict, or list of string/callables
; (: data-frame-aggregate func)

; DataFrame.transform(func, *args, **kwargs)	Call function producing a like-indexed NDFrame
; (: data-frame-transform func)

; DataFrame.groupby([by, axis, level, ...])	Group series using mapper (dict or key function, apply given function to group, return result as series) or by a series of columns.
; (: data-frame-groupby [#:by (Listof Symbol)])
