#lang typed/racket

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 racket/set
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in grip/data/symbol
	  symbol-prefix)
 (only-in "../../../data-frame/indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "../../../data-frame/series.rkt"
	  series-complete)
 (only-in "../../../data-frame/data-frame.rkt"
	  DataFrame new-data-frame
	  data-frame-cseries data-frame-extend)
 (only-in "../../../data-frame/data-frame-concat.rkt"
          data-frame-concat-vertical)
 (only-in "../../../data-frame/numeric-series.rkt"
	  NSeries nseries-iref nseries-label-ref new-NSeries)
 (only-in "../../../data-frame/integer-series.rkt"
	  ISeries iseries-iref new-ISeries)
 (only-in "../../../data-frame/categorical-series.rkt"
          cseries-iref
	  CSeries new-CSeries))

#| 
    def setup(self):
        self.df1 = DataFrame(np.random.randn(10000, 4),
                             columns=['A', 'B', 'C', 'D'])
        self.df2 = self.df1.copy()
        self.df2.index = np.arange(10000, 20000)
        self.mdf1 = self.df1.copy()
        self.mdf1['obj1'] = 'bar'
        self.mdf1['obj2'] = 'bar'
        self.mdf1['int1'] = 5
        try:
            with warnings.catch_warnings(record=True):
                self.mdf1.consolidate(inplace=True)
        except:
            pass
        self.mdf2 = self.mdf1.copy()
        self.mdf2.index = self.df2.index

    def time_append_homogenous(self):
        self.df1.append(self.df2)

    def time_append_mixed(self):
        self.mdf1.append(self.mdf2)
 |#

(define now current-inexact-milliseconds)

; Setup
(define N (expt 10 4))

(define col-a-data (for/vector: : (Vectorof Fixnum) ([i N]) (random N)))

(define col-b-data (for/vector: : (Vectorof Fixnum) ([i N]) (random N)))

(define col-c-data (for/vector: : (Vectorof Fixnum) ([i N]) (random N)))

; no for/flvector loop
(define col-d-data (make-flvector N))

(for ([i N])
  (flvector-set! col-d-data i (real->double-flonum (random N))))

(: col-obj1-data (Vectorof Symbol))
(define col-obj1-data (make-vector N 'bar))

(: col-obj2-data (Vectorof Symbol))
(define col-obj2-data (make-vector N 'bar))

(define col-int1-data (for/vector: : (Vectorof Fixnum) ([i N]) (random 5)))

;******************
; data-frame-numerical
;******************
(define columns-numerical
  (list
   (cons 'A (new-ISeries col-a-data #f))
   (cons 'B (new-ISeries col-b-data #f))
   (cons 'C (new-ISeries col-c-data #f))
   (cons 'D (new-NSeries col-d-data #f))))

;******************
; data-frame-mixed
;******************
(define columns-mixed
  (list
   (cons 'A (new-ISeries col-a-data #f))
   (cons 'B (new-ISeries col-b-data #f))
   (cons 'C (new-ISeries col-c-data #f))
   (cons 'D (new-NSeries col-d-data #f))
   (cons 'ob1 (new-CSeries col-obj1-data))
   (cons 'ob2 (new-CSeries col-obj2-data))
   (cons 'int1 (new-ISeries col-int1-data #f))))

; create new data-frame-numerical
(define data-frame-numerical (new-data-frame columns-numerical))

(define append-numerical-before (now))
(define concat-vertical-result (data-frame-concat-vertical data-frame-numerical data-frame-numerical #:col '()))
(define append-numerical-after (- (now) append-numerical-before))

(fprintf (current-output-port)
         "Append numerical bench ~v ms.\n"
         append-numerical-after)

(printf "Pandas Compare* join_merge.Append.time_append_homogenous 499.99μs. \n")

; create new data-frame-mixed
(define data-frame-mixed (new-data-frame columns-mixed))

(define append-mixed-before (now))
(define concat-vertical-result-2 (data-frame-concat-vertical data-frame-mixed data-frame-mixed #:col '()))
(define append-mixed-after (- (now) append-mixed-before))

(fprintf (current-output-port)
         "Append mixed bench ~v ms.\n"
         append-mixed-after)

(printf "Pandas Compare* join_merge.Append.time_append_mixed 1.04ms. \n")
