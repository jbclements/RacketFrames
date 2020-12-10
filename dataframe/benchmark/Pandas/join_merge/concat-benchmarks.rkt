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
 (only-in "../../../data-frame/indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "../../../data-frame/series.rkt"
	  series-complete)
 (only-in "../../../data-frame/series-description.rkt"
	  SeriesType Series
	  SeriesDescription-type
	  series-type series-length
          series-data)
 (only-in "../../../data-frame/data-frame.rkt"
	  DataFrame new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode data-frame-extend
	  DataFrameDescription DataFrameDescription-series data-frame-description show-data-frame-description
          Column Columns)
 (only-in "../../../data-frame/data-frame-concat.rkt"
          data-frame-concat-vertical
          data-frame-concat-vertical-list
          data-frame-concat-horizontal
          data-frame-concat-horizontal-list)
 (only-in "../../../data-frame/numeric-series.rkt"
	  NSeries NSeries? nseries-iref new-NSeries)
 (only-in "../../../data-frame/integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer)
 (only-in "../../../data-frame/categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "../../../data-frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../../../data-frame/integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "../../../data-frame/categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "../../../data-frame/categorical-series-ops.rkt"
	  cseries-append)
 (only-in "../../../data-frame/numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder))

#|  def setup(self, axis, ignore_index):
        frame_c = DataFrame(np.zeros((10000, 200),
                            dtype=np.float32, order='C'))
        self.frame_c = [frame_c] * 20
        frame_f = DataFrame(np.zeros((10000, 200),
                            dtype=np.float32, order='F'))
        self.frame_f = [frame_f] * 20

    def time_c_ordered(self, axis, ignore_index):
        concat(self.frame_c, axis=axis, ignore_index=ignore_index)

    def time_f_ordered(self, axis, ignore_index):
        concat(self.frame_f, axis=axis, ignore_index=ignore_index)
|#

(define now current-inexact-milliseconds)

; Setup
(define N (expt 10 4))

(define frame-c (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (number->string (+ col (random))))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0) #f)))))

(define twenty-frame-c (for/list: : (Listof DataFrame) ([i 20]) (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (number->string (+ col (random))))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0) #f))))))

(define frame-f (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (number->string col))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0) #f)))))

(define twenty-frame-f (for/list: : (Listof DataFrame) ([i 20]) frame-f))

; horizontal concat benchmark
(define data-frame-list-horizontal-concat-bench-before (now))
(define concat-horizontal-result (data-frame-concat-horizontal-list twenty-frame-c))
(define data-frame-list-horizontal-concat-bench-after (- (now) data-frame-list-horizontal-concat-bench-before))

(show-data-frame-description (data-frame-description concat-horizontal-result))

(fprintf (current-output-port)
         "DataFrame list horizontal concat bench ~v ms.\n"
         data-frame-list-horizontal-concat-bench-after)
(printf "Pandas Compare* join_merge.ConcatDataFrames.time_c_ordered 123.69ms")

;vertical concat benchmark
(define data-frame-list-vertical-concat-bench-before (now))
(define concat-vertical-result (data-frame-concat-vertical-list twenty-frame-f))
(define data-frame-list-vertical-concat-bench-after (- (now) data-frame-list-vertical-concat-bench-before))

(show-data-frame-description (data-frame-description concat-vertical-result))

(fprintf (current-output-port)
         "DataFrame list vertical concat bench ~v ms.\n"
         data-frame-list-vertical-concat-bench-after)
(printf "Pandas Compare* join_merge.ConcatDataFrames.time_f_ordered 123.28ms")
