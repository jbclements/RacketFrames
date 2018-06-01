#lang typed/racket

(require
 (only-in "../../data-frame/indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex
          label-index label->idx)

 (only-in "../../data-frame/categorical-series.rkt"
          new-CSeries
          cseries-iref
          cseries-range))

(require racket/format)

#| def setup(self, index):
        N = 10**5
        indexes = {'string': tm.makeStringIndex(N),
                   'datetime': date_range('1900', periods=N, freq='s')}
        index = indexes[index]
        self.s = Series(np.random.rand(N), index=index)
        self.lbl = index[80000]

    def time_getitem_label_slice(self, index):
        self.s[:self.lbl]

    def time_getitem_pos_slice(self, index):
        self.s[:80000]

    def time_get_value(self, index):
        with warnings.catch_warnings(record=True):
            self.s.get_value(self.lbl)

    def time_getitem_scalar(self, index):
        self.s[80000] |#

(define now current-inexact-milliseconds)

(define N (expt 10 5))

(: data (Vectorof Symbol))
(define data (make-vector N 'a))

(define series-categorical (new-CSeries data))

(define c-iref-bench-before (now))
(define cseries-iref-result (cseries-iref series-categorical 80000))
(define c-iref-bench-after (- (now) c-iref-bench-before))

(fprintf (current-output-port)
         "Categorical Series i-ref bench ~v ms.\n"
         c-iref-bench-after)

(printf "Pandas Compare* Running indexing.NonNumericSeriesIndexing.time_getitem_scalar 20.88ms. \n")

(define c-range-bench-before (now))
(define cseries-range-80000 (cseries-range series-categorical 80000))
(define c-range-bench-after (- (now) c-range-bench-before))

(fprintf (current-output-port)
         "Categorical Series range bench ~v ms.\n"
         c-range-bench-after)

(printf "Running indexing.NonNumericSeriesIndexing.time_getitem_pos_slice 69.11μs. \n") 

