#lang typed/racket

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require
 racket/unsafe/ops
 (only-in "../../data-frame/indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex
          label-index label->idx)

 (only-in "../../data-frame/integer-series.rkt"
          new-ISeries
          iseries-iref
          iseries-label-ref
          iseries-range))

(require racket/format)
; ***********************************************************

 #| def setup(self, index):
        N = 10**6
        idx = index(range(N))
        self.data = Series(np.random.rand(N), index=idx)
        self.array = np.arange(10000)
        self.array_list = self.array.tolist()

    def time_getitem_scalar(self, index):
        self.data[800000]

    def time_getitem_slice(self, index):
        self.data[:800000]

    def time_getitem_list_like(self, index):
        self.data[[800000]]

    def time_getitem_array(self, index):
        self.data[self.array]

    def time_getitem_lists(self, index):
        self.data[self.array_list]

    def time_iloc_array(self, index):
        self.data.iloc[self.array]

    def time_iloc_list_like(self, index):
        self.data.iloc[[800000]]

    def time_iloc_scalar(self, index):
        self.data.iloc[800000]

    def time_iloc_slice(self, index):
        self.data.iloc[:800000]

    def time_ix_array(self, index):
        self.data.ix[self.array]

    def time_ix_list_like(self, index):
        self.data.ix[[800000]]

    def time_ix_scalar(self, index):
        self.data.ix[800000]

    def time_ix_slice(self, index):
        self.data.ix[:800000]

    def time_loc_array(self, index):
        self.data.loc[self.array]

    def time_loc_list_like(self, index):
        self.data.loc[[800000]]

    def time_loc_scalar(self, index):
        self.data.loc[800000]

    def time_loc_slice(self, index):
        self.data.loc[:800000] |#

(define now current-inexact-milliseconds)

(define N (expt 10 6))

(: data (Vectorof Fixnum))
(define data (make-vector N (random N)))

(define series-integer (new-ISeries data #f))

#| def time_getitem_scalar(self, index):
      self.data[800000] |#
(define i-ref-bench-before (now))
(define iseries-iref-value (iseries-iref series-integer 80000))
(define i-ref-bench-after (- (now) i-ref-bench-before))

(fprintf (current-output-port)
         "Integer Series i-ref Bench: ~v ms.\n"
         i-ref-bench-after)

#| def time_getitem_slice(self, index):
        self.data[:800000] |#
(define i-range-bench-before (now))
(define iseries-range-80000 (iseries-range series-integer 80000))
(define i-range-bench-after (- (now) i-range-bench-before))

(fprintf (current-output-port)
         "Integer Series range bench ~v ms.\n"
         i-range-bench-after)

(: label-index (Listof Symbol))
(define label-index (for/list: : (Listof Symbol)
                      ([i N])
                      (string->symbol (string-append "a" (number->string i)))))

(define series-integer-with-label-index (new-ISeries data
                                                     (build-index-from-labels label-index)))

(define i-ref-label-bench-before (now))
(define iseries-label-ref-value (iseries-label-ref series-integer-with-label-index 'a80000))
(define i-ref-label-bench-after (- (now) i-ref-label-bench-before))

(fprintf (current-output-port)
         "Integer Series label ref bench: ~v ms.\n"
         i-ref-label-bench-after)
