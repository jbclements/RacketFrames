#lang typed/racket

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 racket/set
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract))

(require RacketFrames)


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
                   (cons (string->symbol (~a "df~" i "~" col))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0) #f))))))

(define frame-f (new-data-frame
                 (for/list: : Columns ([col 200])
                   (cons (string->symbol (~a "df~" col))
                           (new-ISeries (for/vector: : (Vectorof Fixnum) ([i N]) 0) #f)))))

(define twenty-frame-f (for/list: : (Listof DataFrame) ([i 20]) frame-f))

; horizontal concat benchmark
(define data-frame-list-horizontal-concat-bench-before (now))
(define concat-horizontal-result (data-frame-concat-horizontal-list twenty-frame-c))
(define data-frame-list-horizontal-concat-bench-after (- (now) data-frame-list-horizontal-concat-bench-before))

(show-data-frame-description (data-frame-description concat-horizontal-result))

;(data-frame-head concat-horizontal-result)

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
