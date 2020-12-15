#lang typed/racket

(require RacketFrames)

(require "../../../util/list.rkt")


(define level1 (build-index-from-list (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))

;(random-number-list 10000 200)

;(random-symbol-list 50 10 "" '())

(define-predicate ListofFixnum? (Listof Fixnum))

(define integer-col-1 (cons 'col1 (new-ISeries (list->vector (assert (random-number-list 10000 200) ListofFixnum?)) #f)))

(define integer-col-2 (cons 'col2 (new-ISeries (list->vector (assert (random-number-list 10000 200) ListofFixnum?)) #f)))

(define integer-col-3 (cons 'col3 (new-ISeries (list->vector (assert (random-number-list 10000 200) ListofFixnum?)) #f)))

(define integer-col-4 (cons 'col4 (new-ISeries (list->vector (assert (random-number-list 10000 200) ListofFixnum?)) #f)))


(define columns-integer
  (list integer-col-1 integer-col-2 integer-col-3 integer-col-4))

(define data-frame-integer (new-data-frame columns-integer))


;(define float-col-1 (cons 'float-col-1 (new-NSeries (flvector 1.5 2.5 3.5 4.5 5.5) #f)))

;(define categorical-col-1 (cons' categorical-col-1 (new-CSeries (vector 'a 'b 'c 'd 'e) #f)))

(data-frame-write-tab data-frame-integer (current-output-port))

(displayln "data-frame-groupby")
(data-frame-groupby data-frame-integer (list 'col1))

(data-frame-groupby data-frame-integer (list 'col2))

(data-frame-groupby data-frame-integer (list 'col1 'col2))

(displayln "data-frame-groupby aggregate mean")
(apply-agg-data-frame 'mean (data-frame-groupby data-frame-integer (list 'col1 'col2)))

(displayln "data-frame-groupby aggregate count")
(apply-agg-data-frame 'count (data-frame-groupby data-frame-integer (list 'col1 'col2)))


