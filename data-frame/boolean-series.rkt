;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*    Author: Shubham Kahal
;*    File: boolean-series.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket
(require typed/rackunit)

; ***********************************************************
; One-dimensional array like structure with axis labels. Labels
; must be unique and must be a hashable type. The series object
; supports both integer (idx) and label-based indexing. Functions
; can be mapped to each value of the series allowing for various
; operations. The integer series is optimized for working booleans.
; ***********************************************************

; **************************
; Test cases are at bottom
; of file.
; **************************

; ***********************************************************
; Provide functions in this file to other files.
(provide
 (struct-out BSeries))

(provide:
 [new-BSeries ((Vectorof Boolean) (Option (U (Listof Label) SIndex)) -> BSeries)]
 [set-BSeries-index (BSeries (U (Listof Label) SIndex) -> BSeries)]
 [bseries-iref (BSeries (Listof Index) -> (Listof Boolean))]
 [bseries-label-ref (BSeries Label -> (Listof Boolean))]
 [bseries-range (BSeries Index -> (Vectorof Boolean))]
 [bseries-length (BSeries -> Index)]
 [bseries-referencer (BSeries -> (Index -> Boolean))]
 [bseries-data (BSeries -> (Vectorof Boolean))]
 [map/bs (BSeries (Boolean -> Boolean) -> BSeries)])
; ***********************************************************

; ***********************************************************
; use build-index-from-labels function and Label, SIndex and
; LabelIndex structs from indexed-series.
(require
 racket/unsafe/ops
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label LabelIndex-index SIndex
          LabelIndex label-index label->lst-idx
          idx->label))
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

;; Boolean series.
(struct BSeries LabelIndex ([data : (Vectorof Boolean)]))

; Consumes a Vector of Fixnum and a list of Labels which
; can come in list form or SIndex form and produces a ISeries
; struct object.
(: new-BSeries ((Vectorof Boolean) (Option (U (Listof Label) SIndex)) -> BSeries))
(define (new-BSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(BSeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (BSeries index data))
	  (BSeries #f data))))
; ***********************************************************

; ***********************************************************
(: set-BSeries-index (BSeries (U (Listof Label) SIndex) -> BSeries))
(define (set-BSeries-index bseries labels)

  (define data (BSeries-data bseries))
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(BSeries labels data))
      (let ((index (build-index-from-labels labels)))
        (check-mismatch index)
        (BSeries index data))))
; ***********************************************************

; ***********************************************************
; This function consumes an integer series and returns a
; lambda function which consumes an index and provides the
; value of the data at that index in the series. It can be
; defined once and used repeatedly as a referencer.
(: bseries-referencer (BSeries -> (Index -> Boolean)))
(define (bseries-referencer bseries)
  (let ((data (BSeries-data bseries)))
    (λ: ((idx : Index))
	(vector-ref data idx))))

; This function consumes an integer series and an index and
; returns the value at that index in the series.
(: bseries-iref (BSeries (Listof Index) -> (Listof Boolean)))
(define (bseries-iref series lst-idx)
  (map (lambda ((idx : Index)) (vector-ref (BSeries-data series) idx))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: bseries-range (BSeries Index -> (Vectorof Boolean)))
(define (bseries-range series pos)
   (vector-take (BSeries-data series) pos))

; This function consumes an integer series and returns its
; data vector.
(: bseries-data (BSeries -> (Vectorof Boolean)))
(define (bseries-data series)
  (BSeries-data series))

; This function consumes a series and a Label and returns
; the value at that Label in the series.
(: bseries-label-ref (BSeries Label -> (Listof Boolean)))
(define (bseries-label-ref series label)
  (bseries-iref series (label->lst-idx series label)))

; This function consumes an integer series and returns the
; length of that series.
(: bseries-length (BSeries -> Index))
(define (bseries-length series)
  (vector-length (BSeries-data series)))
; ***********************************************************

; ***********************************************************
(: map/bs (BSeries (Boolean -> Boolean) -> BSeries))
(define (map/bs series fn)
  (let ((old-data (BSeries-data series)))
    (BSeries #f (build-vector (vector-length old-data)
                              (λ: ((idx : Natural))
                                (fn (vector-ref old-data idx)))))))
; ***********************************************************

; ***********************************************************
; Indexing

; label based
;(: bseries-loc ((Listof Label) -> Series)) ;
;(define (bseries-loc labels)
; (when (not is-labeled?) #f)
;)

; index based
(: bseries-iloc (BSeries (U Index (Listof Index)) -> (U Boolean BSeries)))
(define (bseries-iloc bseries idx)
  (let ((referencer (bseries-referencer bseries)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-BSeries
      (new-BSeries
       (for/vector: : (Vectorof Boolean) ([i idx])
         (vector-ref (bseries-data bseries) i))
       (build-index-from-labels (map (lambda ([i : Index]) (idx->label bseries i)) idx)))
      (referencer idx))))

; ***********************************************************
; Test Cases
; ***********************************************************

; boolean series tests

; create boolean series
(define series-boolean (new-BSeries (vector #f #t #t #t)
                                    (build-index-from-labels (list 'a 'b 'c 'd))))

(define series-boolean-2 (new-BSeries (vector #f #t #t #f)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

; bseries reference tests
(check-equal? ((bseries-referencer series-boolean) 0) #f)

(check-equal? ((bseries-referencer series-boolean) 1) #t)

(check-equal? (bseries-iref series-boolean (list 0)) (list #f))

(check-equal? (bseries-iref series-boolean (list 1)) (list #t))

(check-equal? (bseries-label-ref series-boolean 'd) (list #t))

(check-equal? (bseries-label-ref series-boolean 'c) (list #t))

; series length
(check-equal? (bseries-length series-boolean) 4)

(check-equal? (bseries-data (map/bs series-boolean (lambda (b) (if (not b) #t #f)))) (vector #t #f #f #f))

(check-equal? (bseries-iloc series-boolean 1) #t)

(check-equal? (bseries-data (assert (bseries-iloc series-boolean (list 2 3)) BSeries?)) (vector #t #t))

(check-equal? (bseries-data (assert (bseries-iloc series-boolean (range 4)) BSeries?)) (vector #f #t #t #t))
