#lang typed/racket

(provide:
 ;[iseries-head (ISeries [#:rows Index] -> Void)]
 ;[iseries-unique (ISeries -> ISeries)] 
 [iseries-append (ISeries ISeries -> ISeries)])

(require
 (only-in grip/data/format
	  ~a)
 (only-in racket/vector
	  vector-copy)
 (only-in "integer-series.rkt"
	  ISeries iseries-data iseries-length iseries-referencer)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder
	  new-ISeriesBuilder
	  append-ISeriesBuilder
	  complete-ISeriesBuilder))

(: iseries-append (ISeries ISeries -> ISeries))
(define (iseries-append isa isb)

  (define builder (new-ISeriesBuilder (assert (+ (iseries-length isa)
						 (iseries-length isb))
					      index?)))

  (: append-iseries (ISeries -> Void))
  (define (append-iseries is)
    (define is-cnt (iseries-length is))
    (define iref (iseries-referencer is))
    (do ([i 0 (add1 i)])
	((>= i is-cnt))
      (append-ISeriesBuilder builder (iref (assert i index?)))))
  
  (append-iseries isa)
  (append-iseries isb)
  (complete-ISeriesBuilder builder))

#|
(: remove-duplicates ((Listof Fixnum) -> (Listof Fixnum)))
(define (remove-duplicates lst)
  (foldr (lambda ([x : Fixnum] [y : Fixnum]) (cons x (filter (lambda ([z : Fixnum]) (not (= x z))) y))) null lst))

(: iseries-unique (ISeries -> ISeries))
(define (iseries-unique iseries)
  (ISeries #f (list->vector (remove-duplicates (vector->list (iseries-data iseries) 0)))))
|#

(define default-nseries-rows 10)
