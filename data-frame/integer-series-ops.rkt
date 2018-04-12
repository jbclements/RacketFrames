#lang typed/racket/base

(provide:
 ;[nseries-head (NSeries [#:rows Index] -> Void)]
 ;[nseries-unique (NSeries -> NSeries)] 
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

;(: remove-duplicates ((Listof Float) -> (Listof Float)))
;(define (remove-duplicates lst)
;  (foldr (lambda ([x : Float] [y : Float]) (cons x (filter (lambda ([z : Float]) (not (= x z))) y))) null lst))

;(: nseries-unique (NSeries -> NSeries))
;(define (nseries-unique nseries)
;  (NSeries #f (list->flvector (remove-duplicates (flvector->list (nseries-data nseries) 0)))))

(define default-nseries-rows 10)
