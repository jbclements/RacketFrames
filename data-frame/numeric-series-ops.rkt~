#lang typed/racket

(provide:
 ;[nseries-head (NSeries [#:rows Index] -> Void)]
 ;[nseries-unique (NSeries -> NSeries)] 
 [nseries-append (NSeries NSeries -> NSeries)])

(require 
 (only-in grip/data/format
	  ~a)
 (only-in racket/vector
	  vector-copy)
 (only-in "numeric-series.rkt"
	  NSeries nseries-data nseries-length nseries-referencer flvector->list list->flvector)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder
	  new-NSeriesBuilder
	  append-NSeriesBuilder
	  complete-NSeriesBuilder))

(: nseries-append (NSeries NSeries -> NSeries))
(define (nseries-append nsa nsb)

  (define builder (new-NSeriesBuilder (assert (+ (nseries-length nsa)
						 (nseries-length nsb))
					      index?)))

  (: append-nseries (NSeries -> Void))
  (define (append-nseries ns)
    (define ns-cnt (nseries-length ns))
    (define nref (nseries-referencer ns))
    (do ([i 0 (add1 i)])
	((>= i ns-cnt))
      (append-NSeriesBuilder builder (nref (assert i index?)))))
  
  (append-nseries nsa)
  (append-nseries nsb)
  (complete-NSeriesBuilder builder))

;(: remove-duplicates ((Listof Float) -> (Listof Float)))
;(define (remove-duplicates lst)
;  (foldr (lambda ([x : Float] [y : Float]) (cons x (filter (lambda ([z : Float]) (not (= x z))) y))) null lst))

;(: nseries-unique (NSeries -> NSeries))
;(define (nseries-unique nseries)
;  (NSeries #f (list->flvector (remove-duplicates (flvector->list (nseries-data nseries) 0)))))

(define default-nseries-rows 10)
