#lang typed/racket

(require typed/rackunit)

(provide:
 [iseries-head (ISeries [#:rows Index] -> ISeries)]
 [iseries-unique (ISeries -> ISeries)] 
 [iseries-append (ISeries ISeries -> ISeries)]
 [iseries-abs (ISeries -> ISeries)]
 [iseries-isna (ISeries -> BSeries)]
 [iseries-notna (ISeries -> BSeries)])

(require
 (only-in grip/data/format
	  ~a)
 (only-in racket/vector
	  vector-copy)
 (only-in "integer-series.rkt"
	  ISeries new-ISeries iseries-data iseries-length iseries-referencer iseries-iref)
 (only-in "boolean-series.rkt"
	  BSeries new-BSeries bseries-data bseries-length bseries-referencer bseries-iref)
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


(: remove-duplicates ((Listof Fixnum) -> (Listof Fixnum)))
(define (remove-duplicates lst)
  (foldr (lambda ([x : Fixnum] [y : (Listof Fixnum)]) (cons x (filter (lambda ([z : Fixnum]) (not (= x z))) y))) null lst))

(: iseries-unique (ISeries -> ISeries))
(define (iseries-unique iseries)
  (ISeries #f (list->vector (remove-duplicates (vector->list (iseries-data iseries))))))

(: iseries-head (ISeries [#:rows Index] -> ISeries))
(define (iseries-head iseries #:rows [rows 10])
  (if (< (vector-length (iseries-data iseries)) rows)
      (new-ISeries (for/vector: : (Vectorof Fixnum) ([i (vector-length (iseries-data iseries))]) (car (iseries-iref iseries (list i)))) #f)
      (new-ISeries (for/vector: : (Vectorof Fixnum) ([i rows]) (car (iseries-iref iseries (list i)))) #f)))

(define default-iseries-rows 10)

(: display-iseries-head (ISeries [#:rows Index] -> Void))
(define (display-iseries-head iseries #:rows [rows default-iseries-rows])
  (define iref (iseries-referencer iseries))
  (let ((rows (min rows (iseries-length iseries))))
    (do ([i 0 (add1 i)])
	((>= i rows) (displayln ""))
      (display (~a (string-append "[" (number->string i) "]") 
		   #:width 5 #:align 'left))
      (displayln (~a (number->string (iref (assert i index?))) 
		     #:align 'left)))))

(: iseries-abs (ISeries -> ISeries))
(define (iseries-abs iseries)
  (define iref (iseries-referencer iseries))
  (define rows (iseries-length iseries))
  (define builder (new-ISeriesBuilder rows))

  (for ([i rows])
    (append-ISeriesBuilder builder (assert (abs (iref (assert i index?))) fixnum?)))

  (complete-ISeriesBuilder builder))

; Series.isna()	Return a boolean same-sized object indicating if the values are NA.
(: iseries-isna (ISeries -> BSeries))
(define (iseries-isna iseries)
  (define nref (iseries-referencer iseries))
  (let ((rows (iseries-length iseries)))
    (define data : (Vectorof Boolean) (make-vector (iseries-length iseries) #f))
    (for ([i rows])
      (if (= (nref (assert i index?)) -10000)
          (vector-set! data i #t)
          (vector-set! data i #f)))
    (new-BSeries data #f)))

; Series.notna() Return a boolean same-sized object indicating if the values are not NA.
(: iseries-notna (ISeries -> BSeries))
(define (iseries-notna iseries)
  (define nref (iseries-referencer iseries))
  (let ((rows (iseries-length iseries)))
    (define data : (Vectorof Boolean) (make-vector (iseries-length iseries) #f))
    (for ([i rows])
      (if (= (nref (assert i index?)) -10000)
          (vector-set! data i #t)
          (vector-set! data i #f)))
    (new-BSeries data #f)))

; create integer series
(define series-integer (new-ISeries (vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) #f))

;(iseries-data (iseries-unique series-integer))

;(iseries-data (iseries-head series-integer))

(define series-integer-duplicates (new-ISeries (vector 5 5 5 5 5) #f))

;(iseries-data (iseries-unique series-integer-duplicates))

;(iseries-data (iseries-head series-integer-duplicates))

(define series-integer-negatives (new-ISeries (vector -5 -5 -5 -5 -5) #f))

;(iseries-data (iseries-abs series-integer-negatives))

;(bseries-data (iseries-isna series-integer))
