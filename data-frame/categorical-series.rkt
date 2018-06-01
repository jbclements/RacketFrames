#lang typed/racket

(provide
 (struct-out CSeries)
 new-CSeries)
;;writer-CSeries)

(provide:
 [CSeries->SIndex    (CSeries -> SIndex)]
 [cseries-length      (CSeries -> Index)]
 [cseries-iref        (CSeries (Listof Index) -> (Listof Label))]
 [cseries-range (CSeries Index -> (Vectorof Label))]
 [cseries-data        (CSeries -> (Vectorof Symbol))]
 [cseries-referencer (CSeries -> (Fixnum -> Label))]
 [cseries-iloc (CSeries (U Index (Listof Index)) -> (U Label CSeries))])

(require
 (only-in "indexed-series.rkt"
	  SIndex Label
	  LabelIndex))

(define-type CSeriesFn (Label -> Label))

;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer
;; See 12.8 Printer Extensions in Racket doc.
(: writer-CSeries (CSeries Output-Port Boolean -> Void))
(define (writer-CSeries series port mode)
  (let* ([data (CSeries-data series)]
	 [nominals (CSeries-nominals series)]
	 [len (vector-length data)])
    (do ([i 0 (add1 i)])
	((>= i len) (void))
      (displayln  (vector-ref nominals (vector-ref data i))))))

(struct: CSeries LabelIndex ([data     : (Vectorof Index)]
			     [nominals : (Vectorof Label)]))

;; #:methods gen:custom-write [(define write-proc writer-CSeries)])

(: new-CSeries ((Vectorof Label) -> CSeries))
(define (new-CSeries nominals)

  (: nominal-code (HashTable Label Index))
  (define nominal-code (make-hash))

  (define len (vector-length nominals))

  (: data (Vectorof Index))
  (define data (make-vector len 0))

  (: make-nominal-vector (-> (Vectorof Label)))
  (define (make-nominal-vector)
    (define nominals (make-vector (hash-count nominal-code) 'Null))
    (hash-for-each nominal-code
		   (λ: ((nom : Label) (idx : Index))
		       (vector-set! nominals idx nom)))
    nominals)

  (let: loop : CSeries ((idx : Natural 0) (code : Index 0))
	(if (>= idx len)
	    (CSeries #f data (make-nominal-vector))
	    (let ((nom (vector-ref nominals idx)))
	      (if (hash-has-key? nominal-code nom)
		  (begin
		    (vector-set! data idx (hash-ref nominal-code nom))
		    (loop (add1 idx) code))
		  (begin
		    (hash-set! nominal-code nom code)
		    (vector-set! data idx code)
		    (loop (add1 idx) (assert (add1 code) index?))))))))

(: CSeries->SIndex (CSeries -> SIndex))
(define (CSeries->SIndex cs)

  (: sindex SIndex)
  (define sindex (make-hash))

  (let* ((noms (CSeries-nominals cs))
	 (len (vector-length noms)))
    (do ([i 0 (add1 i)])
	([>= i len] sindex)
      (when (index? i)
	    (hash-set! sindex (vector-ref noms i) (list i))))))

(: cseries-referencer (CSeries -> (Fixnum -> Label)))
(define (cseries-referencer cseries)
  (let ((data (CSeries-data cseries))
	(noms (CSeries-nominals cseries)))
    (λ: ((idx : Fixnum))
	(let ((code (vector-ref data idx)))
	  (vector-ref noms code)))))

(: cseries-iref (CSeries (Listof Index) -> (Listof Label)))
(define (cseries-iref cseries lst-idx)
  (map (lambda ((idx : Index))
         (vector-ref (CSeries-nominals cseries)
                     (vector-ref (CSeries-data cseries) idx)))
       lst-idx))

; This function consumes an integer series and an index and
; returns a vector of values in the range [0:index] in the series.
(: cseries-range (CSeries Index -> (Vectorof Label)))
(define (cseries-range series pos)
  (vector-map (lambda ([idx : Index]) (vector-ref (CSeries-nominals series) idx))
              (vector-take (CSeries-data series) pos)))

(: cseries-length (CSeries -> Index))
(define (cseries-length series)
  (vector-length (CSeries-data series)))

(: cseries-data (CSeries -> (Vectorof Symbol)))
(define (cseries-data series)
  (CSeries-nominals series))

; label based
;(: cseries-loc ((Listof Label) -> Series)) ;
;(define (cseries-loc labels)
;)

; index based
(: cseries-iloc (CSeries (U Index (Listof Index)) -> (U Label CSeries)))
(define (cseries-iloc cseries idx)
  (let ((referencer (cseries-referencer cseries)))
  (if (list? idx)
      ; get labels from SIndex that refer to given indicies
      ; make a new index from these labels using build-index-from-labels
      ; sub-vector the data vector to get the data and create a new-BSeries
      (new-CSeries
       (for/vector: : (Vectorof Label) ([i idx])
         (vector-ref (cseries-data cseries) i)))
      (referencer idx))))
