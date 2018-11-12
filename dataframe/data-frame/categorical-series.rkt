#lang typed/racket

(provide
 (struct-out CSeries)
 new-CSeries)
;;writer-CSeries)

(provide:
 ;[CSeries->RIndex    (CSeries -> RIndex)]
 [cseries-length      (CSeries -> Index)]
 [cseries-iref        (CSeries (Listof Index) -> (Listof Label))]
 [cseries-range (CSeries Index -> (Vectorof Label))]
 [cseries-data        (CSeries -> (Vectorof Symbol))]
 [cseries-referencer (CSeries -> (Fixnum -> Label))]
 [cseries-iloc (CSeries (U Index (Listof Index)) -> (U Label CSeries))])
 ;[cseries-print (CSeries Output-Port -> Void)])

(require
 (only-in "indexed-series.rkt"
	  RFIndex Label idx->key
	  LabelIndex LabelIndex-index is-labeled?))

(define-type CSeriesFn (Label -> Label))

;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer
(: cseries-print (CSeries Output-Port -> Void))
(define (cseries-print cseries port)
  (let* ([data (CSeries-data cseries)]
	 [nominals (CSeries-nominals cseries)]
	 [len (vector-length data)])
    (do ([i 0 (add1 i)])
	((>= i len) (void))
      (if (CSeries-index cseries)                  
          (display (idx->key (CSeries-index cseries) (assert i index?)) port)
          (display (assert i index?) port))
      (display " " port)
      (displayln  (vector-ref nominals (vector-ref data i))))))

(struct: CSeries ([index : (Option RFIndex)]
                  [data : (Vectorof Index)]
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

#|
(: CSeries->RIndex (CSeries -> RFIndex))
(define (CSeries->RIndex cs)

  (: rfindex RFIndex)
  (define rfindex (make-hash))

  (let* ((noms (CSeries-nominals cs))
	 (len (vector-length noms)))
    (do ([i 0 (add1 i)])
	([>= i len] rfindex)
      (when (index? i)
	    (hash-set! rfindex (vector-ref noms i) (list i))))))|#

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

#| (: build-labels-by-count ((Listof Label) (Listof Integer) -> (Listof Label)))
(define (build-labels-by-count label-lst count-lst)
  (if (null? label-lst)
      null
      (append
       (for/list: : (Listof Label)
         ([i (car count-lst)])
         (car label-lst))
       
       (build-labels-by-count (cdr label-lst) (cdr count-lst)))))

(: convert-to-label-lst ((U Label (Listof Label)) -> (Listof Label)))
(define (convert-to-label-lst label)
  (if (list? label)
      label
      (list label)))

(define-predicate ListofBoolean? (Listof Boolean))
(define-predicate ListofFixnum? (Listof Fixnum))

; label based
; for two different use cases:
; a.) Selecting rows by label/index
; b.) Selecting rows with a boolean / conditional lookup

; Valid inputs
; A single label, e.g. 'a'.
; A list or array of labels ['a', 'b', 'c'].
; A boolean array.

(: true? (Boolean -> Boolean))
(define (true? boolean)
  (not (false? boolean)))

(: cseries-loc-boolean (CSeries (Listof Boolean) -> (U Fixnum CSeries)))
(define (cseries-loc-boolean cseries boolean-lst)
  (: data (Vectorof Fixnum))
  (define data (cseries-data cseries))

  (: new-data (Vectorof Fixnum))
  (define new-data (make-vector (length (filter true? boolean-lst)) 0))
  
  (define data-idx 0)
  (define new-data-idx 0)

  (if (= (length boolean-lst) 1)
      (if (list-ref boolean-lst 0)
          (vector-ref data 0)
          ; empty cseries
          (new-CSeries (vector) #f))
       
      (for ([b boolean-lst]
            [d data])
        (begin
          (when b
            (begin              
              (vector-set! new-data new-data-idx (vector-ref data data-idx))
              (set! new-data-idx (add1 new-data-idx))))
          (set! data-idx (add1 data-idx)))))

  (if (= (vector-length new-data) 1)
      (vector-ref new-data 0)
      (new-CSeries new-data #f)))
    
(: cseries-loc (CSeries (U Label (Listof Label) (Listof Boolean)) -> (U Fixnum CSeries)))
(define (cseries-loc cseries label)
  (unless (is-labeled? cseries)
    (let ((k (current-continuation-marks)))
      (raise (make-exn:fail:contract "cseries must have a label index." k))))

  (if (ListofBoolean? label)
      (cseries-loc-boolean cseries label)
      (let ((associated-indices-length : (Listof Integer)
                                       (map (lambda ([l : Label]) (length (cseries-label-ref cseries l))) (convert-to-label-lst label)))
            (vals : (Vectorof Fixnum)
             (if (list? label)
                 (list->vector (assert (flatten (map (lambda ([l : Label]) (cseries-label-ref cseries l)) label)) ListofFixnum?))
                 (list->vector (assert (cseries-label-ref cseries label) ListofFixnum?)))))

        (if (= (vector-length vals) 1)
            (vector-ref vals 0)
            (new-CSeries vals (build-index-from-labels (build-labels-by-count (convert-to-label-lst label) associated-indices-length))))))) |#

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
