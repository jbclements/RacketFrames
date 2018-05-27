#lang typed/racket

(require
 (only-in "indexed-series.rkt"
	  Label SIndex LabelIndex)
 (only-in "data-frame-join.rkt"
          IndexableSeries key-fn))

; This function consumes a Listof Listof Label and creates
; a MultiIndex.
#| (: build-multi-index-from-labels ((Listof (Listof Label)) -> SIndex))
(define (build-multi-index-from-labels nested-label-lst)

  ; Get length of one of the IndexableSeries
  (define len (length (car nested-label-lst)))
  (define: series-key : (Index -> String) (key-fn cols))

  (let ((index : SIndex (make-hash '())))

    (let loop ([i 0])
      (if (unsafe-fx>= i len)
          index
          (let: ((i : Index (assert i index?)))
            (let ((key (series-key i)))
              (hash-update! index key
                            (λ: ((idx : (Listof Index)))
                              (cons i idx))
                            (λ () (list))))
            (loop (add1 i))))))) |#

(: build-multi-index-from-cols ((Listof IndexableSeries) -> SIndex))
(define (build-multi-index-from-cols cols)

  ; Get length of one of the IndexableSeries
  (define len (series-length (car cols)))
  (define: series-key : (Index -> String) (key-fn cols))

  (let ((index : SIndex (make-hash '())))

    (let loop ([i 0])
      (if (unsafe-fx>= i len)
          index
          (let: ((i : Index (assert i index?)))
            (let ((key (series-key i)))
              (hash-update! index key
                            (λ: ((idx : (Listof Index)))
                              (append idx (list i)))
                            (λ () (list))))
            (loop (add1 i)))))))
