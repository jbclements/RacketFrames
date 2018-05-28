#lang typed/racket

(provide
 MIndex
 build-multi-index-from-list)
 ;build-multi-index-from-cols)

(define-type GenericType Any)

(define-type Key String)

(define-predicate Key? String)

(define-type MLabeling (Listof (Pair Key (Listof Index))))

(define-type MIndex (HashTable Key (Listof Index)))

(define key-delimiter "\t")

; This function consumes a Listof IndexableSeries and builds key
; string from the columns of a frame and a given set of col labels to use.
; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...
(: key-fn-list ((Listof (Listof GenericType)) -> (Index -> String)))
(define (key-fn-list lsts)
  (λ: ((row-id : Index))
    (let ((outp (open-output-string)))
      (for ([lst (in-list lsts)])
        (let*: ((seg : GenericType (list-ref lst row-id))
                (seg-str : String (cond
                                    [(symbol? seg) (symbol->string seg)]
                                    [(number? seg) (number->string seg)]
                                    ; pretty-format anything else
                                    [else (pretty-format seg)])))
          (display seg-str outp)
          (display key-delimiter outp)))
      (get-output-string outp))))

; This function consumes a Listof Listof Label and creates
; a MultiIndex.
(: build-multi-index-from-list ((Listof (Listof GenericType)) -> MIndex))
(define (build-multi-index-from-list nested-label-lst)

  ; Get length of one of the IndexableSeries
  (define len (length (car nested-label-lst)))
  (define: series-key : (Index -> String) (key-fn-list nested-label-lst))

  (let ((index : MIndex (make-hash '())))

    (let loop ([i 0])
      (if (>= i len)
          index
          (let: ((i : Index (assert i index?)))
            (let ((key (series-key i)))
              (hash-update! index key
                            (λ: ((idx : (Listof Index)))
                              (append idx (list i)))
                            (λ () (list))))
            (loop (add1 i)))))))

#|(: build-multi-index-from-cols ((Listof IndexableSeries) -> MIndex))
(define (build-multi-index-from-cols cols)

  ; Get length of one of the IndexableSeries
  (define len (series-length (car cols)))
  (define: series-key : (Index -> String) (key-fn cols))

  (let ((index : MIndex (make-hash '())))
    (let loop ([i 0])
      (if (>= i len)
          index
          (let: ((i : Index (assert i index?)))
            (let ((key (series-key i)))
              (hash-update! index key
                            (λ: ((idx : (Listof Index)))
                              (append idx (list i)))
                            (λ () (list))))
            (loop (add1 i))))))) |#

(build-multi-index-from-list (list (list 'a 'b 'c) (list 1 2 3)))

(build-multi-index-from-list (list (list 'a 'b 'c 'a 'c) (list 1 2 3 4 3)))


