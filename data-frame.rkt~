#lang racket

(require csv-reading)
(require rackunit)
(require math/statistics)

; ******************************************************
; This function consumes a file-name referencing a CSV
; and parses it making it a 2-Dimensional list with each
; row respresented by a List which is stored as an elemnt
; of the outer list.
;; from-csv : (file-name -> List(List))

(define (from-csv file-name)
      (csv->list (make-csv-reader
                  (open-input-file file-name)
                  '((separator-chars            #\|)
                    (strip-leading-whitespace?  . #t)
                    (strip-trailing-whitespace? . #t)))))

; ******************************************************

; ******************************************************
; This function consumes a hash that maps column names
; to the index in the vector it resides. An empty hash
; is passed in and is recursively built up with the idx
; being incremented.
;; build-column-hash : (column-hash column-names idx -> column-hash)

(define (build-column-hash column-hash column-names idx)
  (cond
    [(empty? column-names) column-hash]
    [else (build-column-hash (begin (hash-set! column-hash (first column-names) idx) column-hash)
                             (rest column-names) (+ idx 1))]))

; ******************************************************

(define (delete-item-at-idx lst idx counter-idx)
  (cond ((null? lst)
         '())
        ((equal? idx counter-idx)
         (cdr lst))
        (else
         (cons (car lst) 
               (delete-item-at-idx (cdr lst) idx (add1 counter-idx))))))

; ******************************************************

; ******************************************************
; This function acts as a constructor for the data-frame
; object. It contains a number of procedures which can
; be invoked a data-store that is constructed either
; from list arguments or by parsing CSV.
;; date-frame : (. args -> column-hash)

(define (data-frame . args)
  ; Declare variables: column-names and data-store. Column names are
  ; either derived from the arguments or parsed from CSV. Same goes
  ; for the data-store.
  (let* ([column-names
          (cond
            [(and (symbol? (first args)) (symbol=? (first args) 'from-csv))          
             (let* ([csv-data (from-csv (second args))]
                    [column-names (list->vector (map first csv-data))])
               column-names)]
          [else (list->vector (map car args))])]
         [data-store
          (cond
            [(and (symbol? (first args)) (symbol=? (first args) 'from-csv))
             (let* ([csv-data (from-csv (second args))]
                    [data-store (list->vector (map (compose list->vector cdr) csv-data))])
               data-store)]
          [else (list->vector (map (compose list->vector cdr) args))])])

    ; Returns all values from row n.
    (define (get-row n)
      (vector-map
       (λ (v) (vector-ref v n))
       data-store))

    ; Returns all values from row start -> end (inclusive) from given data-source.
    (define (get-row-range start end data-source)
      (cond
        [(<= start end)
         (cons (vector-map (λ (v) (vector-ref v start)) data-source)
               (get-row-range (+ start 1) end data-source))]
        [else empty]))

    ; Returns all values from row start -> end (inclusive) but only from particular
    ; columns of data-source.
    (define (get-row-range-cols start end cols)
      (get-row-range start end (list->vector (map get-column-values cols))))

    ; Returns length of data (number of rows).
    (define (get-data-length)
      (vector-length (vector-ref data-store 0)))

    ; Builds column-hash using helper function defined above.
    (define column-hash (build-column-hash (make-hash) (vector->list column-names) 0))

    ; Hashes into column-hash to retrieve index into data-store to retrieve column
    ; values.
    (define (get-column-values column-name)
      (vector-ref data-store (hash-ref column-hash column-name)))

    ; Applies the aggregate function specificed by function-name to the values in
    ; the column-name column. Currently supports 3: sum, avg, count.
    (define (apply-agg function-name column-name)
      (match function-name
        ['sum (apply + (vector->list (get-column-values column-name)))]
        ['avg [let ([column-values (get-column-values column-name)])
              (/ (apply + (vector->list column-values)) (vector-length column-values))]]
        ['count [let ([column-values (get-column-values column-name)])
               (vector-length column-values)]]
        ['median (calc-median column-name)]))

    ;(print (get-column-values 'y))
    
    (define (calc-median column-name)
      (median (vector->list (get-column-values column-name))))

    ; Min function to be applied to a set of values.
    (define (get-min lst)
      (vector-argmin (lambda (x) x) (list->vector lst)))

    ; Max function to be applied to a set of values.
    (define (get-max lst)
      (vector-argmax (lambda (x) x) (list->vector lst)))

    ; Unique function applied to set of values to remove duplicates.
    (define (uniqify lst)
      (cond [(empty? lst)
             '()]
            [(member (first lst) (rest lst))
             (uniqify (rest lst))]
            [else
             (cons (first lst) (uniqify (rest lst)))]))

    ; Applies the aggregate function specificed by function-name to the values in
    ; the column-name column. Currently supports 3: min, max, unique.
    (define (apply-func function-name column-name)
      (match function-name
      ['min (get-min (vector->list (get-column-values column-name)))]
      ['max (get-max (vector->list (get-column-values column-name)))]
      ['unique (uniqify (vector->list (get-column-values column-name)))]))

    ; Formats rows and returns string.
    (define (format-row n)
      (define (default-format-function x)
        (~a x
            #:max-width 8
            #:min-width 8
            #:limit-marker "..."
            #:align 'center
            #:left-pad-string " "
            #:right-pad-string " "))
      
      (let* ([nth-row   (get-row n)]
             [formatted (vector-map default-format-function nth-row)])
        (string-append* (vector->list formatted))))

    ; Retrieves values of first n rows in formatted form.
    (define (format-first-rows n)
      (string-join
       (map format-row (range n))
       "\n"))

    ; Retrieves values of last n rows in formatted form.
    (define (format-last-rows n)
      (string-join
       (map format-row (range n (get-data-length)))
       "\n"))

    ; Formats columns and returns string.
    (define (format-column)
      (define (default-format-function x)
        (~a x
            #:max-width 8
            #:min-width 8
            #:limit-marker "..."
            #:align 'center
            #:left-pad-string " "
            #:right-pad-string " "))

      (let* ([column-formatted (vector-map default-format-function column-names)]
             [column-string (string-append* (vector->list column-formatted))])
        (string-append
         column-string "\n"
         (make-string (string-length column-string) #\―))))

    ; Displays formatted columns.
    (define (show-columns)
      (display (format-column)) (display "\n"))

    ; Shows just the nth row without associated columns of data.
    (define (show-row n)
      (display (format-row n))
      (display "\n"))

    ; Shows the first n rows without associated columns of data. Defaults
    ; to 10 rows for the 'head function.
    (define (show-rows [n 10])
      (display (format-first-rows (min n (get-data-length))))
      (display "\n"))

    (define (show-last-rows [n 10])
      (display (format-last-rows (min n (get-data-length))))
      (display "\n"))

    ; Shows the first n rows along with associated columns of data. Defaults
    ; to 10 rows for the 'head function.
    (define (show-data [n 10])
      (show-columns)
      (show-rows n))

    ; Shows the last n rows along with associated columns of data. Defaults
    ; to 10 rows for the 'head function.
    (define (show-tail-data [n 10])
      (show-columns)
      (show-last-rows (- (get-data-length) n)))

    (define (drop column-name)
      (delete-item-at-idx
       (vector->list data-store)
       (hash-ref column-hash column-name)
       0))

    ; sortBy(column)

    ; dropna()

    ; case-lambda matches on operation being performed on data-frame and performs
    ; the operation using helper functions.
    (case-lambda
      [() (show-data)]
      
      [(op)
       (match op
         ['head (show-data 10)]
         ['tail (show-tail-data 10)]
         ['show-columns (show-columns)]
         ['show (show-data)]
         ['length (get-data-length)]
         ['columns column-names]
         ['data-store data-store]
         )]

      [(op n)
       (match `(,op ,n)
         [(list 'show-row n)
          (show-columns)
          (show-row (sub1 n))]

         [(list 'show 'all)
          (show-data (get-data-length))]
         
         [(list 'head n)
          (show-data n)]

         [(list 'tail n)
          (show-tail-data n)]

         [(list 'agg n)
          (apply-agg n)]

         [(list 'drop c)
          (list->vector (drop c))]
         )]
      
      [(op n1 n2)
       (match `(,op ,n1 ,n2)
         [(list 'agg n1 n2)
          (apply-agg n1 n2)]
         [(list 'func n1 n2)
          (apply-func n1 n2)]
         [(list 'iloc n1 n2)
          (get-row-range n1 n2 data-store)]
         )]

      [(op n1 n2 args)
       (match `(,op ,n1 ,n2 ,args)
         [(list 'iloc n1 n2 (? list? cols))
          (get-row-range-cols n1 n2 cols)]
         )]
    )))

; ******************************************************

; ******************************************************
; Test Cases
; ******************************************************

(define df (data-frame `[x . ,(range 100)] `[y . ,(range 100)]))

(define df2 (data-frame `[x . ,(list 1 1 2 2 3 4 5)] `[y . ,(range 100)]))

; unsure how to match format
(df 'head)

(df 'tail)

(df 'tail 20)

(df 'drop 'y)

;(df 'agg 'median 'y)

(check-equal? (df 'agg 'sum 'x) 4950)

(check-equal? (df 'agg 'avg 'x) 99/2)

(check-equal? (df 'agg 'count 'x) 100)

(check-equal? (df 'func 'min 'y) 0)

(check-equal? (df 'func 'max 'y) 99)

(check-equal? (df2 'func 'unique 'x) '(1 2 3 4 5))

(check-equal? (df 'iloc 0 10)
              '(#(0 0)
                #(1 1)
                #(2 2)
                #(3 3)
                #(4 4)
                #(5 5)
                #(6 6)
                #(7 7)
                #(8 8)
                #(9 9)
                #(10 10)))

; gets rows only from specified columns
(check-equal? (df 'iloc 0 5 '(x y))
              '(#(0 0)
                #(1 1)
                #(2 2)
                #(3 3)
                #(4 4)
                #(5 5)))

;**********
; CSV tests
;**********
(define df-csv (data-frame 'from-csv "fruits.csv"))

; unsure how to match format
(df-csv 'head)

; check if csv is parsed correctly
(check-equal? (from-csv "fruits.csv")
              '(("apples" "2" "0.42")
                ("bananas" "20" "13.69")))

