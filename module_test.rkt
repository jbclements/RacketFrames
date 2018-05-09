#lang typed/racket

(require RacketFrames)

;******************
;data-frame-mix
;******************
(define columns-mix
  (list
   (cons 'integer-col (new-ISeries (vector 1 2 3 4)
                            (build-index-from-labels (list 'a 'b 'c 'd))))
   (cons 'categorical-col (new-CSeries (vector 'hello 'world 'fizz 'buzz)))))

; create new data-frame-mix
(define data-frame-mix (new-data-frame columns-mix))

(frame-write-tab data-frame-mix (current-output-port))

; no schema
(define salary-data-frame-csv-no-schema (load-csv-file "sample-csv/salary.csv" #:schema #f #:delim ","))

(data-frame-head salary-data-frame-csv-no-schema (current-output-port))

(displayln "DataFrame List of Column Names")
(data-frame-names salary-data-frame-csv-no-schema)

(displayln "DataFrame Dimensions")
(data-frame-dim salary-data-frame-csv-no-schema)

(displayln "DataFrame Description")
(show-data-frame-description (data-frame-description salary-data-frame-csv-no-schema))

(displayln "DataFrame Remove")
(data-frame-head (data-frame-remove salary-data-frame-csv-no-schema (list 'first 'age)))

(displayln "DataFrame Project")
(data-frame-head (data-frame-project salary-data-frame-csv-no-schema (list 'first 'last 'dollar)))

(displayln "DataFrame Replace")
(data-frame-head (data-frame-replace salary-data-frame-csv-no-schema (cons 'salary (new-CSeries (make-vector 200 '$0.00)))))

(displayln "DataFrame Replace Non Existent Column")
(data-frame-head (data-frame-replace salary-data-frame-csv-no-schema (cons 'dollar (new-CSeries (make-vector 200 '$0.00)))))

(displayln "DataFrame Extend")
(data-frame-head (data-frame-extend salary-data-frame-csv-no-schema (cons 'state (new-CSeries (make-vector 200 'CA)))))

(displayln "DataFrame Operations")
(define columns-integer-1
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-integer-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
   (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
   (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

; create new data-frame-integer-1
(define data-frame-integer-1 (new-data-frame columns-integer-1))

; create new data-frame-integer-2
(define data-frame-integer-2 (new-data-frame columns-integer-2))

(displayln "data-frame+ Test 1")

(frame-write-tab data-frame-integer-1 (current-output-port))

(frame-write-tab data-frame-integer-2 (current-output-port))

(frame-write-tab (data-frame+ data-frame-integer-1 data-frame-integer-2) (current-output-port))