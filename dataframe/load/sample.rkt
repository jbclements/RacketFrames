#lang typed/racket/base

(provide:
 [determine-schema-from-sample ((Listof String) String -> Schema)])

(require
 (only-in "schema.rkt"
	  Schema ColumnInfo SeriesTypes Schema-headers Schema-SeriesTypes
	  generate-anon-series-names)
 (only-in "delimited.rkt"
	 set-delimiter)
 (only-in "../util/datetime/parse.rkt"
	 is-valid-date? is-valid-datetime? parse-date parse-datetime)
 (only-in "types.rkt"
	  ListofString ListofString?))

(: canonicalize-to-string-or-num ((Listof String) -> (Listof (U Number String))))
(define (canonicalize-to-string-or-num strs)
  (map (λ: ((str : String))
	   (if (string=? (string-upcase str) "N/A")
	       1
	       (let ((num (string->number str)))
		 (if num num str))))
       strs))

;; First line of sample is all strings (non-numeric) assume headers
(: guess-if-headers ((Listof String) -> Boolean))
(define (guess-if-headers fields)
  (andmap string? (canonicalize-to-string-or-num fields)))

(: guess-series-type ((Listof String) -> SeriesTypes))
(define (guess-series-type col)
  (cond
   ((andmap exact-integer? (canonicalize-to-string-or-num col))
    'INTEGER)
   ((andmap number? (canonicalize-to-string-or-num col))
    'NUMERIC)
   ((andmap string? (canonicalize-to-string-or-num col))
    (cond
      [(andmap is-valid-datetime? col) 'DATETIME]
      [(andmap is-valid-date? col) 'DATETIME]
      [else 'CATEGORICAL]))
   (else 'GENERIC)))

(: guess-series-meta ((Listof String) (Listof (Listof String)) -> (Listof ColumnInfo)))
(define (guess-series-meta headers cols)
  (let: loop : (Listof ColumnInfo)
	((headers : (Listof String) headers)
	 (cols : (Listof (Listof String)) cols)
	 (meta : (Listof ColumnInfo) '()))
	(if (null? headers)
	    (reverse meta)
	    (loop (cdr headers)
		  (cdr cols)
		  (cons (ColumnInfo (string->symbol(car headers))
				    (guess-series-type (car cols)))
			meta)))))

(: transpose-rows-to-cols ((Listof (Listof String)) -> (Listof (Listof String))))
(define (transpose-rows-to-cols rows)

  (: transpose-row-to-col ((Listof String) (Listof (Listof String)) -> (Listof (Listof String))))
  (define (transpose-row-to-col row cols)
    (let: loop : (Listof (Listof String)) ((row : (Listof String) row)
					   (cols : (Listof (Listof String)) cols)
					   (accum : (Listof (Listof String)) '()))
	  (if (null? row)
	      (reverse accum)
	      (let ((elem (car row)) (col (car cols)))
		(loop (cdr row) (cdr cols) (cons (cons elem col) accum))))))

  (: create-accumulator ((Listof String) -> (Listof (Listof String))))
  (define (create-accumulator protos)
    (let: loop : (Listof (Listof String))
	  ((protos : (Listof String) protos)
	   (accum : (Listof (Listof String)) '()))
	  (if (null? protos)
	      accum
	      (loop (cdr protos) (cons '() accum)))))

  (: deep-reverse ((Listof (Listof String)) -> (Listof (Listof String))))
  (define (deep-reverse cols)
    (let: loop : (Listof (Listof String))
	  ((cols : (Listof (Listof String)) cols)
	   (accum : (Listof (Listof String)) '()))
	  (if (null? cols)
	      (reverse accum)
	      (loop (cdr cols) (cons (reverse (car cols)) accum)))))

  (if (null? rows)
      '()
      (let: loop : (Listof (Listof String))
	    ((rows : (Listof (Listof String)) rows)
	     (cols : (Listof (Listof String)) (create-accumulator (car rows))))
	    (if (null? rows)
		(deep-reverse cols)
		(loop (cdr rows) (transpose-row-to-col (car rows) cols))))))

; This function takes in a list of parsed lines with delimeter from a delimited file
; and determines the Schema from that.
(: determine-schema-from-sample ((Listof String) String -> Schema))
(define (determine-schema-from-sample lines delim)

  (: sample-length ((Listof String) -> Integer))
  (define (sample-length sample)
    (length sample))

  (: check-consistent-fields ((Listof (Listof String)) -> Void))
  (define (check-consistent-fields samples)
    (when (pair? samples)
	  (let ((l0 (sample-length (car samples))))
	    (unless (andmap (λ: ((sample : (Listof String)))
				(eq? (sample-length sample) l0))
			    samples)
		    (error "Sampling of data file indicates lines of varying field length")))))

  (if (null? lines)
      (Schema #f '())
      (let ((samples (map (set-delimiter delim) lines)))
	(check-consistent-fields samples)
	(let ((headers? (guess-if-headers (car samples))))
	  (let ((headers (if headers?
			     (car samples)
			     (generate-anon-series-names (length (car samples)))))
		(cols (if headers?
                          (transpose-rows-to-cols (cdr samples))
                          (transpose-rows-to-cols samples))))
	    (Schema headers? (guess-series-meta headers cols)))))))

(define schema-1 (determine-schema-from-sample (list "header1, header2, header3, header4" "hello, world, fizz, buzz") ","))
(Schema-headers schema-1)

(Schema-SeriesTypes schema-1)

; this function gets the string split on the delimter (Listof (Listof String)) 
;(transpose-rows-to-cols (list (list "header1" "header2" "header3" "header4") (list "hello" "world" "fizz" "buzz") (list "hello" "world" "fizz" "buzz") (list "hello, world, fizz, buzz") (list "hello, world, fizz, buzz") (list "hello" "world" "fizz" "buzz")))

;(transpose-rows-to-cols (list (list "hello" "world") (list "fizz" "buzz")))

; (list "first|last|gender|yn|char|float" "Louis|Lawson|Male|Y|z|-320102186614.784") needs to become
; (list (list "first" "Louis") (list "last" "Lawson") etc.)

(determine-schema-from-sample (list "first|last|gender|yn|char|float" "Louis|Lawson|Male|Y|z|-320102186614.784") "|")

(define schema-2 (determine-schema-from-sample (list "5.7" "generic header" "world" "fizz" "buzz" "1" "2.5") ","))

(Schema-headers schema-2)

(Schema-SeriesTypes schema-2)

(define schema-3 (determine-schema-from-sample (list "generic header" "5.7" "world" "fizz" "buzz" "1" "2.5") ","))

(Schema-headers schema-3)

(Schema-SeriesTypes schema-3)

(define schema-4 (determine-schema-from-sample (list "Numeric Header" "5.7" "2.5") ","))

(Schema-headers schema-4)

(Schema-SeriesTypes schema-4)
