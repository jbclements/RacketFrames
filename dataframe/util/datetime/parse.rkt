#lang typed/racket/base

(provide
 is-valid-date?
 is-valid-datetime?
 parse-date
 parse-datetime
 parse-julian-day
 parse-date-string)

(require
 racket/match
 (only-in "convert.rkt"
	  julian-day-number)
 (only-in "types.rkt"	  
	  Instant Instant? 
	  Datetime Datetime? Datetime-date Datetime-time
	  Date Date? Date-day Date-month Date-year
	  Time Time? Time-offset Time-hour Time-minute Time-second Time-milli
	  ))

;; Apply the given procedure to the option value if defined.
(: opt-map (All (a b) (Option a) (a -> b) -> (Option b)))
(define (opt-map opt proc)
  (if opt (proc opt) #f))

(: ddmmyy-rx Regexp)
(define ddmmyy-rx #px"(\\d{1,2})(-|\\/)(\\d{1,2})(-|\\/)(\\d{4}|\\d{2}$)")

(define datetime-rx #px"20\\d{2}((0[1-9])|(1[0-2]))(-|\\/)((0[1-9])|([1-2][0-9])|(3[0-1]))(T|\\s)(([0-1][0-9])|(2[0-3])):([0-5][0-9]):([0-5][0-9])")

(: parse-date-string (String -> (Option (Listof (Option String)))))
(define (parse-date-string str)
  (let ((ds (regexp-match ddmmyy-rx str)))
    (if (pair? ds)
	(cdr ds)
	#f)))

(: parse-datetime-string (String -> (Option (Listof (Option String)))))
(define (parse-datetime-string str)
  (let ((ds (regexp-match datetime-rx str)))
    (if (pair? ds)
	(cdr ds)
	#f)))

(: s->n ((Option String) -> (Option Number)))
(define (s->n ostr)
  (opt-map ostr string->number))

(: parse-julian-day (String -> (Option Integer)))
(define (parse-julian-day str)
  (opt-map (parse-date-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter fixnum? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       (julian-day-number d m y))
		      (_ #f)))))

(: is-valid-date? (String -> Boolean))
(define (is-valid-date? str)
  (opt-map (parse-date-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       #t)
		      (_ #f)))))

(: is-valid-datetime? (String -> Boolean))
(define (is-valid-datetime? str)
  (opt-map (parse-datetime-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons mo (cons d (cons y (cons h (cons mn (cons s _))))))
		       #t)
		      (_ #f)))))

(: parse-date (String -> (Option Date)))
(define (parse-date str)
  (opt-map (parse-date-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       (Date y m d))
		      (_ #f)))))

(: parse-datetime (String -> (Option Datetime)))
(define (parse-datetime str)
  (opt-map (parse-datetime-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons mo (cons d (cons y (cons h (cons mn (cons s _))))))
		       (Datetime (Date y mo d) (Time h mn s 0 0)))
		      (_ #f)))))
