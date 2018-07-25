#lang typed/racket/base

(provide
 parse-date
 parse-date-dashed
 parse-julian-day
 parse-date-string)

(require
 racket/match
 (only-in "convert.rkt"
	  julian-day-number)
 (only-in "types.rkt"
	  Date))

;; Apply the given procedure to the option value if defined.
(: opt-map (All (a b) (Option a) (a -> b) -> (Option b)))
(define (opt-map opt proc)
  (if opt (proc opt) #f))

(: ddmmyy-rx Regexp)
(define ddmmyy-rx #px"(\\d{1,2})/(\\d{1,2})/(\\d{4}|\\d{2}$)")

(define ddmmyy-rx-dashed #px"(\\d{1,2})-(\\d{1,2})-(\\d{4}|\\d{2}$)")

(: parse-date-string (String -> (Option (Listof (Option String)))))
(define (parse-date-string str)
  (let ((ds (regexp-match ddmmyy-rx str)))
    (if (pair? ds)
	(cdr ds)
	#f)))

(: parse-date-string-dashed (String -> (Option (Listof (Option String)))))
(define (parse-date-string-dashed str)
  (let ((ds (regexp-match ddmmyy-rx-dashed str)))
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

(: parse-date (String -> (Option Date)))
(define (parse-date str)
  (opt-map (parse-date-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       (Date y m d))
		      (_ #f)))))

(: parse-date-dashed (String -> (Option Date)))
(define (parse-date-dashed str)
  (opt-map (parse-date-string-dashed str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       (Date y m d))
		      (_ #f)))))
