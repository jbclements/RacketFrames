#lang typed/racket/base

(provide
 Tic InstantMaker
 (struct-out Interval)
 (struct-out Instant)
 (struct-out InstantTAI)
 (struct-out InstantUTC)
 (struct-out JulianDay)
 (struct-out ModifiedJulianDay)
 (struct-out Date)
 (struct-out Time)
 (struct-out Datetime)
 Datetime? tic?)

(provide:
 [rf-date-year (Date -> Integer)]
 [rf-date-month (Date -> Integer)]
 [rf-date-day (Date -> Integer)]
 [time-offset (Time -> Integer)]
 [time-hour (Time -> Integer)]
 [time-minute (Time -> Integer)]
 [time-second (Time -> Integer)]
 [time-milli (Time -> Integer)]
 [datetime-date (Datetime -> Date)]
 [datetime-time (Datetime -> Time)])

;; 1 Tic == 1 millisecond.
;; Instant in time in millis along with a Chronometry.
;; Assumes a 64 bit build of Racket for the long haul.
(define-type Tic Integer)
(define-predicate tic? Tic)
(define-type Chronometry (U 'TAI 'UTC))
(define-type (InstantMaker X) (Tic -> X))

(struct: Instant ([tics : Tic]) #:transparent)
(struct: InstantTAI Instant () #:transparent)
(struct: InstantUTC Instant () #:transparent)

(struct: ChronometricInstant Instant ([chronometry : Chronometry]) #:transparent)

(struct: Interval ([tics : Tic]) #:transparent)

(struct: JulianDay ([day : Exact-Rational]) #:transparent)
(struct: ModifiedJulianDay ([day : Exact-Rational]) #:transparent)

(struct: Date ([year   : Integer]
	       [month  : Integer]
	       [day    : Integer]) #:transparent)

(struct: Time ([offset : Integer]
	       [hour   : Integer]
	       [minute : Integer]
	       [second : Integer]
	       [milli  : Integer]) #:transparent)

(struct: Datetime ([date : Date]
		   [time : Time]) #:transparent)


; need to change namespace to use date package
(: rf-date-year (Date -> Integer))
(define (rf-date-year date)
  (Date-year date))

(: rf-date-month (Date -> Integer))
(define (rf-date-month date)
  (Date-month date))

(: rf-date-day (Date -> Integer))
(define (rf-date-day date)
  (Date-day date))

(: time-offset (Time -> Integer))
(define (time-offset time)
  (Time-offset time))

(: time-hour (Time -> Integer))
(define (time-hour time)
  (Time-hour time))

(: time-minute (Time -> Integer))
(define (time-minute time)
  (Time-minute time))

(: time-second (Time -> Integer))
(define (time-second time)
  (Time-second time))

(: time-milli (Time -> Integer))
(define (time-milli time)
  (Time-milli time))

; This function consumes a Datetime and returns its
; date.
(: datetime-date (Datetime -> Date))
(define (datetime-date datetime)
  (Datetime-date datetime))

; This function consumes a Datetime and returns its
; time.
(: datetime-time (Datetime -> Time))
(define (datetime-time datetime)
  (Datetime-time datetime))

;; (define-type Tics Integer)
;; (define-type (ICtor X) (Tics -> X))
;; (struct: I ([millis : Tics]))
;; (struct: TAI I ())
;; (struct: UTC I ())
;; (struct: Interval I ())

;; (: future-instant (All (X) I Interval (ICtor X) -> X))
;; (define (future-instant i d c)
;;   (c (+ (I-millis i) (I-millis d))))


;; (define: f-tai : TAI (future-instant (TAI 0) (Interval 100) TAI))
