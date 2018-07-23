#lang typed/racket

(require "particle.rkt")
(require "vector.rkt")

(: args (Vectorof String))
(define args (current-command-line-arguments))

(when (not (= (vector-length args) 2))
    (error "Usage: racket main.rkt <start> <end>"))

(: time-delta Real)
(define time-delta (assert (string->number (vector-ref args 0)) real?))

(: count Integer)
(define count (assert (string->number (vector-ref args 1)) exact-integer?))


(: particle1 Particle)
(define particle1 (Particle (Vector 0.3 0) -1 (Vector 0 0)))

(: particle2 Particle)
(define particle2 (Particle (Vector -0.3 0) 1 (Vector 0 0)))

(: main (-> Void))
(define (main)
  (: force1 Vector)
  (define force1 (Vector 0 0))
  (: force2 Vector)
  (define force2 (Vector 0 0))
  
  (for ([i (range count)])
    (set! force1 (force-form particle1 particle2))
    (set! force2 (force-form particle2 particle1))

    (set! particle1 (particle-apply-force particle1 force1 time-delta))
    (set! particle2 (particle-apply-force particle2 force2 time-delta)))

  (displayln (particle-to-string particle1))
  (displayln (particle-to-string particle2)))

(main)
