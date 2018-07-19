#lang racket

(require "particle.rkt")
(require "vector.rkt")

(define args (current-command-line-arguments))

(when (not (= (vector-length args) 2))
    (error "UsageL racket rolling-sum.rkt <start> <end>"))

(define time-delta (string->number (vector-ref args 0)))

(define count (string->number (vector-ref args 1)))

(define particle1 (Particle (Vector 0.3 0) -1 (Vector 0 0)))

(define particle2 (Particle (Vector -0.3 0) 1 (Vector 0 0)))

(define (main)
  (define force1 (Vector 0 0))
  (define force2 (Vector 0 0))
  
  (for ([i (range count)])
    (set! force1 (force-form particle1 particle2))
    (set! force2 (force-form particle2 particle1))

    (set! particle1 (particle-apply-force particle1 force1 time-delta))
    (set! particle2 (particle-apply-force particle2 force2 time-delta)))

  (displayln (particle-to-string particle1))
  (displayln (particle-to-string particle2)))
