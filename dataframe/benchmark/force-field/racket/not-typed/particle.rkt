#lang racket

(provide (all-defined-out))

(require "vector.rkt")

(struct Particle [position mass velocity])

(define (force-form particle1 particle2)
  (let* ([distance (vector-substract (Particle-position particle1) (Particle-position particle2))]
         [magnitude (/ (* (Particle-mass particle1) (Particle-mass particle2)) (vector-dot distance distance))])
    (vector-scale (vector-normalize distance) magnitude)))

(define (particle-apply-force particle force time-delta)
  (Particle
   (vector-scale (Particle-position particle) time-delta)
   (Particle-mass particle)
   (vector-add (Particle-velocity particle) (vector-scale force (/ time-delta (abs (Particle-mass particle)))))))
            
(define (particle-to-string particle)
  (string-append "{Particle mass: " (number->string (Particle-mass particle)) " velocity: "
                 (vector-to-string (Particle-velocity particle))
                 " position: " (vector-to-string (Particle-position particle)) "}"))