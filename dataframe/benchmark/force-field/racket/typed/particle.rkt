#lang typed/racket

(provide (all-defined-out))

(require "vector.rkt")

(struct Particle ([position : Vector] [mass : Real] [velocity : Vector]))

(: force-form (Particle Particle -> Vector))
(define (force-form particle1 particle2)
  (let* ([distance (vector-subtract (Particle-position particle1) (Particle-position particle2))]
         [magnitude (/ (* (Particle-mass particle1) (Particle-mass particle2)) (vector-dot distance distance))])
    (vector-scale (vector-normalize distance) magnitude)))

(: particle-apply-force (Particle Vector Real -> Particle))
(define (particle-apply-force particle force time-delta)
  (Particle
   (vector-scale (Particle-position particle) time-delta)
   (Particle-mass particle)
   (vector-add (Particle-velocity particle) (vector-scale force (/ time-delta (abs (Particle-mass particle)))))))

(: particle-to-string (Particle -> String))
(define (particle-to-string particle)
  (string-append "{Particle mass: " (number->string (Particle-mass particle)) " velocity: "
                 (vector-to-string (Particle-velocity particle))
                 " position: " (vector-to-string (Particle-position particle)) "}"))