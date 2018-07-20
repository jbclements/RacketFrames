#lang typed/racket

(provide (all-defined-out))

(struct Vector ([x : Real] [y : Real]))

(: vector-subtract (Vector Vector -> Vector))
(define (vector-subtract vec1 vec2)
  (Vector (- (Vector-x vec1) (Vector-x vec2)) (- (Vector-y vec1) (Vector-y vec2))))

(: vector-add (Vector Vector -> Vector))
(define (vector-add vec1 vec2)
  (Vector (+ (Vector-x vec1) (Vector-x vec2)) (+ (Vector-y vec1) (Vector-y vec2))))

(: vector-scale (Vector Real -> Vector))
(define (vector-scale vec d)
  (Vector (* (Vector-x vec) d) (* (Vector-y vec) d)))

(: vector-dot (Vector Vector -> Real))
(define (vector-dot vec1 vec2)
  (+ (* (Vector-x vec1) (Vector-x vec2)) (* (Vector-y vec1) (Vector-y vec2))))

(: vector-normalize (Vector -> Vector))
(define (vector-normalize vec)
  (define magnitude (assert (sqrt (vector-dot vec vec)) real?))

  (if (= magnitude 0.0)
      (Vector 0 0)
      (vector-scale vec (/ 1.0 magnitude))))

(: vector-to-string (Vector -> String))
(define (vector-to-string vec)
  (string-append "(" (number->string (Vector-x vec)) ", " (number->string (Vector-y vec)) ")"))