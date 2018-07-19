#lang racket

(provide (all-defined-out))

(struct Vector [x y])

(define (vector-substract vec1 vec2)
  (Vector (- (Vector-x vec1) (Vector-x vec2)) (- (Vector-y vec1) (Vector-y vec2))))

(define (vector-add vec1 vec2)
  (Vector (+ (Vector-x vec1) (Vector-x vec2)) (+ (Vector-y vec1) (Vector-y vec2))))

(define (vector-scale vec d)
  (Vector (* (Vector-x vec) d) (* (Vector-y vec) d)))

(define (vector-dot vec1 vec2)
  (+ (* (Vector-x vec1) (Vector-x vec2)) (* (Vector-y vec1) (Vector-y vec2))))

(define (vector-normalize vec)
  (define magnitude (sqrt (vector-dot vec vec)))

  (if (= magnitude 0.0)
      (Vector 0 0)
      (vector-scale vec (/ 1.0 magnitude))))

(define (vector-to-string vec)
  (string-append "(" (number->string (Vector-x vec)) ", " (number->string (Vector-y vec)) ")"))