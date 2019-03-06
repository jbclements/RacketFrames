#lang typed/racket/base

(provide
 Line Line?
 LineParser
 ListofString
 ListofString?)

(define-type Line String)
(define-type LineParser (Line -> (Listof String)))

(define-type ListofString (Listof String))

(define Line? string?)

(define ListofString? (lambda ([x : ListofString]) : (U #f ListofString) (if (andmap string? x) x #f)))
