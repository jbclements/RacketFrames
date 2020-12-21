#lang typed/racket

(provide
 FilePath)

(provide:
 [FilePath->string  (FilePath -> String)])

(struct: FilePath ([p : String])  #:transparent)

(: FilePath->string (FilePath -> String))
(define (FilePath->string fp)
  (FilePath-p fp))
