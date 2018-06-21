#lang typed/racket/base

(provide:
 [set-delimiter (String -> LineParser)]
 [read-delimited-file (FilePath Boolean DataFrameBuilder String -> DataFrameBuilder)])

(require
 (only-in grip/system/filepath
	  FilePath)
 (only-in "types.rkt"
	  LineParser)
 (only-in "delimited-common.rkt"
	  data-frame-builder-tank
	  read-formatted-file)
 (only-in "data-frame-builder.rkt"
	  DataFrameBuilder))

(require/typed racket/string
	       [string-trim (String -> String)]
	       [string-split (String (U String Regexp) -> (Listof String))])

(: set-delimiter (String -> LineParser))
(define (set-delimiter delim)
  (lambda ([str : String])
    (map string-trim (string-split str delim))))

(: read-delimited-file (FilePath Boolean DataFrameBuilder String -> DataFrameBuilder))
(define (read-delimited-file fpath header? data-frame-builder delim)
  (read-formatted-file fpath header? 
		       (data-frame-builder-tank data-frame-builder (set-delimiter delim))))