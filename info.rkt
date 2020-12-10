#lang info
(define collection 'RacketFrames 'multi)
(define version "1.0")
(define deps '("typed-racket-lib"               
               "plot-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

(define compile-omit-paths '("tests/"                            
                             "benchmark/"                         
                             "validation/"
                             "jupyter-notebooks/"
                             "sample-csv/"                            
                             "util/future-work/"
                             "stats/future-work/"))