#lang info

(define collection 'RacketFrames 'multi)
(define version "1.0")

(define deps '("typed-racket-lib"               
               "plot-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))

(define compile-omit-paths '("dataframe/tests/"                            
                             "dataframe/benchmark/"                         
                             "dataframe/validation/"
                             "dataframe/jupyter-notebooks/"
                             "dataframe/sample-csv/"                            
                             "dataframe/util/future-work/"
                             "dataframe/stats/future-work/"))