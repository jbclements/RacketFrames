#lang info
(define deps '("typed-racket-lib"               
               "plot-lib"))
(define collection 'RacketFrames)
(define version "1.0")

(define compile-omit-paths '("dataframe/tests/"                            
                             "dataframe/benchmark/"                         
                             "dataframe/validation/"
                             "dataframe/jupyter-notebooks/"
                             "dataframe/sample-csv/"                            
                             "dataframe/util/future-work/"
                             "dataframe/stats/future-work/"))