#lang info
(define deps '("typed-racket-lib"))
(define collection 'RacketFrames)
(define version "1.0")

(define compile-omit-paths '("dataframe/tests/"                            
                             "dataframe/benchmark/"
                             "dataframe/thesis-paper/"
                             "dataframe/images/"
                             "dataframe/slides/"
                             "dataframe/validation/"
                             "dataframe/jupyter-notebooks/"
                             "dataframe/sample-csv/"                            
                             "dataframe/util/future-work"))