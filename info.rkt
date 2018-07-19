#lang info
(define deps '("typed-racket-lib"))
(define collection 'RacketFrames)
(define version "1.0")

(define compile-omit-paths '("/RacketFrames/dataframe/tests/"                            
                             "/RacketFrames/dataframe/benchmark/"
                             "/RacketFrames/dataframe/thesis-paper/"
                             "/RacketFrames/dataframe/images/"
                             "/RacketFrames/dataframe/slides/"))