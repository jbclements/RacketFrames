#lang setup/infotab

(define collection 'multi)
(define version "1.0")

(define deps '("typed-racket-lib"               
               "plot-lib"))

(define build-deps '("racket-doc"
                     "scribble-lib"))

#|
(define compile-omit-paths '("dataframe/tests/"                            
                             "dataframe/benchmark/"                         
                             "dataframe/validation/"
                             "dataframe/jupyter-notebooks/"
                             "dataframe/sample-csv/"                            
                             "dataframe/util/future-work/"
                             "dataframe/stats/future-work/"))
|#
(define pkg-desc "RacketFrames: A DataFrame implemenation in Racket.")

(define pkg-authors '(skahal))