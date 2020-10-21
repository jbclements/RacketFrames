#lang info

(define scribblings '(("documentation/dataframe.scrbl" ())))

(define compile-omit-paths '("tests/"                            
                             "benchmark/"
                             "thesis-paper/"
                             "images/"
                             "slides/"
                             "validation/"
                             "jupyter-notebooks/"
                             "sample-csv/"
                             "data-frame/categorical-series.rkt"
                             "util/plot-datetime.rkt"
                             "util/math.rkt"                           
                             "stats/tabulate.rkt"))