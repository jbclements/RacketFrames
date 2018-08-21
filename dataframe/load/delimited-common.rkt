#lang typed/racket

(provide:
 [check-data-file-exists (FilePath -> Void)]
 [sample-formatted-file (FilePath Integer -> (Listof String))]
 [data-frame-builder-appenders (DataFrameBuilder -> (Listof (Line -> Void)))]
 [read-formatted-file (FilePath Boolean DataFrameBuilder LineParser -> (Listof Boolean))])

(require
 (only-in "../util/filepath.rkt"
	  FilePath FilePath->string)
 (only-in "../data-frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../data-frame/generic-series-builder.rkt"
	  GenSeriesBuilder GenSeriesBuilder?
	  append-GenSeriesBuilder)
 (only-in "../data-frame/categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder)
 (only-in "../data-frame/integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder)
 (only-in "../data-frame/boolean-series-builder.rkt"
	  BSeriesBuilder BSeriesBuilder?
	  append-BSeriesBuilder)
 (only-in "../data-frame/numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder)
 (only-in "data-frame-builder.rkt"	  
	  DataFrameBuilder DataFrameBuilder-builders
	  append-data-fields)
 (only-in "schema.rkt"
	  Schema)
 (only-in "types.rkt"
	  LineParser Line Line?))

(: data-frame-builder-appenders (DataFrameBuilder -> (Listof (Line -> Void))))
(define (data-frame-builder-appenders data-frame-builder)
  (map (λ: ((builder : SeriesBuilder))
         (cond
           [(GenSeriesBuilder? builder)
            (λ: ((str : String))
              (append-GenSeriesBuilder builder str))]
           [(CSeriesBuilder? builder)
            (λ: ((str : String))
              (append-CSeriesBuilder builder str))]
           [(ISeriesBuilder? builder)
            (λ: ((str : String))
              (append-ISeriesBuilder builder str))]
           [(BSeriesBuilder? builder)
            (λ: ((str : String))
              (append-BSeriesBuilder builder str))]
           [(NSeriesBuilder? builder)
            (λ: ((str : String))
              (append-NSeriesBuilder builder str))]
           [(DatetimeSeriesBuilder? builder)
            (append-DatetimeSeriesBuilder builder str)]
           [else (λ: ((str : String)) (void))]))
       (DataFrameBuilder-builders data-frame-builder)))  

(: check-data-file-exists (FilePath -> Void))
(define (check-data-file-exists fpath)
  (unless (file-exists? (FilePath->string fpath))
	  (error (format "File not found: ~s" (FilePath->string fpath)))))

(: read-formatted-file (FilePath Boolean DataFrameBuilder LineParser -> (Listof Boolean)))
(define (read-formatted-file fpath headers data-frame-builder line-parser)
  (check-data-file-exists fpath)
  (let* ((fpath (FilePath->string fpath))
         (file-lines (file->lines fpath)))
    (map (λ: ((line : Line))
           (append-data-fields (data-frame-builder-appenders data-frame-builder) (line-parser line))) file-lines)))

(: sample-formatted-file (FilePath Integer -> (Listof String)))
(define (sample-formatted-file fpath cnt)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (take (file->lines fpath) cnt)))
