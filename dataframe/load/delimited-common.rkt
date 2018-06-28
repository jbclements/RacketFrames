#lang typed/racket

(provide:
 [check-data-file-exists (FilePath -> Void)]
 [sample-formatted-file (FilePath Integer -> (Listof String))]
 [data-frame-builder-tank (DataFrameBuilder LineParser -> (Tank Line DataFrameBuilder))]
 [read-formatted-file (FilePath Boolean (Tank String DataFrameBuilder) -> DataFrameBuilder)])

(require
 (only-in "../util/filepath.rkt"
	  FilePath FilePath->string)
 (only-in pipe
	  Tank Stream Continue Done
	  pump/text-input-port
	  drain head-n)
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

(: data-frame-builder-tank (DataFrameBuilder LineParser -> (Tank Line DataFrameBuilder)))
(define (data-frame-builder-tank data-frame-builder line-parser)

  (: appenders (Listof (Line -> Void)))
  (define appenders (map (λ: ((builder : SeriesBuilder))
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
                               [else (λ: ((str : String)) (void))]))
                         (DataFrameBuilder-builders data-frame-builder)))

  #| (define-type (Stream D) (U D 'Nothing 'EOS))

     (define-type (Tank D A) (U (Done D A) (Continue D A)))

     (struct: (D A) Done ([stream : (Stream D)]
                     [accum : A]))

     (struct: (D A) Continue ([step : ((Stream D) -> (Tank D A))])) |#

  (: step ((Stream String) -> (Tank String DataFrameBuilder)))
  (define (step input)
    (cond
     ((Line? input)
      (append-data-fields appenders (line-parser input))
      (Continue step))
     ((eq? input 'EOS)
      (Done 'EOS data-frame-builder))
     ((eq? input 'Nothing)
      (Continue step))))

  (Continue step))

(: check-data-file-exists (FilePath -> Void))
(define (check-data-file-exists fpath)
  (unless (file-exists? (FilePath->string fpath))
	  (error (format "File not found: ~s" (FilePath->string fpath)))))

(: read-formatted-file (FilePath Boolean (Tank String DataFrameBuilder) -> DataFrameBuilder))
(define (read-formatted-file fpath headers frame-tank)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (call-with-input-file*
     fpath
     (λ: ((inp : Input-Port))
	 (when headers (read-line inp))
	 (drain (((inst pump/text-input-port DataFrameBuilder) inp) frame-tank))))))

(: sample-formatted-file (FilePath Integer -> (Listof String)))
(define (sample-formatted-file fpath cnt)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (call-with-input-file*
     fpath
     (λ: ((inp : Input-Port))
	 (drain (((inst pump/text-input-port (Listof String)) inp)
		 ((inst head-n String) cnt)))))))
