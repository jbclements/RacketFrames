#lang typed/racket

(provide:
 [determine-schema (FilePath Integer String -> Schema)]
 [load-csv-file (String [#:schema (Option Schema)] -> DataFrame)]
 [load-delimited-file (String String [#:schema (Option Schema)] -> DataFrame)])

(require
 racket/match
 (only-in "../util/list.rkt"
	  zip)
 (only-in "../util/filepath.rkt"
	  FilePath FilePath->string)
 (only-in "schema.rkt"
	  generate-anon-series-names
	  Schema ColumnInfo SeriesTypes Schema-has-headers
	  Schema-SeriesTypes Schema-headers)
 (only-in "../data-frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../data-frame/generic-series-builder.rkt"
	  new-GenSeriesBuilder
	  GenSeriesBuilder
	  GenSeriesBuilder?
	  complete-GenSeriesBuilder)
 (only-in "../data-frame/integer-series-builder.rkt"
	  new-ISeriesBuilder
	  ISeriesBuilder
	  ISeriesBuilder?
	  complete-ISeriesBuilder)
 (only-in "../data-frame/boolean-series-builder.rkt"
	  new-BSeriesBuilder
	  BSeriesBuilder
	  BSeriesBuilder?
	  complete-BSeriesBuilder)
 (only-in "../data-frame/numeric-series-builder.rkt"
	  new-NSeriesBuilder
	  NSeriesBuilder
	  NSeriesBuilder?
	  complete-NSeriesBuilder)
 (only-in "../data-frame/categorical-series-builder.rkt"
	  new-CSeriesBuilder
	  CSeriesBuilder
	  CSeriesBuilder?
	  complete-CSeriesBuilder
	  append-CSeriesBuilder)
 (only-in "../data-frame/series-description.rkt"
	  Series)
 (only-in "../data-frame/data-frame.rkt"
	  DataFrame
	  new-data-frame
          data-frame-explode)
 (only-in "../data-frame/data-frame-print.rkt"
          data-frame-write-tab
          data-frame-head)
 "data-frame-builder.rkt"
 (only-in "delimited-common.rkt"
	  sample-formatted-file
	  check-data-file-exists)
 (only-in "schema.rkt"
	  Schema)
 (only-in "sample.rkt"
	  determine-schema-from-sample)
 (only-in "csv-delimited.rkt"
	  read-csv-file)
 (only-in "delimited.rkt"
	  read-delimited-file))

(: new-DataFrameBuilder-from-Schema (Schema -> DataFrameBuilder))
(define (new-DataFrameBuilder-from-Schema schema)

  (: determine-SeriesBuilder (SeriesTypes -> SeriesBuilder))
  (define (determine-SeriesBuilder stypes)
    (match stypes
      ['GENERIC (new-GenSeriesBuilder)]
      ['CATEGORICAL (new-CSeriesBuilder)]
      ['INTEGER     (new-ISeriesBuilder)]
      ['NUMERIC     (new-NSeriesBuilder)]
      ['BOOLEAN     (new-BSeriesBuilder)]))

  (DataFrameBuilder ((inst map SeriesBuilder SeriesTypes)
		 determine-SeriesBuilder
		 (Schema-SeriesTypes schema))))

(: complete-SeriesBuilders (DataFrameBuilder -> (Listof Series)))
(define (complete-SeriesBuilders frame-builder)
  (map (λ: ((builder : SeriesBuilder))
         (cond
           [(GenSeriesBuilder? builder)
            (complete-GenSeriesBuilder builder)]
           [(CSeriesBuilder? builder)
            (complete-CSeriesBuilder builder)]
           [(ISeriesBuilder? builder)
            (complete-ISeriesBuilder builder)]
           [(BSeriesBuilder? builder)
            (complete-BSeriesBuilder builder)]
           [(NSeriesBuilder? builder)
            (complete-NSeriesBuilder builder)]
           [else (error "Inconsistent DataFrameBuilder")]))
       (DataFrameBuilder-builders frame-builder)))

(: anon-headers (Integer -> (Listof Symbol)))
(define (anon-headers cnt)
  (map string->symbol (generate-anon-series-names cnt)))

(: make-data-frame (Schema DataFrameBuilder -> DataFrame))
(define (make-data-frame schema builder)
  (let ((cols (complete-SeriesBuilders builder)))
    (let ((headers (if (Schema-has-headers schema)
		       (Schema-headers schema)
		       (anon-headers (length cols)))))
      (new-data-frame ((inst zip Symbol Series) headers cols)))))

(: schema-if-needed ((Option Schema) FilePath (Option String) -> Schema))
(define (schema-if-needed schema fpath delim)
  (define SAMPLE-SIZE 20)
  (if schema schema (determine-schema fpath SAMPLE-SIZE (if delim delim ","))))

; delimiter must be specified by user if no schema provided, need to still do this
(: load-csv-file (String [#:schema (Option Schema)] -> DataFrame))
(define (load-csv-file fpath-str #:schema [schema #f])
  (let* ((fpath (FilePath fpath-str))
         (schema (schema-if-needed schema fpath #f)))
    (make-data-frame schema (read-csv-file fpath
				      (Schema-has-headers schema)
				      (new-DataFrameBuilder-from-Schema schema)))))

(: load-delimited-file (String String [#:schema (Option Schema)] -> DataFrame))
(define (load-delimited-file fpath-str delim #:schema [schema #f])
  (let* ((fpath (FilePath fpath-str))
         (schema (schema-if-needed schema fpath delim)))
    (make-data-frame schema (read-delimited-file fpath
                                                 (Schema-has-headers schema)
                                                 (new-DataFrameBuilder-from-Schema schema)
                                                 delim))))

(: determine-schema (FilePath Integer String -> Schema))
(define (determine-schema fpath cnt delim)
  (check-data-file-exists fpath)
  (determine-schema-from-sample (sample-formatted-file fpath cnt) delim))

; test cases

(define salary-schema (Schema #t (list (ColumnInfo 'first 'CATEGORICAL) (ColumnInfo 'last 'CATEGORICAL)
                                       (ColumnInfo 'age 'INTEGER) (ColumnInfo 'dollar 'CATEGORICAL)
                                       (ColumnInfo 'phone 'CATEGORICAL))))

(define random-demographic-schema (Schema #t (list (ColumnInfo 'first 'CATEGORICAL) (ColumnInfo 'last 'CATEGORICAL)
                                                   (ColumnInfo 'gender 'CATEGORICAL) (ColumnInfo 'yn 'CATEGORICAL)
                                                   (ColumnInfo 'char 'GENERIC) (ColumnInfo 'float 'NUMERIC))))

; read csv
(define salary-data-frame-csv-schema (load-csv-file "../sample-csv/salary.csv" #:schema salary-schema))

(data-frame-head salary-data-frame-csv-schema)

(displayln "NO SCHEMA");

; no schema
(define salary-data-frame-csv-no-schema (load-csv-file "../sample-csv/salary.csv" #:schema #f))

(data-frame-head salary-data-frame-csv-no-schema)

; read delimited
(define random-demographic-data-frame-delimited (load-delimited-file "../sample-csv/random_demographic.csv" "|" #:schema random-demographic-schema))

(data-frame-head random-demographic-data-frame-delimited)

;(series-data (data-frame-series random-demographic-data-frame-delimited 'char))

; no schema
(define fruits-data-frame-delimited-no-schema (load-delimited-file "../sample-csv/random_demographic.csv" "|" #:schema #f))

(data-frame-head fruits-data-frame-delimited-no-schema)
