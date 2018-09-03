#lang typed/racket

(require plot)

#| data : DataFrame

x : label or position, default None

y : label or position, default None

Allows plotting of one column versus another

kind : str

‘line’ : line plot (default)
‘bar’ : vertical bar plot
‘barh’ : horizontal bar plot
‘hist’ : histogram
‘box’ : boxplot
‘kde’ : Kernel Density Estimation plot
‘density’ : same as ‘kde’
‘area’ : area plot
‘pie’ : pie plot
‘scatter’ : scatter plot
‘hexbin’ : hexbin plot
ax : matplotlib axes object, default None

subplots : boolean, default False

Make separate subplots for each column

sharex : boolean, default True if ax is None else False

In case subplots=True, share x axis and set some x axis labels to invisible; defaults to True if ax is None otherwise False if an ax is passed in; Be aware, that passing in both an ax and sharex=True will alter all x axis labels for all axis in a figure!

sharey : boolean, default False

In case subplots=True, share y axis and set some y axis labels to invisible

layout : tuple (optional)

(rows, columns) for the layout of subplots

figsize : a tuple (width, height) in inches

use_index : boolean, default True

Use index as ticks for x axis

title : string or list

Title to use for the plot. If a string is passed, print the string at the top of the figure. If a list is passed and subplots is True, print each item in the list above the corresponding subplot.

grid : boolean, default None (matlab style default)

Axis grid lines

legend : False/True/’reverse’

Place legend on axis subplots

style : list or dict

matplotlib line style per column

logx : boolean, default False

Use log scaling on x axis

logy : boolean, default False

Use log scaling on y axis

loglog : boolean, default False

Use log scaling on both x and y axes

xticks : sequence

Values to use for the xticks

yticks : sequence

Values to use for the yticks

xlim : 2-tuple/list

ylim : 2-tuple/list

rot : int, default None

Rotation for ticks (xticks for vertical, yticks for horizontal plots)

fontsize : int, default None

Font size for xticks and yticks

colormap : str or matplotlib colormap object, default None

Colormap to select colors from. If string, load colormap with that name from matplotlib.

colorbar : boolean, optional

If True, plot colorbar (only relevant for ‘scatter’ and ‘hexbin’ plots)

position : float

Specify relative alignments for bar plot layout. From 0 (left/bottom-end) to 1 (right/top-end). Default is 0.5 (center)

table : boolean, Series or DataFrame, default False

If True, draw a table using the data in the DataFrame and the data will be transposed to meet matplotlib’s default layout. If a Series or DataFrame is passed, use passed data to draw a table.

yerr : DataFrame, Series, array-like, dict and str

See Plotting with Error Bars for detail.

xerr : same types as yerr.

stacked : boolean, default False in line and

bar plots, and True in area plot. If True, create stacked plot.

sort_columns : boolean, default False

Sort column names to determine plot ordering

secondary_y : boolean or sequence, default False

Whether to plot on the secondary y-axis If a list/tuple, which columns to plot on secondary y-axis

mark_right : boolean, default True

When using a secondary_y axis, automatically mark the column labels with “(right)” in the legend

kwds : keywords

Options to pass to matplotlib plotting method |#

(require
 racket/pretty
 racket/unsafe/ops
 racket/flonum
 racket/set
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in "../util/symbol.rkt"
	  symbol-prefix)
 (only-in "../data-frame/indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "../data-frame/series.rkt"
	  series-complete)
 (only-in "../data-frame/series-description.rkt"
	  SeriesType Series Series? SeriesList SeriesList?
	  SeriesDescription-type
	  series-iref series-type series-length
          series-data)
 (only-in "../data-frame/data-frame.rkt"
	  DataFrame DataFrame? Column Columns Columns? Column? new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode column-series
	  DataFrameDescription DataFrameDescription-series data-frame-description)
 (only-in "../data-frame/generic-series.rkt"
	  GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
	  gen-series-referencer)
 (only-in "../data-frame/numeric-series.rkt"
	  NSeries NSeries? nseries-iref nseries-label-ref new-NSeries)
 (only-in "../data-frame/integer-series.rkt"
	  ISeries ISeries? iseries-iref new-ISeries
	  iseries-referencer)
 (only-in "../data-frame/categorical-series.rkt"
	  cseries-referencer cseries-length cseries-iref
	  CSeries CSeries? new-CSeries)
 (only-in "../data-frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../data-frame/generic-series-builder.rkt"
	  GenSeriesBuilder GenSeriesBuilder?
	  append-GenSeriesBuilder complete-GenSeriesBuilder
	  new-GenSeriesBuilder)
 (only-in "../data-frame/integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "../data-frame/categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "../data-frame/categorical-series-ops.rkt"
	  cseries-append)
 (only-in "../data-frame/numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder)
 (only-in "../data-frame/data-frame-print.rkt"
          data-frame-write-tab)
  (only-in "../util/datetime/types.rkt"
          Datetime))


#|
#:x-min [x-min -10]
 	 	#:x-max [x-max 10]
 	 	#:y-min [y-min 10]
 	 	#:y-max y-max	 	 	 	 
 	 	#:sym sym	 	 	 	 
 	 	#:color color	 	 	 	 
 	 	#:fill-color fill-color	 	 	 	 
 	 	#:x-jitter x-jitter	 	 	 	 
 	 	#:y-jitter y-jitter	 	 	 	 
 	 	#:size size	 	 	 	 
 	 	#:line-width line-width	 	 	 	 
 	 	#:alpha alpha	 	 	 	 
 	 	#:label label |#

; ***********************************************************

(: is-plottable-series (Series -> Boolean))
(define (is-plottable-series series)
  (or (GenSeries? series) (ISeries? series) (NSeries? series)))

(: get-series-point-sequence (Series -> (Listof (Listof Real))))
(define (get-series-point-sequence series)

  (when (or (eq? (series-type series) 'CategoricalSeries) (eq? (series-type series) 'BooleanSeries))
    (error 'get-series-point-sequence "Invalid series to plot."))
  
  (for/list: : (Listof (Listof Real))
    ([idx : Real (range (series-length series))])
    (list (add1 idx)
          (assert (car (assert (series-iref series (assert idx index?)) list?)) real?))))

(: get-column-point-sequence (Column -> (Listof (Listof Real))))
(define (get-column-point-sequence column)
  (get-series-point-sequence (column-series column)))

(: get-series-to-series-point-sequence (Series Series -> (Listof (Listof Real))))
(define (get-series-to-series-point-sequence series-1 series-2)

  (when (or
         (or (eq? (series-type series-1) 'CategoricalSeries) (eq? (series-type series-1) 'BooleanSeries))
         (or (eq? (series-type series-2) 'CategoricalSeries) (eq? (series-type series-2) 'BooleanSeries)))
    (error 'get-series-point-sequence "Invalid series to plot."))
  
  (for/list: : (Listof (Listof Real))
    ([idx : Real (range (series-length series-1))])
    (list (assert (car (assert (series-iref series-1 (assert idx index?)) list?)) real?)
          (assert (car (assert (series-iref series-2 (assert idx index?)) list?)) real?))))

(: get-data-frame-point-sequence (DataFrame -> (Listof (Listof (Listof Real)))))
(define (get-data-frame-point-sequence data-frame)
  (let* ((data-frame-exploded (data-frame-explode data-frame))
         (x-series (cdr (car data-frame-exploded)))
         (y-series (map column-series (cdr data-frame-exploded))))
    (map (lambda ([y : Series]) (get-series-to-series-point-sequence x-series y)) y-series)))

(: get-series-list-point-sequence (SeriesList -> (Listof (Listof (Listof Real)))))
(define (get-series-list-point-sequence series-list)
  (let ((x-series (car series-list))
        (y-series (cdr series-list)))
    (map (lambda ([y : Series]) (get-series-to-series-point-sequence x-series y)) y-series)))

(: get-columns-point-sequence (Columns -> (Listof (Listof (Listof Real)))))
(define (get-columns-point-sequence columns)
  (let ((x-series (column-series (car columns)))
        (y-series (map column-series (cdr columns))))
    (map (lambda ([y : Series]) (get-series-to-series-point-sequence x-series y)) y-series)))

(: make-points ((Listof (Listof Real)) -> renderer2d))
(define (make-points list-of-points)
  (points list-of-points))

(: make-lines ((Listof (Listof Real)) -> renderer2d))
(define (make-lines list-of-points)
  (lines list-of-points))

(: make-scatter-plot ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-scatter-plot data)
  (let: ((plot-points : (Treeof renderer2d)
         (cond
           [(and (Series? data) (is-plottable-series data)) (list (points (get-series-point-sequence data)))]
           [(and (SeriesList? data) (andmap is-plottable-series data)) (map make-points (get-series-list-point-sequence data))]
           [(Column? data) (list (points (get-column-point-sequence data)))]
           [(Columns? data) (map make-points (get-columns-point-sequence data))]
           [(DataFrame? data) (map make-points (get-data-frame-point-sequence data))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    (plot
     plot-points         
     #:x-min -20 #:x-max 20 #:y-min -20 #:y-max 20)))

(: make-line-plot ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-line-plot data)
  (let: ((plot-points : (Treeof renderer2d)
         (cond
           [(and (Series? data) (is-plottable-series data)) (list (lines (get-series-point-sequence data)))]
           [(and (SeriesList? data) (andmap is-plottable-series data)) (map make-lines (get-series-list-point-sequence data))]
           [(Column? data) (list (lines (get-column-point-sequence data)))]
           [(Columns? data) (map make-lines (get-columns-point-sequence data))]
           [(DataFrame? data) (map make-lines (get-data-frame-point-sequence data))]
           [else (error 'make-line-plot "Invalid data to plot")])))
    (plot
     plot-points         
     #:x-min -20 #:x-max 20 #:y-min -20 #:y-max 20)))

; ***********************************************************

; ***********************************************************

(define-type HistBin (HashTable Any Real))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-hist-bins (-> HistBin))
(define (make-hist-bins)
  (make-hash))

; Used to determine the groups for the groupby. If by is a function, it’s called
; on each value of the object’s index. If a dict or Series is passed, the Series
; or dict VALUES will be used to determine the groups (the Series’ values are first
; aligned; see .align() method). If an ndarray is passed, the values are used as-is
; determine the groups. A label or list of labels may be passed to group by the columns
; in self. Notice that a tuple is interpreted a (single) key.
(: get-discrete-hist-data ((U (Vectorof Any) (Vectorof Boolean) (Vectorof Datetime) (Vectorof Fixnum) (Vectorof Symbol) FlVector) -> HistBin))
(define (get-discrete-hist-data data-vec)
  (define: hist-bin-index : HistBin (make-hist-bins))
  (define len
    (if (flvector? data-vec)
        (flvector-length data-vec)
        (vector-length data-vec)))

  (let loop ([i 0])
    (if (>= i len)
	hist-bin-index
	(let: ((i : Index (assert i index?)))
	      (let ((key

                     (if (flvector? data-vec)
                         (flvector-ref data-vec i)
                         (vector-ref data-vec i))))
		(hash-update! hist-bin-index key
			      (λ: ((incr : Real))
				  (add1 incr))
			      (λ () 0)))
	      (loop (add1 i))))))

(: list-of-vec-from-hist-bin (HistBin -> (Listof (Vector Any Real))))
(define (list-of-vec-from-hist-bin hist-bin)
  (hash-map hist-bin (lambda ([key : Any] [value : Real]) : (Vector Any Real) (vector key value))))

;ann
(: make-discrete-histogram ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-discrete-histogram data)
  (let: ((plot-points : renderer2d
         (cond
           [(and (Series? data) (is-plottable-series data))
            (discrete-histogram (cast (list-of-vec-from-hist-bin (get-discrete-hist-data (series-data data))) (Sequenceof
             (U (List Any (U False Real ivl)) (Vector Any (U False Real ivl))))))]
           ;[(and (SeriesList? data) (andmap is-plottable-series data)) (map make-points (get-series-list-point-sequence data))]
           ;[(Column? data) (list (points (get-column-point-sequence data)))]
           ;[(Columns? data) (map make-points (get-columns-point-sequence data))]
           ;[(DataFrame? data) (map make-points (get-data-frame-point-sequence data))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    (plot
     plot-points)))

; ***********************************************************

;******************
; test cases
;******************

(displayln "plotting integer series")

(make-scatter-plot (new-ISeries (vector 1 2 3 4 5) #f))

(make-line-plot (new-ISeries (vector 1 2 3 4 5) #f))

(define float-column (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f)))

(displayln "plotting float columns")

(make-scatter-plot float-column)

(make-line-plot float-column)

;******************
;data-frame-integer
;******************
(define columns-integer
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame columns-integer))

(displayln "plotting integer data-frame")

(make-scatter-plot data-frame-integer)

(displayln "plotting integer columns")

(make-scatter-plot columns-integer)

;******************
;data-frame-float
;******************
(define columns-float
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5) #f))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5) #f))))

; create new data-frame-float
(define data-frame-float (new-data-frame columns-float))

(displayln "plotting float data-frame")

(make-scatter-plot data-frame-float)

(displayln "plotting float columns")

(make-scatter-plot columns-float)

(displayln "discrete histogram")

(make-discrete-histogram (new-ISeries (vector 1 2 3 4 5 5 5) #f))


; individual testing does not work due to Racket type checking
; (get-discrete-hist-data (vector 1 2 3 4 5))

; (list-of-vec-from-hist-bin (get-discrete-hist-data (vector 1 2 3 4 5)))
