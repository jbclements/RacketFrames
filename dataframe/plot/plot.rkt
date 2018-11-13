#lang typed/racket

(require plot)
(require plot/utils)

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
 racket/fixnum
 racket/set
 racket/vector
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in "../util/symbol.rkt"
	  symbol-prefix)
 (only-in "../data-frame/indexed-series.rkt"
	  IndexDataType build-index-from-list
	  Label SIndex LabelIndex LabelIndex-index
          label-index label->lst-idx
          idx->key is-labeled?)
 (only-in "../data-frame/series.rkt"
	  series-complete)
 (only-in "../data-frame/series-description.rkt"
	  SeriesType Series Series? SeriesList SeriesList?
	  SeriesDescription-type
	  series-iref series-type series-length
          series-data get-series-index has-series-index?)
 (only-in "../data-frame/data-frame.rkt"
	  DataFrame DataFrame? Column Columns Columns? Column? new-data-frame data-frame-names
	  data-frame-cseries data-frame-explode column-series
	  DataFrameDescription DataFrameDescription-series data-frame-description)
 (only-in "../data-frame/generic-series.rkt"
	  GenSeries GenSeries? GenericType gen-series-iref new-GenSeries
	  gen-series-referencer)
 (only-in "../data-frame/numeric-series.rkt"
	  NSeries NSeries? nseries-iref nseries-index-ref new-NSeries)
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

(define-type PlottableSeries (U NSeries ISeries))

(define-predicate PlottableSeries? (U NSeries ISeries))

(define-predicate RealList? (Listof Real))

(define-predicate NestedRenderer2dList? (Listof (Listof renderer2d)))

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

(define-type HistBinStacked (HashTable Any (Listof Real)))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-hist-bin (-> HistBin))
(define (make-hist-bin)
  (make-hash))

; This function is self-explanatory, it consumes no arguments
; and creates a hash map which will represent a JoinHash.
(: make-hist-bin-stacked (-> HistBinStacked))
(define (make-hist-bin-stacked)
  (make-hash))

(: get-flvector/vector-length ((U FlVector (Vectorof Fixnum)) -> Index))
(define (get-flvector/vector-length flvector/fxvector)
  (if (flvector? flvector/fxvector)
        (flvector-length flvector/fxvector)
        (vector-length flvector/fxvector)))

(: get-flvector/vector-ref ((U FlVector (Vectorof Fixnum)) Index -> (U Float Fixnum)))
(define (get-flvector/vector-ref flvector/fxvector i)
  (if (flvector? flvector/fxvector)
        (flvector-ref flvector/fxvector i)
        (vector-ref flvector/fxvector i)))
  

; Used to determine the groups for the groupby. If by is a function, it’s called
; on each value of the object’s index. If a dict or Series is passed, the Series
; or dict VALUES will be used to determine the groups (the Series’ values are first
; aligned; see .align() method). If an ndarray is passed, the values are used as-is
; determine the groups. A label or list of labels may be passed to group by the columns
; in self. Notice that a tuple is interpreted a (single) key.
(: get-discrete-hist-data ((U (Vectorof Any) (Vectorof Boolean) (Vectorof Datetime) (Vectorof Fixnum) (Vectorof Symbol) FlVector) -> HistBin))
(define (get-discrete-hist-data data-vec)
  (define: hist-bin-index : HistBin (make-hist-bin))
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

(: get-discrete-hist-data-vec ((U (Vectorof IndexDataType) (Vectorof Any) (Vectorof Boolean) (Vectorof Datetime) (Vectorof Fixnum) (Vectorof Symbol) FlVector)
                               PlottableSeries -> HistBinStacked))
(define (get-discrete-hist-data-vec data-vec-one data-series-two)
  (define: hist-bin-index : HistBinStacked (make-hist-bin-stacked))
  (define vec-one-len
    (if (flvector? data-vec-one)
        (flvector-length data-vec-one)
        (vector-length data-vec-one)))

  (define series-two-len
    (series-length data-series-two))

  (when (not (= vec-one-len series-two-len))
    (error "Vectors must be of the same length."))

  (let loop ([i 0])
    (if (>= i vec-one-len)
	hist-bin-index
	(let: ((i : Index (assert i index?)))
	      (let ((key
                     (if (flvector? data-vec-one)
                         (flvector-ref data-vec-one i)
                         (vector-ref data-vec-one i))))
		(hash-update! hist-bin-index key
			      (λ: ((val-list : (Listof Real)))
				  (append val-list (assert (series-iref data-series-two i) RealList?)))
			      (λ () (list)))
          (loop (add1 i)))))))

(: list-of-vec-from-hist-bin (HistBin -> (Listof (Vector Any Real))))
(define (list-of-vec-from-hist-bin hist-bin)
  (hash-map hist-bin (lambda ([key : Any] [value : Real]) : (Vector Any Real) (vector key value))))

(: list-of-vec-from-hist-bin-stacked (HistBinStacked -> (Listof (Vector Any (Listof Real)))))
(define (list-of-vec-from-hist-bin-stacked hist-bin-stacked)
  (hash-map hist-bin-stacked (lambda ([key : Any] [value : (Listof Real)]) : (Vector Any (Listof Real)) (vector key value))))

(: discrete-histogram-from-vector ((U (Vectorof Any) (Vectorof Boolean) (Vectorof Datetime) (Vectorof Fixnum) (Vectorof Label) FlVector) -> renderer2d))
(define (discrete-histogram-from-vector vec)
  (discrete-histogram (cast (list-of-vec-from-hist-bin (get-discrete-hist-data vec))
                            (Sequenceof (U (List Any (U False Real ivl)) (Vector Any (U False Real ivl)))))))

(: discrete-histogram-stacked-from-vector ((U (Vectorof Any) (Vectorof IndexDataType) (Vectorof Boolean) (Vectorof Datetime) (Vectorof Fixnum) (Vectorof Label) FlVector)
                                           PlottableSeries -> (Listof renderer2d)))
(define (discrete-histogram-stacked-from-vector data-vec-one data-series-two)
  (stacked-histogram (cast (list-of-vec-from-hist-bin-stacked (get-discrete-hist-data-vec data-vec-one (assert data-series-two PlottableSeries?))) 	
                            (Sequenceof (U (Vector Any (Sequenceof Real)) (List Any (Sequenceof Real)))))))

;ann
(: make-discrete-histogram ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-discrete-histogram data)
  (let: ((plot-points : (U renderer2d (Listof renderer2d))
         (cond
           [(and (Series? data) (is-plottable-series data))
            (discrete-histogram-from-vector (series-data data))]
           [(and (SeriesList? data) (andmap is-plottable-series data))
            (map (lambda ([d : Series]) : renderer2d
                  (discrete-histogram-from-vector (series-data d))) data)]
           [(and (Column? data) (is-plottable-series (column-series data)))
            (discrete-histogram-from-vector (series-data (column-series data)))]
           [(and (Columns? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) data)))
            (map (lambda ([d : Column])
             (discrete-histogram-from-vector (series-data (column-series d)))) data)]
           ; A histogram is a representation of the distribution of data. This function calls matplotlib.pyplot.hist(),
           ;on each series in the DataFrame, resulting in one histogram per column.
           [(and (DataFrame? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) (data-frame-explode data))))
            (map (lambda ([d : Column])
             (discrete-histogram-from-vector (series-data (column-series d)))) (data-frame-explode data))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    (if (list? plot-points)
        (map (lambda ([p-p : renderer2d]) (plot p-p)) plot-points)
        (plot
         plot-points))))

(: get-index-vector (Series -> (U (Vectorof IndexDataType) (Vectorof Fixnum))))
(define (get-index-vector series)
  (: series-len Index)
  (define series-len (series-length series))
  (define v (series-data series))
  ; initialize vectors with default values
  (: index-data-type-vector (Vectorof IndexDataType))
  (define index-data-type-vector (make-vector (assert series-len index?) 0))
  (: index-vector (Vectorof Fixnum))
  (define index-vector (make-vector (assert series-len index?) 0))
  
  (let ((len series-len))
    (if (zero? len)
        (vector)
        (begin
          (do ((i 0 (add1 i)))
            ((>= i len)
             (if (has-series-index? series)                  
                  index-data-type-vector
                  index-vector))
            (if (has-series-index? series)
                (vector-set! index-data-type-vector (assert i index?) (idx->key (get-series-index series) (assert i index?)))
                (vector-set! index-vector (assert i index?) (assert i index?))))))))

; In the case of multiple data vectors, the first vector is plotted against all other vectors. Else in the singular case,
; the index is plotted against the vector. The DataFrame can be projected to only use a subset of columns.
(: make-discrete-histogram-stacked ((U Series (Listof Series) DataFrame Column Columns) -> Any))
(define (make-discrete-histogram-stacked data)
  (let: ((plot-points : (U (Listof (Listof renderer2d)) (Listof renderer2d))
         (cond
           [(and (Series? data) (is-plottable-series data))
            (discrete-histogram-stacked-from-vector (get-index-vector data) (assert data PlottableSeries?))]
           [(and (SeriesList? data) (andmap is-plottable-series data))
            (map (lambda ([d : Series]) : (Listof renderer2d)
                   (discrete-histogram-stacked-from-vector (series-data (car data)) (assert d PlottableSeries?))) (cdr data))]
           [(and (Column? data) (is-plottable-series (column-series data)))
            (discrete-histogram-stacked-from-vector (get-index-vector (column-series data)) (assert (column-series data) PlottableSeries?))]
           [(and (Columns? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) data)))
            (map (lambda ([d : Column])
                   (discrete-histogram-stacked-from-vector (series-data (column-series (car data))) (assert (column-series d) PlottableSeries?))) (cdr data))]
           ; A histogram is a representation of the distribution of data. This function calls matplotlib.pyplot.hist(),
           ;on each series in the DataFrame, resulting in one histogram per column.
           [(and (DataFrame? data) (andmap is-plottable-series (map (lambda ([d : Column]) (column-series d)) (data-frame-explode data))))
            (let ((columns : Columns (data-frame-explode data)))
              (map (lambda ([d : Column])
                     (discrete-histogram-stacked-from-vector (series-data (column-series (car columns))) (assert (column-series d) PlottableSeries?))) (cdr columns)))]
           [else (error 'make-scatter-plot "Invalid data to plot")])))
    
        (if (NestedRenderer2dList? plot-points)
            (map (lambda ([p-p : (Listof renderer2d)]) (plot p-p)) plot-points)
            (plot
             plot-points))))

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
(define integer-columns
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4 4) #f))
   (cons 'col2 (new-ISeries (vector 5 6 7 8 24) #f))
   (cons 'col3 (new-ISeries (vector 9 10 11 12 24) #f))))

; create new data-frame-integer
(define data-frame-integer (new-data-frame integer-columns))

(displayln "plotting integer data-frame")

(make-scatter-plot data-frame-integer)

(displayln "plotting integer columns")

(make-scatter-plot integer-columns)

;******************
;data-frame-float
;******************
(define float-columns
  (list 
   (cons 'col1 (new-NSeries (flvector 1.5 2.5 3.5 4.5) #f))
   (cons 'col2 (new-NSeries (flvector 5.5 6.5 7.5 8.5) #f))
   (cons 'col3 (new-NSeries (flvector 9.5 10.5 11.5 12.5) #f))))

; create new data-frame-float
(define data-frame-float (new-data-frame float-columns))

(displayln "plotting float data-frame")

(make-scatter-plot data-frame-float)

(displayln "plotting float columns")

(make-scatter-plot float-columns)

(displayln "discrete histogram")

(make-discrete-histogram (new-ISeries (vector 1 2 2 3 4 5 5 5) #f))

(make-discrete-histogram (new-GenSeries (vector 1 2 2 1.5 3 4 5 5 'a 5) #f))

(make-discrete-histogram float-column)

(make-discrete-histogram integer-columns)

(make-discrete-histogram data-frame-float)

(get-index-vector (new-ISeries (vector 1 2 3 4 4) #f))

(define series-integer-labeled
  (new-ISeries (vector 1 2 3 4)
               (list 'a 'b 'c 'd)))

(get-index-vector series-integer-labeled)

(make-discrete-histogram-stacked float-column)

(make-discrete-histogram-stacked float-columns)

;******************
;data-frame-integer
;******************
(define integer-columns-2
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4)
                            (build-index-from-list (list 'a 'b 'c 'd))))
   (cons 'col2 (new-ISeries (vector 5 6 7 8)
                            (build-index-from-list (list 'e 'f 'g 'h))))
   (cons 'col3 (new-ISeries (vector 9 10 11 12)
                            (build-index-from-list (list 'i 'j 'k 'l))))))

; create new data-frame-integer
(define data-frame-integer-2 (new-data-frame integer-columns-2))

(make-discrete-histogram-stacked data-frame-integer-2)

; individual testing does not work due to Racket type checking
;(get-discrete-hist-data (vector 1 2 3 4 5))

;(list-of-vec-from-hist-bin (get-discrete-hist-data (vector 1 2 3 4 5)))
