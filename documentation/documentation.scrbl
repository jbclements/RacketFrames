#lang scribble/manual
@(require scribble/core)

@require[(for-label racket/contract)]

@title[#:tag "top" #:version "1.0" #:style 'toc-hidden]{RacketFrames}

@author["Shubham" "Kahal"]

@"\U2192" A DataFrame implementation in the spirit of Pandas or R data frames.

@"\U2192" In this day and age procedural and functional programmers alike are working
with large amounts of data. Many analytical libraries have been developed for many
domains to work with such data. Python/Pandas, R or even the command prompt can be
used to accomplish theses tasks, but this option is especially good if you are 1)
using Racket already 2) may need to integrate your solution with other Racket applications
in the future or 3) just plain love functional programming.

Table of Contents

@table-of-contents[]

@; ======================================================================
@section[#:tag "Series"]{Series}

@itemlist[

 @item{One-dimensional ndarray with axis labels (including time series).

Labels need not be unique but must be a hashable type. The object supports both integer- and label-based indexing and provides a host of methods for performing operations involving the index. Statistical methods from ndarray have been overridden to automatically exclude missing data (currently represented as NaN).

Operations between Series (+, -, /, , *) align values based on their associated index values– they need not be the same length. The result index will be the sorted union of the two indexes.
}

]

@subsection[#:style 'toc]{Integer Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-ISeries"]{new-ISeries}
@defproc[#:link-target? #f
 (new-ISeries [data (Vectorof Fixnum)] [label (Option (U (Listof Label) SIndex))])
ISeries?]{
Returns a new ISeries.
}

@codeblock|{
    (define series-integer (new-ISeries (vector 1 2 3 4)
                (build-index-from-labels (list 'a 'b 'c 'd))))
  }|

@subsubsection[#:tag "iseries-iref"]{iseries-iref}
@defproc[#:link-target? #f
 (iseries-iref [iseries ISeries] [idx Index])
Fixnum?]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-iref series-integer 0) ; 1

  (iseries-iref series-integer 1) ; 2
  }|

@subsubsection[#:tag "iseries-label-ref"]{iseries-label-ref}
@defproc[#:link-target? #f
 (iseries-label-ref [iseries ISeries] [label Label])
Fixnum?]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-label-ref series-integer 'd) ; 4

  (iseries-label-ref series-integer 'c) ; 3
  }|

@subsubsection[#:tag "iseries-range"]{iseries-range}
@defproc[#:link-target? #f
 (iseries-range [iseries ISeries] [pos Index])
(Vectorof Fixnum)]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-range series-integer 2) ; (vector 1 2)
  }|

@subsubsection[#:tag "iseries-length"]{iseries-length}
@defproc[#:link-target? #f
 (iseries-length [iseries ISeries])
Index]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-length series-integer) ; 4
  }|

(ISeries -> (Index -> Fixnum)
@subsubsection[#:tag "iseries-referencer"]{iseries-referencer}
@defproc[#:link-target? #f
 (iseries-referencer [iseries ISeries])
(Index -> Fixnum)]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  ((iseries-referencer series-integer) 0) ; 1

  ((iseries-referencer series-integer) 1) ; 2
  }|


@subsubsection[#:tag "iseries-data"]{iseries-data}

@defproc[#:link-target? #f
 (iseries-data [iseries ISeries])
(Vectorof Fixnum)]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (iseries-data series-integer) ; (vector 1 2 3 4)
  }|

[map/is (ISeries (Fixnum -> Fixnum) -> ISeries)]
 [bop/is (ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)]
 [comp/is (ISeries ISeries (Fixnum Fixnum -> Boolean) -> BSeries)]
@subsubsection[#:tag "map/is"]{map/is}

@defproc[#:link-target? #f
 (map/is [iseries ISeries] [fn (Fixnum -> Fixnum)])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (ISeries-data (map/is series-integer (λ: ((x : Fixnum)) (unsafe-fx+ x 1)))) ; (vector 2 3 4 5)
  }|

@subsubsection[#:tag "bop/is"]{bop/is}
(ISeries ISeries (Fixnum Fixnum -> Fixnum) -> ISeries)

@defproc[#:link-target? #f
 (bop/is [iseries ISeries] [iseries-2 ISeries] [fn (Fixnum Fixnum -> Fixnum)])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (bop/is series-integer series-integer-2 (λ: ((x : Fixnum) (y : Fixnum)) (unsafe-fx+ x y)))) ; (new-ISeries 6 8 10 12)
  }|

@subsubsection[#:tag "comp/is"]{comp/is}

@defproc[#:link-target? #f
 (comp/is [iseries ISeries] [iseries-2 ISeries] [fn (Fixnum Fixnum -> Boolean)])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (comp/is series-integer series-integer-2 (λ: ((x : Fixnum) (y : Fixnum)) (unsafe-fx> x y)))) ; (new-BSeries #f #f #f #f)
  }|


@subsubsection[#:tag "+/is"]{+/is}

@defproc[#:link-target? #f
 (+/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (+/is series-integer series-integer-2)) ; (vector 6 8 10 12)
  }|

@subsubsection[#:tag "-/is"]{-/is}

@defproc[#:link-target? #f
 (+/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (-/is series-integer series-integer-2)) ; (vector -4 -4 -4 -4)
  }|

@subsubsection[#:tag "*/is"]{*/is}

@defproc[#:link-target? #f
 (*/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (*/is series-integer series-integer-2)) ; (vector 5 12 21 32)
  }|

@subsubsection[#:tag "//is"]{//is}

@defproc[#:link-target? #f
 (//is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (//is series-integer series-integer-2)) ; (vector 0 0 0 0)
  }|

@subsubsection[#:tag "iseries-data"]{%/is}

@defproc[#:link-target? #f
 (%/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (%/is series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "r/is"]{r/is}

@defproc[#:link-target? #f
 (r/is [iseries ISeries] [iseries-2 ISeries])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (ISeries-data (r/is series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "+./is"]{+./is}

@defproc[#:link-target? #f
 (+./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (+./is series-integer 2)) ; (vector 3 4 5 6)
  }|

@subsubsection[#:tag "-./is"]{-./is}

@defproc[#:link-target? #f
 (-./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (-./is series-integer 2)) ; (vector -1 0 1 2)
  }|

@subsubsection[#:tag "*./is"]{*./is}

@defproc[#:link-target? #f
 (*./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (*./is series-integer 2)) ; (vector 2 3 6 8)
  }|

@subsubsection[#:tag "/./is"]{/./is}

@defproc[#:link-target? #f
 (/./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (/./is series-integer 2)) ; (vector 0 1 1 2)
  }|

@subsubsection[#:tag "%./is"]{%./is}

@defproc[#:link-target? #f
 (%./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (%./is series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag "r./is"]{r./is}

@defproc[#:link-target? #f
 (r./is [iseries ISeries] [num Fixnum])
ISeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (ISeries-data (r./is series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag ">/is"]{>/is}

@defproc[#:link-target? #f
 (>/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (>/is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "</is"]{</is}

@defproc[#:link-target? #f
 (</is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (</is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag ">=/is"]{>=/is}

@defproc[#:link-target? #f
 (>=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (>=/is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "<=/is"]{<=/is}

@defproc[#:link-target? #f
 (<=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (<=/is series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "=/is"]{=/is}

@defproc[#:link-target? #f
 (=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (=/is series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "!=/is"]{!=/is}

@defproc[#:link-target? #f
 (!=/is [iseries ISeries] [iseries-2 ISeries])
BSeries]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-ISeries (vector 5 6 7 8) #f))

  (BSeries-data (!=/is series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "{apply-agg-is"]{apply-agg-is}

@defproc[#:link-target? #f
 (apply-agg-is [func Symbol] [iseries ISeries])
Real]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (apply-agg-is 'sum series-integer) ; 10

  (apply-agg-is 'mean series-integer) ; 10/4

  (apply-agg-is 'count series-integer) ; 4

  (apply-agg-is 'min series-integer) ; 1

  (apply-agg-is 'max series-integer) ; 4
  }|

@subsubsection[#:tag "apply-stat-is"]{apply-stat-is}

@defproc[#:link-target? #f
 (apply-stat-is [func Symbol] [iseries ISeries])
Real]{
Returns the Fixnum value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-ISeries (vector 1 2 3 4) #f))

  (apply-stat-is 'variance series-integer) ; 5/4

  (apply-stat-is 'stddev series-integer) ; 1.118033988749895

  (apply-stat-is 'skewness series-integer) ; 0.0
  }|

@subsection[#:style 'toc]{Numerical Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-NSeries"]{new-NSeries}
@defproc[#:link-target? #f
 (new-NSeries [data (Vectorof Float)] [label (Option (U (Lnstof Label) SIndex))])
NSeries?]{
Returns a new NSeries.
}

@codeblock|{
    (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))
  }|

@subsubsection[#:tag "nseries-iref"]{nseries-iref}
@defproc[#:link-target? #f
 (nseries-iref [nseries NSeries] [idx Index])
Float?]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
 
(define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))
                                      
(nseries-iref series-float 0) ; 1.5

(nseries-iref series-float 1) ; 2.4
  }|

@subsubsection[#:tag "nseries-label-ref"]{nseries-label-ref}
@defproc[#:link-target? #f
 (nseries-label-ref [nseries NSeries] [label Label])
Float?]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-label-ref series-float 'd) ; 4.1
  
  (nseries-label-ref series-float 'c) ; 3.6
  }|

@subsubsection[#:tag "nseries-range"]{nseries-range}
@defproc[#:link-target? #f
 (nseries-range [nseries NSeries] [pos Index])
(Vectorof Float)]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-range series-float 2) ; (flvector 1.5 2.4)
  }|

@subsubsection[#:tag "nseries-length"]{nseries-length}
@defproc[#:link-target? #f
 (nseries-length [nseries NSeries])
Index]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-length series-float) ; 4
  }|

@subsubsection[#:tag "nseries-referencer"]{nseries-referencer}
@defproc[#:link-target? #f
 (nseries-referencer [nseries NSeries])
(Index -> Float)]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  ((nseries-referencer series-float) 0) ; 1.5

  ((nseries-referencer series-float) 1) ; 2.4
  }|


@subsubsection[#:tag "nseries-data"]{nseries-data}

@defproc[#:link-target? #f
 (nseries-data [nseries NSeries])
(Vectorof Float)]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (nseries-data series-foat) ; (flvector 1.5 2.4 3.6 4.1)ap
  }|

@subsubsection[#:tag "map/ns"]{map/ns}

@defproc[#:link-target? #f
 (map/ns [nseries NSeries] [fn (Float -> Float)])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
  (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (map/ns series-float (λ: ((x : Float)) (fl+ x 1.0)))) ; (flvector 2.5 3.4 4.6 5.1)
  }|

@subsubsection[#:tag "bop/ns"]{bop/ns}
(NSeries NSeries (Float Float -> Float) -> NSeries)

@defproc[#:link-target? #f
 (bop/ns [nseries NSeries] [nseries-2 NSeries] [fn (Float Float -> Float)])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (bop/ns series-integer series-integer-2 (λ: ((x : Float) (y : Float)) (unsafe-fx+ x y)))) ; (new-NSeries 6 8 10 12)
  }|

@subsubsection[#:tag "comp/ns"]{comp/ns}

@defproc[#:link-target? #f
 (comp/ns [nseries NSeries] [nseries-2 NSeries] [fn (Float Float -> Boolean)])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (comp/ns series-integer series-integer-2 (λ: ((x : Float) (y : Float)) (unsafe-fx> x y)))) ; (new-BSeries #f #f #f #f)
  }|


@subsubsection[#:tag "+/ns"]{+/ns}

@defproc[#:link-target? #f
 (+/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (+/ns series-float series-float-2)) ; (flvector 6.5 8.4 10.6 12.1))
  }|

@subsubsection[#:tag "-/ns"]{-/ns}

@defproc[#:link-target? #f
 (+/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (-/ns series-float series-float-2)) ; (flvector -3.5 -3.6 -3.4 -3.9000000000000004)
  }|

@subsubsection[#:tag "*/ns"]{*/ns}

@defproc[#:link-target? #f
 (*/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (*/ns series-float series-float-2)) ; (flvector 7.5 14.399999999999999 25.2 32.8)
  }|

@subsubsection[#:tag "//ns"]{//ns}

@defproc[#:link-target? #f
 (//ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-float (new-NSeries (flvector 1.5 2.4 3.6 4.1)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (define series-float-2 (new-NSeries (flvector 5.0 6.0 7.0 8.0)
                                      (build-index-from-labels (list 'a 'b 'c 'd))))

  (NSeries-data (//ns series-float series-float-2)) ; (flvector 0.3 0.39999999999999997 0.5142857142857143 0.5125)
  }|

@subsubsection[#:tag "nseries-data"]{%/ns}

@defproc[#:link-target? #f
 (%/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (%/ns series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "r/ns"]{r/ns}

@defproc[#:link-target? #f
 (r/ns [nseries NSeries] [nseries-2 NSeries])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (NSeries-data (r/ns series-integer series-integer-2)) ; (vector 1 2 3 4)
  }|

@subsubsection[#:tag "+./ns"]{+./ns}

@defproc[#:link-target? #f
 (+./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (+./ns series-integer 2)) ; (vector 3 4 5 6)
  }|

@subsubsection[#:tag "-./ns"]{-./ns}

@defproc[#:link-target? #f
 (-./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (-./ns series-integer 2)) ; (vector -1 0 1 2)
  }|

@subsubsection[#:tag "*./ns"]{*./ns}

@defproc[#:link-target? #f
 (*./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (*./ns series-integer 2)) ; (vector 2 3 6 8)
  }|

@subsubsection[#:tag "/./ns"]{/./ns}

@defproc[#:link-target? #f
 (/./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (/./ns series-integer 2)) ; (vector 0 1 1 2)
  }|

@subsubsection[#:tag "%./ns"]{%./ns}

@defproc[#:link-target? #f
 (%./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (%./ns series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag "r./ns"]{r./ns}

@defproc[#:link-target? #f
 (r./ns [nseries NSeries] [num Float])
NSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (NSeries-data (r./ns series-integer 2)) ; (vector 1 0 1 0)
  }|

@subsubsection[#:tag ">/ns"]{>/ns}

@defproc[#:link-target? #f
 (>/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (>/ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "</ns"]{</ns}

@defproc[#:link-target? #f
 (</ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (</ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag ">=/ns"]{>=/ns}

@defproc[#:link-target? #f
 (>=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (>=/ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "<=/ns"]{<=/ns}

@defproc[#:link-target? #f
 (<=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (<=/ns series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "=/ns"]{=/ns}

@defproc[#:link-target? #f
 (=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (=/ns series-integer series-integer-2)) ; (vector #f #f #f #f)
  }|

@subsubsection[#:tag "!=/ns"]{!=/ns}

@defproc[#:link-target? #f
 (!=/ns [nseries NSeries] [nseries-2 NSeries])
BSeries]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (define series-integer-2 (new-NSeries (vector 5 6 7 8) #f))

  (BSeries-data (!=/ns series-integer series-integer-2)) ; (vector #t #t #t #t)
  }|

@subsubsection[#:tag "{apply-agg-ns"]{apply-agg-ns}

@defproc[#:link-target? #f
 (apply-agg-ns [func Symbol] [nseries NSeries])
Real]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (apply-agg-ns 'sum series-integer) ; 10

  (apply-agg-ns 'mean series-integer) ; 10/4

  (apply-agg-ns 'count series-integer) ; 4

  (apply-agg-ns 'min series-integer) ; 1

  (apply-agg-ns 'max series-integer) ; 4
  }|

@subsubsection[#:tag "apply-stat-ns"]{apply-stat-ns}

@defproc[#:link-target? #f
 (apply-stat-ns [func Symbol] [nseries NSeries])
Real]{
Returns the Float value at the specified index in the series.
}

@codeblock|{
  (define series-integer (new-NSeries (vector 1 2 3 4) #f))

  (apply-stat-ns 'variance series-integer) ; 5/4

  (apply-stat-ns 'stddev series-integer) ; 1.118033988749895

  (apply-stat-ns 'skewness series-integer) ; 0.0
  }|


@subsection[#:style 'toc]{Categorical Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-CSeries"]{new-CSeries}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.

@subsection[#:style 'toc]{Boolean Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-BSeries"]{new-BSeries}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.


@subsection[#:style 'toc]{General Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-GSeries"]{new-GSeries}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.


@; ======================================================================

@; ======================================================================
@section{DataFrames}

@itemlist[

 @item{One-dimensional ndarray with axis labels (including time series).

Labels need not be unique but must be a hashable type. The object supports both integer- and label-based indexing and provides a host of methods for performing operations involving the index. Statistical methods from ndarray have been overridden to automatically exclude missing data (currently represented as NaN).

Operations between Series (+, -, /, , *) align values based on their associated index values– they need not be the same length. The result index will be the sorted union of the two indexes.
}

]

@subsection[#:style 'toc]{DataFrame}

@local-table-of-contents[]

@subsubsection[#:tag "new-data-frame"]{new-data-frame}
@defproc[#:link-target? #f
 (new-data-frame [columns Columns])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
    ;******************
  ;data-frame-integer
  ;******************
  ; will define parse to automatically build this columns structure
  (define columns-integer
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4)
  (build-index-from-labels (list 'a 'b 'c 'd))))
  (cons 'col2 (new-ISeries (vector 5 6 7 8)
  (build-index-from-labels (list 'e 'f 'g 'h))))
  (cons 'col3 (new-ISeries (vector 9 10 11 12)
  (build-index-from-labels (list 'i 'j 'k 'l))))))

  ; create new data-frame-integer
  (define data-frame-integer (new-data-frame columns-integer))
  }|

@subsubsection[#:tag "data-frame-rename"]{data-frame-rename}
@defproc[#:link-target? #f
 (data-frame-rename [df DataFrame] [col Label] [new-col Label])
DataFrame?]{
 This function consumes a DataFrame and two labels. It locates
 the index of the column in the DataFrame LabelIndex and creates
 a new LabelIndex with the new column name mapped to this index.
 Then a new DataFrame is created and returned.
}

@codeblock|{
    (data-frame-rename)
  }|

@subsubsection[#:tag "data-frame-drop"]{data-frame-drop}
@defproc[#:link-target? #f
 (data-frame-drop [df DataFrame] [col-name Label])
DataFrame?]{
 This function consumes a DataFrame and a Label for the column
 to drop from the DataFrame. Using the data-frame-explode
 function, this column is filtered out of the DataFrame, and a
 new DataFrame is returned.
}

@codeblock|{
    (data-frame-drop)
  }|

@subsubsection[#:tag "data-frame-series"]{data-frame-series}
@defproc[#:link-target? #f
 (data-frame-drop [df DataFrame] [col-name Label])
Series?]{
 This function consumes a DataFrame and a Symbol representing
 the column name and returns the Series of that column.
}

@codeblock|{
    (data-frame-series)
  }|

@subsubsection[#:tag "data-frame-names"]{data-frame-names}
@defproc[#:link-target? #f
 (data-frame-names [df DataFrame] [col-name Label])
Series?]{
 This function consumes a DataFrame and returns of Listof
 Symbol representing all the column names.
}

@codeblock|{
    (data-frame-names)
  }|

@subsubsection[#:tag "data-frame-dim"]{data-frame-dim}
@defproc[#:link-target? #f
 (data-frame-dim [df DataFrame])
Dim?]{
 This function consumes a DataFrame and calculates the dimensions.
 It does so by calculating the length of a column series, thus
 getting the number of rows and the total number of columns. It
 then stores these two values in the Dim struct and returns it.
}

@codeblock|{
    (data-frame-dim)
  }|


@subsubsection[#:tag "data-frame-description"]{data-frame-description}
@defproc[#:link-target? #f
 (data-frame-description [df DataFrame] [project LabelProjection])
DataFrameDescription?]{
 This function consumes a DataFrame and an optional LabelProjection
 and constructs a DataFrameDescription struct which contains the
 DataFrame Dim and a list of SeriesDecriptions for each series of
 each column.
}

@codeblock|{
    (data-frame-description)
  }|

@subsubsection[#:tag "show-data-frame-description"]{show-data-frame-description}
@defproc[#:link-target? #f
 (show-data-frame-description [dfd DataFrameDescription])
Void?]{
 This function consumes a DataFrameDescription struct and displays
 the DataFrame Dim and Series Descriptions in formated form.
}

@codeblock|{
    (show-data-frame-description)
  }|

@subsubsection[#:tag "data-frame-explode"]{data-frame-explode}
@defproc[#:link-target? #f
 (data-frame-explode [df DataFrame] [project LabelProjection])
Columns?]{
 This function consumes a DataFrame and an optional LabelProjection
 which indicates which columns to project and returns a Listof Column.
 If no LabelProjection is given, all columns are projected. The
 label-sort-positional returns a labeling, which is a (Listof (Pair Label Index)).
 This list is looped through and only the items to project are filtered for.
 In the end an appropriate Listof Column is returned.
}

@codeblock|{
    (data-frame-explode)
  }|

@subsubsection[#:tag "data-frame-remove"]{data-frame-remove}
@defproc[#:link-target? #f
 (data-frame-remove [df DataFrame] [project LabelProjection])
DataFrame?]{
 This function consumes a DataFrame and LabelProjection indicating
 which columns to remove from the DataFrame. The column names in
 the DataFrame are put into a set called all-labels. The drop-projection
 is converted to a set as well. The labels to be kept are calculated
 by substracting the drop-projection from all of the possible labels.
 Then the DataFrame is exploded retaining only the columns to keep.
 A new data frame is constructed fomr these column and returned.
}

@codeblock|{
    (data-frame-remove)
  }|

@subsubsection[#:tag "data-frame-project"]{data-frame-project}
@defproc[#:link-target? #f
 (data-frame-project [df DataFrame] [project LabelProjection])
DataFrame?]{
 This function consumes a DataFrame and LabelProjection and
 creates a new DataFrame only containing the columns to project.
}

@codeblock|{
    (data-frame-project)
  }|

@subsubsection[#:tag "data-frame-replace"]{data-frame-replace}
@defproc[#:link-target? #f
 (data-frame-replace [df DataFrame] [col Column])
DataFrame?]{
 This function consumes a DataFrame and Column and replaces
 the old column with the same column heading with this new
 column.
}

@codeblock|{
    (data-frame-replace)
  }|

@subsubsection[#:tag "data-frame-extend"]{data-frame-extend}
@defproc[#:link-target? #f
 (data-frame-extend [df DataFrame] [col (U Column Columns DataFrame)])
DataFrame?]{
 This functions consumes a DataFrame and either a Column,
 Listof Column or a DataFrame and extends the given DataFrame.
 If a DataFrame is passed for the second argument it is first
 exploded and appended on to the exploded given data frame. Then
 a new DataFrame is constructed and returned. The other cases
 are self-explanatory.
}

@codeblock|{
    (data-frame-extend)
  }|

@subsection[#:style 'toc]{DataFrame Operations}

@local-table-of-contents[]

@subsubsection[#:tag "data-frame+"]{data-frame+}
@defproc[#:link-target? #f
 (data-frame+ [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame+")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame+ data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame-"]{data-frame-}
@defproc[#:link-target? #f
 (data-frame- [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame-")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame- data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame*"]{data-frame*}
@defproc[#:link-target? #f
 (data-frame* [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame*")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame* data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame/"]{data-frame/}
@defproc[#:link-target? #f
 (data-frame/ [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
  (define columns-integer-1
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 5 6 7 8) #f))
  (cons 'col3 (new-ISeries (vector 9 10 11 12) #f))
  (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

  (define columns-integer-2
  (list 
  (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
  (cons 'col2 (new-ISeries (vector 25 26 27 28) #f))
  (cons 'col3 (new-ISeries (vector 29 30 31 32) #f))
  (cons 'col4 (new-ISeries (vector 1 2 3 4) #f))))

  ; create new data-frame-integer-1
  (define data-frame-integer-1 (new-data-frame columns-integer-1))

  ; create new data-frame-integer-2
  (define data-frame-integer-2 (new-data-frame columns-integer-2))

  (displayln "data-frame/")

  (frame-write-tab data-frame-integer-1 (current-output-port))

  (frame-write-tab data-frame-integer-2 (current-output-port))

  (frame-write-tab (data-frame/ data-frame-integer-1 data-frame-integer-2) (current-output-port))
  }|

@subsubsection[#:tag "data-frame%"]{data-frame%}
@defproc[#:link-target? #f
 (data-frame% [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame-r"]{data-frame-r}
@defproc[#:link-target? #f
 (data-frame-r [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame="]{data-frame=}
@defproc[#:link-target? #f
 (data-frame= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame!="]{data-frame!=}
@defproc[#:link-target? #f
 (data-frame!= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame<"]{data-frame<}
@defproc[#:link-target? #f
 (data-frame< [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame>"]{data-frame>}
@defproc[#:link-target? #f
 (data-frame> [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame<="]{data-frame<=}
@defproc[#:link-target? #f
 (data-frame<= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame>="]{data-frame>=}
@defproc[#:link-target? #f
 (data-frame>= [dfa DataFrame] [dfb DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsubsection[#:tag "data-frame-abs"]{data-frame-abs}
@defproc[#:link-target? #f
 (data-frame-abs [df DataFrame])
DataFrame?]{
Returns a new DataFrame.
}

@subsection[#:style 'toc]{DataFrame Join}

@local-table-of-contents[]

@subsubsection[#:tag "data-frame-join-left"]{data-frame-join-left}
@defproc[#:link-target? #f
 (data-frame-join-left [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-left data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col4
1		a		a		21		11		a		22
2		b		1.5		22		0		null		0	
3		c		20		23		0		null		0
4		d		10		24		0		null		0
;; output ;;

  }|

@subsubsection[#:tag "data-frame-join-right"]{data-frame-join-right}
@defproc[#:link-target? #f
 (data-frame-join-right [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-right data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

  col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1		a		21		11		a		a		22
0		null		0		21		b		b		22
0		null		0		31		g		c		23
0		null		0		41		d		d		24
;; output ;;
  }|

@subsubsection[#:tag "data-frame-join-inner"]{data-frame-join-inner}
@defproc[#:link-target? #f
 (data-frame-join-inner [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-inner data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

  col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1		a		a		21		11		a		a		22
;; output ;;
  }|

@subsubsection[#:tag "data-frame-join-outer"]{data-frame-join-outer}
@defproc[#:link-target? #f
 (data-frame-join-outer [dfa DataFrame] [dfb DataFrame] [on (Listof Symbol)])
DataFrame?]{
Returns a new DataFrame.
}

@codeblock|{
   (define columns-mixed-7
  (list 
   (cons 'col1 (new-ISeries (vector 1 2 3 4) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'c 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 1.5 20 10) #f))
   (cons 'col4 (new-ISeries (vector 21 22 23 24) #f))))

(define columns-mixed-8
  (list 
   (cons 'col1 (new-ISeries (vector 11 21 31 41) #f))
   (cons 'col2 (new-CSeries (vector 'a 'b 'g 'd)))
   (cons 'col3 (new-GenSeries (vector 'a 'b 'c 'd) #f))
   (cons 'col4 (new-ISeries (vector 22 22 23 24) #f))))

; create new data-frame-mixed-7
(define data-frame-mixed-7 (new-data-frame columns-mixed-7))

; create new data-frame-mixed-8
(define data-frame-mixed-8 (new-data-frame columns-mixed-8))

(frame-write-tab data-frame-mixed-7 (current-output-port))

(frame-write-tab data-frame-mixed-8 (current-output-port))

(frame-write-tab (data-frame-join-outer data-frame-mixed-7 data-frame-mixed-8 #:on (list 'col3)) (current-output-port))

;; output ;;
col1	col2	col3	col4
1	a	a	21
2	b	1.5	22
3	c	20	23
4	d	10	24

  col1	col2	col3	col4
11	a	a	22
21	b	b	22
31	g	c	23
41	d	d	24

dfa-col1	dfa-col2	dfa-col3	dfa-col4	dfb-col1	dfb-col2	dfb-col3	dfb-col4
1		a		a		21		11		a		a		22
2		b		1.5		22		0		null		null		0
3		c		20		23		0		null		null		0
4		d		10		24		0		null		null		0
0		null		null		0		21		b		b		22
0		null		null		0		31		g		c		23
0		null		null		0		41		d		d		24
;; output ;;
  }|

@index-section[]
