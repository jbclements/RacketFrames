#lang scribble/base
@(require scribble/core
          scribble/manual)

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

This sentence is a paragraph all by itself.

@t{This sentence is a paragraph.}
@t{This sentence is also a paragraph, but it is connected to the
   previous paragraph as a compound paragraph by virtue of having no
   paragraph-breaking space before it, and each paragraph is in a
   ``SIntraPara'' @tt{<div>} instead of a @tt{<p>}.}

This sentence is a paragraph, as is each of A1, B1, A2, B2, A3, B3a,
and B2a in the following table, but B3a and B2a form a compound paragraph.
@;
@tabular[(list (list "A1"
                     "B1") 
               (list "A2"
                     "B2") 
               (list "A3" 
                     @compound-paragraph[plain (list @t{B3a} @t{B3b})]))]
@;
This sentence is a paragraph, and with the preceding table and
  paragraph forms a compound paragraph.

@nested{
 @t{This is a first paragraph in a @tt{<blockquote>}.}
 @t{This is a second paragraph in a @tt{<blockquote>}.}
}

@defproc[(make-sandwich [ingredients (listof ingredient?)])
sandwich?]{
Returns a sandwich given the right ingredients.
}

@defproc[#:kind "sandwich-maker"
(make-reuben [ingredient sauerkraut?] ...
[#:veggie? veggie? any/c #f])
sandwich?]{
Produces a reuben given some number of @racket[ingredient]s.
80
If @racket[veggie?] is @racket[#f], produces a standard
reuben with corned beef. Otherwise, produces a vegetable
reuben.
}

@index-section[]
