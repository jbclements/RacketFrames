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
 (make-sandwich [ingredients (listof ingredient?)])
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

@subsubsection[#:tag "iseries-iref"]{iseries-iref}

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

@subsubsection[#:tag "iseries-label-ref"]{iseries-label-ref}

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

@subsubsection[#:tag "iseries-range"]{iseries-range}

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

@subsubsection[#:tag "iseries-length"]{iseries-length}

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

@subsubsection[#:tag "iseries-referencer"]{iseries-referencer}

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

@subsubsection[#:tag "iseries-data"]{iseries-data}

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

@subsubsection[#:tag "map/is"]{map/is}

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

@subsubsection[#:tag "bop/is"]{bop/is}

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

@subsubsection[#:tag "comp/is"]{comp/is}

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

@subsubsection[#:tag "+/is"]{+/is}

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

@subsubsection[#:tag "-/is"]{-/is}

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

@subsubsection[#:tag "*/is"]{*/is}

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

@subsubsection[#:tag "//is"]{//is}

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

@subsubsection[#:tag "iseries-data"]{%/is}

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

@subsubsection[#:tag "r/is"]{r/is}

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

@subsubsection[#:tag "+./is"]{+./is}

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

@subsubsection[#:tag "-./is"]{-./is}

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

@subsubsection[#:tag "*./is"]{*./is}

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

@subsubsection[#:tag "/./is"]{/./is}

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

@subsubsection[#:tag "%./is"]{%./is}

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

@subsubsection[#:tag "r./is"]{r./is}

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

@subsubsection[#:tag ">/is"]{>/is}

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

@subsubsection[#:tag "</is"]{</is}

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

@subsubsection[#:tag ">=/is"]{>=/is}

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

@subsubsection[#:tag "<=/is"]{<=/is}

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

@subsubsection[#:tag "=/is"]{=/is}

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

@subsubsection[#:tag "!=/is"]{!=/is}

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

@subsubsection[#:tag "{apply-agg-is"]{apply-agg-is}

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

@subsubsection[#:tag "apply-stat-is"]{apply-stat-is}

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

@subsection[#:style 'toc]{Numerical Series}

@local-table-of-contents[]

@subsubsection[#:tag "new-NSeries"]{new-NSeries}

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

@subsubsection[#:tag "nseries-iref"]{nseries-iref}

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

@subsubsection[#:tag "nseries-label-ref"]{nseries-label-ref}

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

@subsubsection[#:tag "nseries-range"]{nseries-range}

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

@subsubsection[#:tag "nseries-length"]{nseries-length}

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

@subsubsection[#:tag "nseries-referencer"]{nseries-referencer}

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

@subsubsection[#:tag "nseries-data"]{nseries-data}

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

@subsubsection[#:tag "map/ns"]{map/ns}

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

@subsubsection[#:tag "bop/ns"]{bop/ns}

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

@subsubsection[#:tag "comp/ns"]{comp/ns}

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

@subsubsection[#:tag "+/ns"]{+/ns}

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

@subsubsection[#:tag "-/ns"]{-/ns}

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

@subsubsection[#:tag "*/ns"]{*/ns}

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

@subsubsection[#:tag "//ns"]{//ns}

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

@subsubsection[#:tag "nseries-data"]{%/ns}

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

@subsubsection[#:tag "r/ns"]{r/ns}

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

@subsubsection[#:tag "+./ns"]{+./ns}

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

@subsubsection[#:tag "-./ns"]{-./ns}

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

@subsubsection[#:tag "*./ns"]{*./ns}

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

@subsubsection[#:tag "/./ns"]{/./ns}

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

@subsubsection[#:tag "%./ns"]{%./ns}

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

@subsubsection[#:tag "r./ns"]{r./ns}

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

@subsubsection[#:tag ">/ns"]{>/ns}

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

@subsubsection[#:tag "</ns"]{</ns}

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

@subsubsection[#:tag ">=/ns"]{>=/ns}

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

@subsubsection[#:tag "<=/ns"]{<=/ns}

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

@subsubsection[#:tag "=/ns"]{=/ns}

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

@subsubsection[#:tag "!=/ns"]{!=/ns}

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

@subsubsection[#:tag "{apply-agg-ns"]{apply-agg-ns}

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

@subsubsection[#:tag "apply-stat-ns"]{apply-stat-ns}

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
