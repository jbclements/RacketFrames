#lang scribble/base
@(require scribble/core
          scribble/manual)

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

@subsection[#:style 'toc]{Methods}

@local-table-of-contents[]

@subsubsection[#:tag "deepest"]{Subsubsection in ``H5''}

@"\U2190" This page has no on-this-page panel in a multi-page
rendering, because there are no numbered subsections, but it has three
levels shown in the table-of-contents panel.

@subsubsub*section{``SSubSubSubSection''}


@subsection{Second Subsection in ``H4''}


@; ======================================================================
@section[#:tag "no-toc" #:style 'no-toc]{Suppressed ToC Panel}

In multi-page rendering, this page has no gobal table-of-contents
panel, because it is suppressed with @racket['no-toc].

@subsection{Subsection}

@subsection{Another Subsection}


@; ======================================================================
@section[#:tag "all-non-sec"]{Non-Section On-This-Page Links}

This section has only non-section targets in the on-this-page
panel of a multi-page rendering.
Here is the target for the
@toc-target-element[#f @elem{``tocsublink'' 1} `(demo (prefixable "non-sec 1"))]
link.
Here is the target for the
@toc-target-element[#f @elem{``tocsublink'' 2} `(demo (prefixable "non-sec 2"))]
link.

Here is the target for the @as-index{``indexlink''} link in the
@seclink["doc-index"]{index} (where ``indexlink'' is used for the
index entry and not here).

@; ======================================================================
@section{Element Styles}

Some spans:

@itemlist[

 @item{@tt{``stt''}}

 @item{@elem[#:style 'roman]{``sroman''}}

 @item{@elem[#:style "slant"]{``slant''}}

 @item{@elem[#:style 'sf]{``ssanserif''}}

 @item{@smaller{``Smaller''}}

 @item{@larger{``Larger''}}

 @item{``hspace'' is used for forced @hspace[3] space}

 @item{``url'' is used for URLs: @url{http://racket-lang.org}}

 @item{@elem[#:style 'no-break]{``nobreak'', which is used to prevent
 line breaks anywhere in the element so that the element may run too
 far right}}

 @item{@italic{italic} directly sets @tt{font-style} to @tt{italic}}

 @item{@bold{bold} directly sets @tt{font-weight} to @tt{bold}}

 @item{@elem[#:style 'superscript]{superscript} directly sets
        @tt{vertical-align} to @tt{super} and @tt{font-size} to @tt{80%}.}

 @item{@elem[#:style 'subscript]{subscript} directly sets
        @tt{vertical-align} to @tt{sub} and @tt{font-size} to @tt{80%}.}

]

Link spans:

@itemlist[

 @item{@elemref[#:underline? #f '(prefixable "plain-target")]{``plainlink''}
       hyperlink to @elemtag['(prefixable "plain-target")]{here}}

 @item{@deftech{technical term} definitions are simply italicized by default}

 @item{@tech{technical term} references are in ``techoutside'', then ``techinside''}

]

@; ======================================================================
@section{Block Styles}

@nested{This paragraph is in a ``SubFlow'' @tt{<blockquote>}.}

@nested[#:style 'inset]{This paragraph is in a plain @tt{<blockquote>}.}

@nested[#:style 'code-inset]{This paragraph is in a ``SCodeFlow''
@tt{<blockquote>}.}

@nested[#:style 'vertical-inset]{This paragraph is in a
``SVInsetFlow'' @tt{<blockquote>}. This style is useful when space is
not normally included between blocks.}

@centered{This paragraph is in a ``SCentered'' @tt{<blockquote>}.}

@tabular[#:style 'boxed (list (list @t{A ``boxed'' table.}))]

@; ======================================================================
@section{Enumerations}

This one is unordered, so it uses @tt{<ul>}:

@itemlist[

 @item{six}

 @item{half-dozen}

]

This one is ordered, so it uses @tt{<ol>}:

@itemlist[#:style 'ordered

 @item{First}

 @item{Second

       @itemlist[#:style 'ordered

          @item{Second, first half}

          @item{Second, second half

                @itemlist[#:style 'ordered

                           @item{First half of that}

                           @item{Second half of that

                                 @itemlist[#:style 'ordered

                                    @item{Thin-slice start}

                                    @item{Thin-sliced end}

                                 ]}
                          ]}

                ]}

 @item{Third}
]

This one is ``compact'':

@itemlist[ #:style 'compact

 @item{six}

 @item{half-dozen}

]

This paragraph follows the enumeration above.

@; ======================================================================
@section{Paragraph Spacing}

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


@; ======================================================================
@section{Navigation Bars}

For multi-page rendering, this page will have a navigation bar at the
top and bottom. The bars are within ``maincolumn'' and ``main''.

The tap bar is in ``navsettop'', and the bottom one is in
``navsetbottom''.  Within those divs, ``navsetleft'' wraps content to
be left-aligned and ``navsetright'' wraps content to be right-aligned.

Links that are disabled (such as a next-page link on the last page)
are each in a span ``nonavigation''.

When a search box is included, then it is in ``searchform'' and then
``searchbox''. If no search box is included, then a ``nosearchform''
@tt{div} is used.

Finally, and not part of the nagivation bar, the bottom nagivation bar
is followed by a @tt{div} with the name
``contextindicator''. JavaScript code attached to the page copies the
@tt{ctxtname} query argument, if any, to the @tt{div} and makes it
visible.

@; ======================================================================
@index-section[]
