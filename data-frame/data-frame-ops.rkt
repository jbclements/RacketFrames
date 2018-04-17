#lang typed/racket

;(: data-frame+ (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame- (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame* (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame/ (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame% (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame== (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame!= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame< (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame> (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame<= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame>= (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame-pow (DataFrame DataFrame [#:on (Listof Symbol)] -> DataFrame))

;(: data-frame-abs (DataFrame [#:on (Listof Symbol)] -> DataFrame))

; DataFrame.apply(func[, axis, broadcast, ...])	Applies function along input axis of DataFrame.
; (: data-frame-apply func)

; DataFrame.applymap(func)	Apply a function to a DataFrame that is intended to operate elementwise, i.e.
; (: data-frame-applymap func)

; DataFrame.aggregate(func[, axis])	Aggregate using callable, string, dict, or list of string/callables
; (: data-frame-aggregate func)

; DataFrame.transform(func, *args, **kwargs)	Call function producing a like-indexed NDFrame
; (: data-frame-transform func)

; DataFrame.groupby([by, axis, level, ...])	Group series using mapper (dict or key function, apply given function to group, return result as series) or by a series of columns.
; (: data-frame-groupby [#:by (Listof Symbol)])
