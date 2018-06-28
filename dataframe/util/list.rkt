#lang typed/racket

(provide: 
 [zip (All (A B) (Listof A) (Listof B) -> (Listof (Pair A B)))])

(: zip (All (A B) ((Listof A) (Listof B) -> (Listof (Pair A B)))))
(define (zip lsta lstb)
  (let: loop : (Listof (Pair A B)) ((as : (Listof A) lsta) 
                                    (bs : (Listof B) lstb) 
                                    (accum : (Listof (Pair A B))'()))
    (if (or (null? as)
            (null? bs))
        (reverse accum)
        (loop (cdr as) (cdr bs) (cons (cons (car as) (car bs)) accum)))))
