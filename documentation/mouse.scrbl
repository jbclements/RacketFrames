#lang scribble/manual

(racketblock
 (define (loop x)
   (loop (not x))))

