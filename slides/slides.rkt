#lang slideshow

(require slideshow/code)
(require slideshow/play)
(require plot)

 (play-n
 (lambda (n1 n2 n3)
   (vl-append
   (cellophane (t "Hello")
               (* n1 (- 1.0 n2)))
   (rectangle (* 10 n1) (* 10 n1))
   (arrow (* 30 n2) 0)
   (circle (* 10 n3))
   )
   )
 )

(slide
 (plot-pict (function sqr -1 1 #:label "y = x^2")))