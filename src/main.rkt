#lang typed/racket

;; looks terrible, i know, but i whanna show more syntax and simple hello, world is too ;; easy

;; Using higher-order occurrence typing
(define-type SrN (U String Number))
(: tog ((Listof SrN) -> String))
(define (tog l)
  (apply string-append
         (filter string? l)))
(tog (list 5 "hello "
           1/2 "from Racket" (sqrt -1)))