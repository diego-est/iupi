#lang plait

;----- Language Types -----;
; General Expression type
(define-type Expr
  [operation (op : Operation)]
  [color (c : Color)])

; Operation type
(define-type Operation
  [add (color : Color) (e : Expr)]
  [subtract (color : Color) (e : Expr)]
  [multiply (color : Color) (e : Expr)]
  [divide (color : Color) (e : Expr)]
  [value-invert (e : Expr)]
  [linear-invert (e : Expr)]
  [interpolate (color : Color) (e : Expr) (percent : Number)]
  [hue-shift (e : Expr) (shift : Number)]
  [max (color : Color) (e : Expr)]
  [min (color : Color) (e : Expr)])

; Color type
(define-type Color
  [rgb-color (red : Number) (green : Number) (blue : Number)]
  [grayscale-color (n : Number)])

;----- Parser Types -----;
(define-type (ParseResult 'a)
  [ok (r : (String * 'a))]
  [err])

(define-type-alias (Parser 'a)
  (String -> (ParseResult 'a)))
