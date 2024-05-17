#lang plait

;----- Language Types -----;
; Language type
(define-type Language
  [binary-operation (op : BinaryOperation)]
  [unary-operation (op : UnaryOperation)]
  [lang-color (c : RGBColor)])

; General Expression type
(define-type Expr
  [operation (op : BinaryOperation)]
  [color (c : RGBColor)])

; BinaryOperation type
(define-type BinaryOperation
  [add (color : RGBColor) (e : Expr)]
  [subtract (color : RGBColor) (e : Expr)]
  [multiply (color : RGBColor) (e : Expr)]
  [divide (color : RGBColor) (e : Expr)]
  [interpolate (color : RGBColor) (e : Expr) (percent : Float)]
  [max (color : RGBColor) (e : Expr)]
  [min (color : RGBColor) (e : Expr)])

; OperatorType type
(define-type OperatorType
  [floating-type (n : Float)]
  [string-type (s : String)])

; UnaryOperation type
(define-type UnaryOperation
  [value-invert (l : Language)]
  [linear-invert (l : Language)])

; Float type
(define-type Float
  [float (ds : Decimals)])

; Digits type
(define-type Digits
  [number (first : Digit) (rest : Digits)]
  [empty-digit])

; Digit type
(define-type Digit
  [digit (n : Number)])

; Decimals type
(define-type Decimals
  [decimals (first : Digit) (rest : Decimals)]
  [empty-decimal])

; RGBColor type
(define-type RGBColor
  [rgbcolor (red : Digits) (blue : Digits) (green : Digits)])

; HSVColor type
(define-type HSVColor
  [hsvcolor (hue : Number) (saturation : Number) (value : Number)])

;----- Parser Types -----;
; This is simply a shorthand including either an (ok) variant, indicating that
; something of type ('a) was parsed succesfully. The (err) variant indicates
; that something of type ('a) could not be parsed. The pair inside the (ok)
; variant includes ('a) the thing that was just parsed and a (String), the rest
; of the source code that has not been consumed yet.
(define-type (ParseResult 'a)
  [ok (r : (String * 'a))]
  [err])

; An alias for a parser type. A parser is a function that takes a (String) and
; produces a (ParseResult) of ('a).
(define-type-alias (Parser 'a)
  (String -> (ParseResult 'a)))
