#lang plait

;----- Language Types -----;
; Language type :DONE
(define-type Language
  [binary-operation (op : BinaryOperation)]
  [unary-operation (op : UnaryOperation)]
  [lang-color (c : RGBColor)])

; General Expression type :DONE
(define-type Expr
  [operation (op : BinaryOperation)]
  [color (c : RGBColor)])

; BinaryOperation type :DONE
(define-type BinaryOperation
  [add (color : RGBColor) (e : Expr)]
  [subtract (color : RGBColor) (e : Expr)]
  [multiply (color : RGBColor) (e : Expr)]
  [divide (color : RGBColor) (e : Expr)]
  [interpolate (color : RGBColor) (e : Expr) (percent : Float)]
  [max (color : RGBColor) (e : Expr)]
  [min (color : RGBColor) (e : Expr)])

; OperatorType type :DONE
(define-type OperatorType
  [floating-type (n : Float)]
  [string-type (s : String)])

; UnaryOperation type :DONE
(define-type UnaryOperation
  [value-invert (l : Language)]
  [linear-invert (l : Language)])

; Float type :DONE
(define-type Float
  [float (ds : Decimals)])

; Digits type :DONE
(define-type Digits
  [number (first : Digit) (rest : Digits)]
  [empty-digit])

; Digit type :DONE
(define-type Digit
  [digit (n : Number)])

; Decimals type :DONE
(define-type Decimals
  [decimals (first : Digit) (rest : Decimals)]
  [empty-decimal])

; RGBColor type :DONE
(define-type RGBColor
  [rgbcolor (red : Digits) (blue : Digits) (green : Digits)])

; HSVColor type :DONE
(define-type HSVColor
  [hsvcolor (hue : Number) (saturation : Number) (value : Number)])

; TODO: DEPRECATED
; Color type
;(define-type-alias (Color) (Number * (Number * Number)))

; TODO: DEPRECATED
;(define-type-alias (HSVColor) (Number * (Number * Number)))

;----- Parser Types -----;
; TODO: document
(define-type (ParseResult 'a)
  [ok (r : (String * 'a))]
  [err])

; TODO: document
(define-type-alias (Parser 'a)
  (String -> (ParseResult 'a)))
