#lang plait

(require "utilities.rkt")
(require "parser.rkt")
(require "types.rkt")
(require "combinators.rkt")

;----- Language Implementation -----;
; Evaluates an Operation and returns its Color value
(define (eval-op [op : Operation]) : Color
  (type-case Operation op
    [(add c e) (color-add c (eval e))]
    [(subtract c e) (color-subtract c (eval e))]
    [(multiply c e) (color-multiply c (eval e))]
    [(divide c e) (color-divide c (eval e))]
    [(interpolate c e f) (color-interpolate c (eval e) f)]
    [(value-invert e) (color-value-invert (eval e))]
    [(linear-invert e) (color-linear-invert (eval e))]
    [(hue-shift e n) (color-hue-shift (eval e) n)]
    [(max c e) (color-max c (eval e))]
    [(min c e) (color-min c (eval e))]))

; Evaluates an Expression and returns its Color value
(define (eval [e : Expr]) : Color
  (type-case Expr e
    [(operation op) (eval-op op)]
    [(color c) c]))

; This is shorthand to define a binary operation that evaluates a function f
; on each field of an RGB Color
; Below the definition of this function there are several examples.
(define (binary-color-op [c1 : Color] [c2 : Color] [f : (Number Number -> Number)]) : Color
  (normalize-color (type-case Color c1
    [(rgb-color r1 g1 b1) (type-case Color c2
                            [(rgb-color r2 g2 b2) (rgb-color (f r1 r2) (f g1 g2) (f b1 b2))]
                            [(grayscale-color m) (rgb-color (f r1 m) (f g1 m) (f b1 m))])]
    [(grayscale-color n) (type-case Color c2
                           [(grayscale-color m) (grayscale-color (f n m))]
                           [(rgb-color r g b) (rgb-color (f r n) (f g n) (f b n))])])))

;----- Language Functionality -----;
; Add two colors
(define (color-add [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 +))

; Multiply two colors
(define (color-multiply [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 (λ (n m) (floor (* n m)))))

; Divide two colors (will floor their values and return 0 if divide by 0)
(define (color-divide [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 (λ (n m) (if (= m 0) 0 (floor (/ n m))))))

; Subtract two colors
(define (color-subtract [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 -))

; Invert the luminosity of a color
(define (color-value-invert [c : Color]) : Color
  c)

; Invert each RGB value of a color
(define (color-linear-invert [c : Color]) : Color
  (color-subtract [rgb-color 255 255 255] c))

; Interpolate a color between two other colors
(define (color-interpolate [c1 : Color] [c2 : Color] [percent : Number]) : Color
  (color-add [color-multiply {color-subtract c2 c1} {rgb-color percent percent percent}] c1))

; Shift the hue of a color
(define (color-hue-shift [c : Color] [shift : Number]) : Color
  c)

; Choose the color with larger values
(define (color-max [c1 : Color] [c2 : Color]) : Color
  c1)

; Choose the color with smaller values
(define (color-min [c1 : Color] [c2 : Color]) : Color
  c1)

; Normalize a color modulo 256
(define (normalize-color [c : Color]) : Color
  (type-case Color c
    [(rgb-color r g b) (rgb-color (mod256 r) (mod256 g) (mod256 b))]
    [(grayscale-color n) (grayscale-color (mod256 n))]))

;----- The Great Parser -----;
; Parser that parses many operations
(define (many1/p-op [s : String]) : (ParseResult Operation)
  (do (p-op s)
    (λ (result) (do ((many1/p (seq/p ops p-color)) (fst result))
                  (λ (result2) (p-result (fst result2) (concat-op-list (snd result) (snd result2))))))))

; From a list of (character, color) concatenates all the operations and returns
; the underlying operation of all of those operations.
(define (concat-op-list [first : Operation] [lst : (Listof (Char * Color))]) : Operation
  (foldl (λ (char-color acc)
           [let ([ch (fst char-color)] [col (snd char-color)])
             [cond
               [(char=? ch #\+) (add col (operation acc))]
               [(char=? ch #\*) (multiply col (operation acc))]
               [(char=? ch #\/) (divide col (operation acc))]
               [(char=? ch #\-) (subtract col (operation acc))]]]) first lst))
; TODO: Add other operations

;; some tests
;((fmap (const #\a) (char/p #\n)) "nice")
;(map char/p (string->list "null"))
;((m-join (char/p #\a) (char/p #\p)) "apple")
;((m-prepend (m-join (char/p #\a) (char/p #\p)) (char/p #\p)) "apple")

(p-number "123")
(p-color "(123)")
(p-color "(123,54,42)")

(p-add "(123)+(579)")

(p-op "(255,0,42)+(42,42,42)")
(do (p-op "(255,0,42)+(42,42,42)")
  (λ (op) (p-result (fst op) (eval-op (snd op)))))
(p-op "(123)*(579)")
(p-op "(255,0,42)*(42,42,42)")
(p-op "(123)-(579)")
(p-op "(255,0,42)-(42,42,42)")
(p-op "(123)/(579)")
(p-op "(255,0,42)/(42,42,42)")

;(eval-op (add (grayscale-color 1) (operation (add (grayscale-color 2) (color (rgb-color 1 2 3))))))

(many1/p-op "(255,0,42)+(42,42,42)*(456)/(2)")

(do (many1/p-op "(255,0,42)+(42,42,42)*(456)/(2)")
  (λ (op) (p-result (fst op) (eval-op (snd op)))))
