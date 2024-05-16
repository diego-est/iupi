#lang plait

(require "utilities.rkt")
(require "parser.rkt")
(require "types.rkt")
(require "combinators.rkt")

;----- Language Implementation -----;
; Evaluates an Operation and returns its Color value
;(define (eval-op [op : Operation]) : Color
;  (type-case Operation op
;    [(add c e) (color-add c (eval e))]
;    [(subtract c e) (color-subtract c (eval e))]
;    [(multiply c e) (color-multiply c (eval e))]
;    [(divide c e) (color-divide c (eval e))]
;    [(interpolate c e f) (color-interpolate c (eval e) f)]
;    [(value-invert e) (color-value-invert (eval e))]
;    [(linear-invert e) (color-linear-invert (eval e))]
;    [(hue-shift e n) (color-hue-shift (eval e) n)]
;    [(max c e) (color-max c (eval e))]
;    [(min c e) (color-min c (eval e))]))

; Evaluates an Expression and returns its Color value
;(define (eval [e : Expr]) : Color
;  (type-case Expr e
;    [(operation op) (eval-op op)]
;    [(color c) c]))

; This is shorthand to define a binary operation that evaluates a function f
; on each field of an RGB Color
; Below the definition of this function there are several examples.
;(define (binary-color-op [c1 : Color] [c2 : Color] [f : (Number Number -> Number)]) : Color
;  (normalize-color (rgb-color
;		     (f (red c1) (red c2))
;		     (f (green c1) (green c2))
;		     (f (blue c1) (blue c2)))))

;----- Language Functionality -----;
; Add two colors
;(define (color-add [c1 : Color] [c2 : Color]) : Color
;  (binary-color-op c1 c2 +))

; Multiply two colors
;(define (color-multiply [c1 : Color] [c2 : Color]) : Color
;  (binary-color-op c1 c2 (λ (n m) (floor (* n m)))))

; Divide two colors (will floor their values and return 0 if divide by 0)
;(define (color-divide [c1 : Color] [c2 : Color]) : Color
;  (binary-color-op c1 c2 (λ (n m) (if (= m 0) 0 (floor (/ n m))))))

; Subtract two colors
;(define (color-subtract [c1 : Color] [c2 : Color]) : Color
;  (binary-color-op c1 c2 -))

; Invert the luminosity of a color
;(define (color-value-invert [c : Color]) : Color
;  c)

; Invert each RGB value of a color
;(define (color-linear-invert [c : Color]) : Color
;  (color-subtract [rgb-color 255 255 255] c))

; Interpolate a color between two other colors
;(define (color-interpolate [c1 : Color] [c2 : Color] [percent : Number]) : Color
;  (color-add [color-multiply {color-subtract c2 c1} {rgb-color percent percent percent}] c1))

; Choose the color with larger values
;(define (color-max [c1 : Color] [c2 : Color]) : Color
;  c1)

; Choose the color with smaller values
;(define (color-min [c1 : Color] [c2 : Color]) : Color
;  c1)

; Convert HSVColor to RGB
;(define (hsv->rgb [c : HSVColor]) : Color
;  (let [(h (hue c)) (s (saturation c)) (v (value c))]
;    (let* [(chroma (* v s))
;	   (hp (/ h 60))
;	   (x (* chroma (- 1 (abs (- (modulo hp 2) 1)))))
;           (c1 [cond
;                 [(< hp 1) (rgb-color chroma x 0)]
;                 [(< hp 2) (rgb-color x chroma 0)]
;                 [(< hp 3) (rgb-color 0 chroma x)]
;                 [(< hp 4) (rgb-color 0 x chroma)]
;                 [(< hp 5) (rgb-color x 0 chroma)]
;                 [else (rgb-color chroma 0 x)]])
;	   (m (- v chroma))
;	   (r (+ (red c1) m))
;	   (g (+ (green c1) m))
;	   (b (+ (blue c1) m))] (rgb-color r g b))))

; Convert RGB color to HSV color
;(define (rgb->hsv [c : Color]) : HSVColor
;  (let* [(r (/ (red c) 255))
;         (g (/ (green c) 255))
;         (b (/ (blue c) 255))
;         (x+ (maxel (list r g b)))
;	 (x- (minel (list r g b)))
;	 (v x+)
;	 (c (- x+ x-))
;	 (l (/ (+ x+ x-) 2))
;         (h [cond
;              [(= c 0) 0]
;              [(= v r) (modulo (* 60 (/ (- g b) c)) 6)]
;              [(= v g) (* 60 (+ (/ (- b r) c) 2))]
;              [(= v b) (* 60 (+ (/ (- r g) c) 4))]
;              [else 0]])
;         (s (if (= v 0) 0 (/ c v)))
;	 ] (hsv-color h s v)))

;----- The Great Parser -----;
(define (p-digit [s : String]) : (ParseResult Digit)
  (do ((or/p (list (char/p #\1)
               (char/p #\2)
               (char/p #\3)
               (char/p #\4)
               (char/p #\5)
               (char/p #\6)
               (char/p #\7)
               (char/p #\8)
               (char/p #\9)
               (char/p #\0))) s)
    (λ (result) (let [(char (snd result)) (cdr (fst result))] (return cdr (digit (char->num char)))))))

(define (p-digits [s : String]) : (ParseResult Digits)
  (do ((many1/p p-digit) s)
    (λ (result) (let [(digitlist (snd result)) (cdr (fst result))] (return cdr (digitlist->digits digitlist))))))

(define (p-decimals [s : String]) : (ParseResult Decimals)
  (do ((many1/p p-digit) s)
    (λ (result) (let [(digitlist (snd result)) (cdr (fst result))] (return cdr (digitlist->decimals digitlist))))))

(define (p-float [s : String]) : (ParseResult Float)
  (do ((right/p (char/p #\.) p-decimals) s)
    (λ (result) (let* [(decimals (snd result)) (cdr (fst result))] (return cdr (float decimals))))))

(define (p-color [s : String]) : (ParseResult RGBColor)
  (do ((right/p (char/p #\() p-digits) s)
    (λ (result1) (let [(red (snd result1)) (cdr1 (fst result1))]
                   (do ((right/p (char/p #\,) p-digits) cdr1)
                     (λ (result2) (let [(green (snd result2)) (cdr2 (fst result2))]
                                    (do ((left/p (right/p (char/p #\,) p-digits) (char/p #\))) cdr2)
                                      (λ (result3) (let [(blue (snd result3)) (cdr3 (fst result3))]
                                                     (return cdr3 (rgbcolor red green blue))))))))))))

(define (p-unary-operation [s : String]) : (ParseResult UnaryOperation)
  (do ((or/p (list (seq/p (string/p "<^>") p-lang) (seq/p (string/p "<|>") p-lang))) s)
    (λ (result) (let [(op-string (fst (snd result))) (lang (snd (snd result))) (cdr (fst result))]
                  (return cdr (if (string=? op-string "<^>")
                                  (value-invert lang)
                                  (linear-invert lang)))))))

(define (p-operator-type [s : String]) : (ParseResult OperatorType)
  (type-case (ParseResult String) ((or/p (list
                                          (string/p "+")
                                          (string/p "-")
                                          (string/p "*")
                                          (string/p "/")
                                          (string/p "^")
                                          (string/p "!"))) s)
    [(ok result) (let [(str (snd result)) (cdr (fst result))] (return cdr (string-type str)))]
    [(err) (do (p-float s)
             (λ (result) (let [(n (snd result)) (cdr (fst result))] (return cdr (floating-type n)))))]))

(define (p-binary-operation [s : String]) : (ParseResult BinaryOperation)
  (do ((seq/p p-color (seq/p p-operator-type p-expr)) s)
    (λ (result) (let* [(col-type-expr (snd result)) (col (fst col-type-expr)) (op-type (fst (snd col-type-expr))) (expr (snd (snd col-type-expr))) (cdr (fst result))]
                  (return cdr (type-case OperatorType op-type
                    [(floating-type fp) (interpolate col expr fp)]
                    [(string-type str) [cond
                                    [(string=? str "+") (add col expr)]
                                    [(string=? str "-") (subtract col expr)]
                                    [(string=? str "*") (multiply col expr)]
                                    [(string=? str "/") (divide col expr)]
                                    [(string=? str "^") (max col expr)]
                                    [(string=? str "!") (min col expr)]]]))))))

(define (p-expr [s : String]) : (ParseResult Expr)
  (type-case (ParseResult BinaryOperation) (p-binary-operation s)
    [(ok result) (let [(bin-op (snd result)) (cdr (fst result))]
                   (return cdr (operation bin-op)))]
    [(err) (type-case (ParseResult RGBColor) (p-color s)
             [(ok result) (let [(col (snd result)) (cdr (fst result))]
                            (return cdr (color col)))]
             [(err) (err)])]))

(define (p-lang [s : String]) : (ParseResult Language)
  (type-case (ParseResult UnaryOperation) (p-unary-operation s)
    [(ok result) (let [(un-op (snd result)) (cdr (fst result))]
                   (return cdr (unary-operation un-op)))]
    [(err) (type-case (ParseResult BinaryOperation) (p-binary-operation s)
             [(ok result) (let [(bin-op (snd result)) (cdr (fst result))]
                            (return cdr (binary-operation bin-op)))]
             [(err) (type-case (ParseResult RGBColor) (p-color s)
                      [(ok result) (let [(col (snd result)) (cdr (fst result))]
                                     (return cdr (lang-color col)))]
                      [(err) (err)])])]))

; Parser that parses many operations
;  (define (many1/p-op [s : String]) : (ParseResult BinaryOperation)
;    (do (p-op s)
;      (λ (result) (do ((many1/p (seq/p ops p-color)) (fst result))
;		      (λ (result2) (return (fst result2) (concat-op-list (snd result) (snd result2))))))))

; From a list of (character, color) concatenates all the operations and returns
; the underlying operation of all of those operations.
;(define (concat-op-list [first : BinaryOperation] [lst : (Listof (Char * Color))]) : BinaryOperation
;  (foldl (λ (char-color acc)
;           [let ([ch (fst char-color)] [col (snd char-color)])
;             [cond
;               [(char=? ch #\+) (add col (operation acc))]
;               [(char=? ch #\*) (multiply col (operation acc))]
;               [(char=? ch #\/) (divide col (operation acc))]
;               [(char=? ch #\-) (subtract col (operation acc))]]]) first lst))
; TODO: Add other operations
;(define (the-great-parser [s : String]) : Expr)

;; some tests
;((fmap (const #\a) (char/p #\n)) "nice")
;(map char/p (string->list "null"))
;((m-join (char/p #\a) (char/p #\p)) "apple")
;((m-prepend (m-join (char/p #\a) (char/p #\p)) (char/p #\p)) "apple")

(p-number "123")
;(p-color "(123,54,42)")

;(p-op "(255,0,42)+(42,42,42)")
;(do (p-op "(255,0,42)+(42,42,42)")
;  (λ (op) (return (fst op) (eval-op (snd op)))))
;(p-op "(255,0,42)*(42,42,42)")
;(p-op "(255,0,42)-(42,42,42)")
;(p-op "(255,0,42)/(42,42,42)")

;(eval-op (add (grayscale-color 1) (operation (add (grayscale-color 2) (color (rgb-color 1 2 3))))))

;(many1/p-op "(255,0,42)+(42,42,42)*(456,4,56)/(2,0,0)")

;(do (many1/p-op "(255,0,42)+(42,42,42)*(456,345,1000000)/(2,0,0)")
;  (λ (op) (return (fst op) (eval-op (snd op)))))

(p-lang "<|><^>(400,59,0)/(1,2,3).49(7,7,7)")
