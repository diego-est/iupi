#lang plait

(require "utilities.rkt")
(require "parser.rkt")
(require "types.rkt")
(require "combinators.rkt")

;----- Language Implementation -----;
; Converts a single digit into a number. Basically just returns the number
; contained inside the digit.
(define (digit->number [d : Digit]) : Number
  (type-case Digit d
    [(digit n) n]))

; Converts a series of digits into a number.
(define (digits->number [ds : Digits]) : Number
  (type-case Digits ds
    [(number first rest) (+ (digit->number first) (* 10 (digits->number rest)))]
    [(empty-digit) 0]))

; Converts a decimal into a number.
(define (decimals->number [dc : Decimals]) : Number
  (type-case Decimals dc
    [(decimals first rest) (/ (+ (digit->number first) (decimals->number rest)) 10)]
    [(empty-decimal) 0]))

; Converts a floating point type number into a number.
(define (float->number [fp : Float]) : Number
  (type-case Float fp
    [(float ds) (decimals->number ds)]))

;----- Language Functionality -----;
; Helper function to create binary operations.
(define (bin-op-builder [c1 : RGBColor] [c2 : RGBColor] [f : (Number Number -> Number)]) : RGBColor
  (type-case RGBColor c1
    [(rgbcolor red1 green1 blue1) (type-case RGBColor c2
                                    [(rgbcolor red2 green2 blue2) (let*
                                                                      [(r1 (digits->number red1))
                                                                       (r2 (digits->number red2))
                                                                       (g1 (digits->number green1))
                                                                       (g2 (digits->number green2))
                                                                       (b1 (digits->number blue1))
                                                                       (b2 (digits->number blue2))
                                                                       (r3 (mod256 (f r1 r2)))
                                                                       (g3 (mod256 (f g1 g2)))
                                                                       (b3 (mod256 (f b1 b2)))]
                                                                    (rgbcolor (number->digits r3) (number->digits g3) (number->digits b3)))])]))

; Add two colors
(define (color-add [c1 : RGBColor] [c2 : RGBColor]) : RGBColor
  (bin-op-builder c1 c2 +))

; Multiply two colors
(define (color-multiply [c1 : RGBColor] [c2 : RGBColor]) : RGBColor
  (bin-op-builder c1 c2 (λ (n m) (floor (* n m)))))

; Divide two colors (will floor their values and return 0 if divide by 0)
(define (color-divide [c1 : RGBColor] [c2 : RGBColor]) : RGBColor
  (bin-op-builder c1 c2 (λ (n m) (if (= m 0) 0 (floor (/ n m))))))

; Subtract two colors
(define (color-subtract [c1 : RGBColor] [c2 : RGBColor]) : RGBColor
  (bin-op-builder c1 c2 -))

; Invert the luminosity of a color
(define (color-value-invert [c1 : RGBColor]) : RGBColor
  (type-case HSVColor (rgb->hsv c1)
    [(hsvcolor h1 s1 v1)
    (let [(h2 h1) (s2 s1) (v2 (- 1 v1))] (hsv->rgb [hsvcolor h2 s2 v2]))]))

; Invert each RGB value of a color
(define (color-linear-invert [c : RGBColor]) : RGBColor
  (color-subtract [rgbcolor (number->digits 255) (number->digits 255) (number->digits 255)] c))

; Interpolate a color between two other colors
(define (color-interpolate [c1 : RGBColor] [c2 : RGBColor] [percent : Float]) : RGBColor
  (type-case HSVColor (rgb->hsv c1)
    [(hsvcolor h1 s1 v1) (type-case HSVColor (rgb->hsv c2)
                           [(hsvcolor h2 s2 v2)
                            (let* [(prcnt (float->number percent))
                                   (h3 (interpolate-num h1 h2 prcnt))
                                   (s3 (interpolate-num s1 s2 prcnt))
                                   (v3 (interpolate-num v1 v2 prcnt))] (hsv->rgb (hsvcolor h3 s3 v3)))])]))

; Sums all rgb values of a color
(define (sumar-color [color : RGBColor] ) : Number
   (+
    (+
    (mod256(digits->number(rgbcolor-red color)))
    (mod256(digits->number(rgbcolor-blue color))))
    (mod256(digits->number(rgbcolor-green color)))))

; Choose the color with larger values
(define (color-max [c1 : RGBColor] [c2 : RGBColor]) : RGBColor
      (let ((sumc1 (sumar-color c1))
            (sumc2 (sumar-color c2)))
      (cond ((> sumc1 sumc2) c1)
             (else c2))))

; Choose the color with smaller values
(define (color-min [c1 : RGBColor] [c2 : RGBColor]) : RGBColor
      (let ((sumc1 (sumar-color c1))
            (sumc2 (sumar-color c2)))
      (cond ((< sumc1 sumc2) c1)
             (else c2))))


; Convert HSVColor to RGB
(define (hsv->rgb [c : HSVColor]) : RGBColor
  (type-case HSVColor c
    [(hsvcolor hue saturation value) (let [(h hue) (s saturation) (v value)]
    (let* [(chroma (* v s))
	   (hp (/ h 60))
	   (x (if (even? (round hp)) 0 chroma))
           (c1 [cond
                 [(< hp 1) (list chroma x 0)]
                 [(< hp 2) (list x chroma 0)]
                 [(< hp 3) (list 0 chroma x)]
                 [(< hp 4) (list 0 x chroma)]
                 [(< hp 5) (list x 0 chroma)]
                 [else (list chroma 0 x)]])
	   (m (- v chroma))
	   (r (round (* 256 (+ (first c1) m))))
	   (g (round (* 256 (+ (second c1) m))))
	   (b (round (* 256 (+ (third c1) m))))] (rgbcolor (number->digits r) (number->digits g) (number->digits b))))]))

; Convert RGB color to HSV color
(define (rgb->hsv [c : RGBColor]) : HSVColor
  (type-case RGBColor c
    [(rgbcolor red green blue) (let*
        [(r (/ (digits->number red) 255))
         (g (/ (digits->number green) 255))
         (b (/ (digits->number blue) 255))
         (x+ (maxel (list r g b)))
	 (x- (minel (list r g b)))
	 (v x+)
	 (c (- x+ x-))
	 (l (/ (+ x+ x-) 2))
         (h [cond
              [(= c 0) 0]
              [(= v r) (* 60 (modulo (round (/ (- g b) c)) 6))]
              [(= v g) (* 60 (+ (/ (- b r) c) 2))]
              [(= v b) (* 60 (+ (/ (- r g) c) 4))]
              [else 0]])
         (s (if (= v 0) 0 (/ c v)))
	 ] (hsvcolor h s v))]))

;----- The Parsers -----;
; Parser that parses a single digit.
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

; Parser that parses many digits. Will continue consuming digits from the input
; string until the parser fails. In which case it will return the sequence of
; digits that have been parsed already.
(define (p-digits [s : String]) : (ParseResult Digits)
  (do ((many1/p p-digit) s)
    (λ (result) (let [(digitlist (snd result)) (cdr (fst result))] (return cdr (digitlist->digits digitlist))))))

; Parser that parses decimal numbers.
(define (p-decimals [s : String]) : (ParseResult Decimals)
  (do ((many1/p p-digit) s)
    (λ (result) (let [(digitlist (snd result)) (cdr (fst result))] (return cdr (digitlist->decimals digitlist))))))

; Parser that parses an entire floating point number.
(define (p-float [s : String]) : (ParseResult Float)
  (do ((right/p (char/p #\.) p-decimals) s)
    (λ (result) (let* [(decimals (snd result)) (cdr (fst result))] (return cdr (float decimals))))))

; Parser that parses an entire color. Returns a (ParseResult RGBColor) where
; each component of the RGBColor is of type Digits.
(define (p-color [s : String]) : (ParseResult RGBColor)
  (do ((right/p (char/p #\() p-digits) s)
    (λ (result1) (let [(red (snd result1)) (cdr1 (fst result1))]
                   (do ((right/p (char/p #\,) p-digits) cdr1)
                     (λ (result2) (let [(green (snd result2)) (cdr2 (fst result2))]
                                    (do ((left/p (right/p (char/p #\,) p-digits) (char/p #\))) cdr2)
                                      (λ (result3) (let [(blue (snd result3)) (cdr3 (fst result3))]
                                                     (return cdr3 (rgbcolor red green blue))))))))))))

; Parser that parses a unary operation. It will also parse the rest of the
; source code and use it as an argument for the unary operation argument.
(define (p-unary-operation [s : String]) : (ParseResult UnaryOperation)
  (do ((or/p (list (seq/p (string/p "<^>") p-lang) (seq/p (string/p "<|>") p-lang))) s)
    (λ (result) (let [(op-string (fst (snd result))) (lang (snd (snd result))) (cdr (fst result))]
                  (return cdr (if (string=? op-string "<^>")
                                  (value-invert lang)
                                  (linear-invert lang)))))))

; Parser that parses an operator symbol and color. This is a helper function for
; (p-binary-operation). This function is necessary because the symbol in the
; middle can either be a symbol string or a floating-point number. Wrapping the
; result in OperatorType means that the data gathered from this parse can then
; be type-checked by plait correctly.
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
             (λ (result) (let [(n (snd result)) (cdr (fst result))](return cdr (floating-type n)))))]))

; Parser that parses a binary operation. The recursive nature of this parser
; means that it will continue parsing the input string until it cannot continue.
; In which case it starts evaluating the expression.
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

; Parser that parses an entire expression. This type is necessary to separate
; the (UnaryOperation)'s from the (BinaryOperation)'s.
(define (p-expr [s : String]) : (ParseResult Expr)
  (type-case (ParseResult BinaryOperation) (p-binary-operation s)
    [(ok result) (let [(bin-op (snd result)) (cdr (fst result))]
                   (return cdr (operation bin-op)))]
    [(err) (type-case (ParseResult RGBColor) (p-color s)
             [(ok result) (let [(col (snd result)) (cdr (fst result))]
                            (return cdr (color col)))]
             [(err) (err)])]))

; Parser that parses the entire language.
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

;----- Evaluation -----;
; Evaluates a parsed language and returns the corresponding (RGBColor) that
; would result if the expression was computed.
(define (evaluate [src : Language]) : RGBColor
  (type-case Language src
    [(binary-operation op) (eval-binary-operation op)]
    [(unary-operation op) (eval-unary-operation op)]
    [(lang-color col) col]))

; Evaluates a unary operation.
(define (eval-unary-operation [op : UnaryOperation]) : RGBColor
  (type-case UnaryOperation op
    [(value-invert col) (color-value-invert (evaluate col))]
    [(linear-invert col) (color-linear-invert (evaluate col))]))

; Evaluates a binary operation.
(define (eval-binary-operation [op : BinaryOperation]) : RGBColor
  (type-case BinaryOperation op
    [(add col exp) (color-add col (eval-expr exp))]
    [(subtract col exp) (color-subtract col (eval-expr exp))]
    [(multiply col exp) (color-multiply col (eval-expr exp))]
    [(divide col exp) (color-divide col (eval-expr exp))]
    [(interpolate col exp percent) (color-interpolate col (eval-expr exp) percent)]
    [(max col exp) (color-max col (eval-expr exp))]
    [(min col exp) (color-min col (eval-expr exp))]))

; Evaluates an expression.
(define (eval-expr [e : Expr]) : RGBColor
  (type-case Expr e
    [(operation op) (eval-binary-operation op)]
    [(color col) col]))

;----- Tests -----;
(define accept1 "(0,000000,010)")
(define accept2 "<|><^>(0,0,0)")
(define accept3 "(0,0,0)/(0,0,0)")
(define accept4 "(400,100,200)+(10,10,10)/(2,2,2)")
(define accept5 "(0,0,00).32(0100,319,7880).0(400,100,200)")

(define deny1 "(0,0,0.0)")
(define deny2 "<|>(0,0,00)<^>(100,319,80)")
(define deny3 "(0,0,00)0.32(0100,319,7880)")
(define deny4 "(400,100,200)++(10,10,10)")
(define deny5 "(400,100,200)+")

; Testing function for strings.
(define (tester [s : String]) : RGBColor
  (type-case (ParseResult Language) (p-lang s)
    [(ok lang) (cond
                 [(not (string=? "" (fst lang))) (error 'incompleteParse (fst lang))]
                 [else (evaluate (snd lang))])]
    [(err) (error 'parseFailed "not a valid string")]))

"Accept 1:"
(tester accept1)
"Accept 2:"
(tester accept2)
"Accept 3:"
(tester accept3)
"Accept 4:"
(tester accept4)
"Accept 5:"
(tester accept5)

;"Deny 1:"
;(tester deny1)
;"Deny 2:"
;(tester deny2)
;"Deny 3:"
;(tester deny3)
;"Deny 4:"
;(tester deny4)
;"Deny 5:"
;(tester deny5)