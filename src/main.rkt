#lang plait
;; Functional utilities
(define (id x) x)

(define (const a)
  (λ (b) a))

(define (flip a)
  (λ (x y) (a y x)))

(define (iota n)
  (build-list n id))

(define (mod256 n)
  (modulo n 256))

(define (char->string [ch : Char]) : String
  (list->string (list ch)))

(define (do [p : (ParseResult 'a)] [f : ((String * 'a) -> (ParseResult 'b))]) : (ParseResult 'b)
  (type-case (ParseResult 'a) p
    [(ok res) (f res)]
    [(err) (err)]))

(define (p-result [left : String] [right : 'a]) : (ParseResult 'a)
  (ok (pair left right)))

;; Constructs a pair 1 argument at a time
(define (pair-curried [x : 'a]) : ('b -> ('a * 'b))
  (λ (y)
    (pair x y)))

;; Zips two lists (its length is the length of the smaller list)
(define (zip [l1 : (Listof 'a)] [l2 : (Listof 'b)]) : (Listof ('a * 'b))
  (if (or (empty? l1) (empty? l2))
      '()
      (cons (pair (first l1) (first l2)) (zip (rest l1) (rest l2)))))

;; Applies the leftside of a pair to the rightside
(define (apply-pair [p : (('a -> 'b) * 'a)]) : 'b
  ((fst p) (snd p)))

;; Maps apply-pair to a list
(define (apply-pair-list [lst : (Listof (('a -> 'b) * 'a))]) : (Listof 'b)
  (map (λ (p) (apply-pair p)) lst))

;; Gets the first Char from a String
(define (first-char [s : String]) : (Optionof Char)
  (if (empty? (string->list s))
      [none]
      [some (first (string->list s))]))

;; Returns the tail of a String
(define (string-tail [s : String]) : String
  (if (empty? (string->list s))
      ""
      [list->string (rest (string->list s))]))

(define (char->num [c : Char]) : Number
  (cond [(char=? c #\1) 1]
        [(char=? c #\2) 2]
        [(char=? c #\3) 3]
        [(char=? c #\4) 4]
        [(char=? c #\5) 5]
        [(char=? c #\6) 6]
        [(char=? c #\7) 7]
        [(char=? c #\8) 8]
        [(char=? c #\9) 9]
        [(char=? c #\0) 0]
      ))

;; Parser Stuff
;; Parse result type
(define-type (ParseResult 'a)
  [ok (r : (String * 'a))]
  [err])

;; Parser type
(define-type-alias (Parser 'a) (String -> (ParseResult 'a)))

;; Language implementation
;; Language types

(define-type Expr
  [operation (op : Operation)]
  [color (c : Color)])

;(define (eval [e : Expr]) : Color
;  (type-case Expr e
;    [(operation op rest) ()]

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

(define (eval [e : Expr]) : Color
  (type-case Expr e
    [(operation op) (eval-op op)]
    [(color c) c]))

(define-type Color
  [rgb-color (red : Number) (green : Number) (blue : Number)]
  [grayscale-color (n : Number)])

; Setup for operations
(define (binary-color-op [c1 : Color] [c2 : Color] [f : (Number Number -> Number)]) : Color
  (normalize-color (type-case Color c1
    [(rgb-color r1 g1 b1) (type-case Color c2
                            [(rgb-color r2 g2 b2) (rgb-color (f r1 r2) (f g1 g2) (f b1 b2))]
                            [(grayscale-color m) (rgb-color (f r1 m) (f g1 m) (f b1 m))])]
    [(grayscale-color n) (type-case Color c2
                           [(grayscale-color m) (grayscale-color (f n m))]
                           [(rgb-color r g b) (rgb-color (f r n) (f g n) (f b n))])])))  

; Definition of all operations
(define (color-add [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 +))

(define (color-multiply [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 (λ (n m) (floor (* n m)))))

(define (color-divide [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 (λ (n m) (if (= m 0) 0 (floor (/ n m))))))

(define (color-subtract [c1 : Color] [c2 : Color]) : Color
  (binary-color-op c1 c2 -))

(define (color-value-invert [c : Color]) : Color
  c)

(define (color-linear-invert [c : Color]) : Color
  (color-subtract [rgb-color 255 255 255] c))

(define (color-interpolate [c1 : Color] [c2 : Color] [percent : Number]) : Color
  (color-add [color-multiply {color-subtract c2 c1} {rgb-color percent percent percent}] c1))

(define (color-hue-shift [c : Color] [shift : Number]) : Color
  c)

(define (color-max [c1 : Color] [c2 : Color]) : Color
  c1)

(define (color-min [c1 : Color] [c2 : Color]) : Color
  c1)

(define (normalize-color [c : Color]) : Color
  (type-case Color c
    [(rgb-color r g b) (rgb-color (mod256 r) (mod256 g) (mod256 b))]
    [(grayscale-color n) (grayscale-color (mod256 n))]))

;; char/p implementation
(define (char/p [c : Char]) : (Parser Char)
  (λ (s)
    (type-case (Optionof Char) (first-char s)
      [(some x) (if (char=? x c) (ok (pair (string-tail s) c)) (err))]
      [(none) (err)])))

(define (run-parser [p : (Parser 'a)] [s : String]) (p s))

;; The following functor and applicative implementations for char/p were taken
;;  from the prelude implementation of haskell at:
;;  https://hackage.haskell.org/package/base-4.19.1.0/docs/src/GHC.Base.html

;; char/p functor
(define (fmap [f : ('a -> 'b)] [p : (Parser 'a)]) : (Parser 'b)
  (λ (input)
    (do (p input)
      (λ (x) (ok (pair (fst x) (f (snd x))))))))

;; char/p applicative
(define (pure [x : 'a]) : (Parser 'a)
  (λ (input)
    (ok (pair input x))))

(define (seq-ap [p1 : (Parser ('a -> 'b))] [p2 : (Parser 'a)]) : (Parser 'b)
  (λ (input)
    (type-case (ParseResult ('a -> 'b)) (p1 input)
      [(ok f) (do (p2 input)
                (λ (y) (p-result (fst y) ((snd f) (snd y)))))]
      [(err) (err)])))

(define (m-join [p1 : (Parser Char)] [p2 : (Parser Char)]) : (Parser String)
  (λ (input)
    (type-case (ParseResult Char) (p1 input)
      [(ok r1) ;(do (p2 (fst r1))
               ;  (λ (r2) (p-result (fst r2) (list->string (list (snd r1) (snd r2))))))]

       (type-case (ParseResult Char) (p2 (fst r1))
                   [(ok r2) (ok (pair (fst r2) (list->string (list (snd r1) (snd r2)))))]
                   [(err) (err)])]
      [(err) (err)])))

;; char/p monoid
(define (m-append [p1 : (Parser Char)] [p2 : (Parser String)]) : (Parser String)
  (λ (input)
    (type-case (ParseResult Char) (p1 input)
      [(ok r1) (type-case (ParseResult String) (p2 (fst r1))
                   [(ok r2) (ok (pair (fst r2) (string-append (snd r2) (list->string (list (snd r1))))))]
                   [(err) (err)])]
      [(err) (err)])))

(define (m-prepend [p1 : (Parser String)] [p2 : (Parser Char)]) : (Parser String)
  (λ (input)
    (type-case (ParseResult String) (p1 input)
      [(ok r1) (type-case (ParseResult Char) (p2 (fst r1))
                   [(ok r2) (ok (pair (fst r2) (string-append (snd r1) (list->string (list (snd r2))))))]
                   [(err) (err)])]
      [(err) (err)])))

;; string/p implementation
(define (string/p [s : String]) : (Parser String)
  (foldr (flip m-prepend) (pure "") (reverse (map char/p (string->list s)))))

;; parsers
(define (alt [res1 : (ParseResult 'a)] [res2 : (ParseResult 'a)]) : (ParseResult 'a)
  (type-case (ParseResult 'a) res1
    [(ok x) (ok x)]
    [(err) res2]))

(define (p-or [ps : (Listof (Parser 'a))]) : (Parser 'a)
  (λ (input)
    (let ([res1 ((first ps) input)])
    (foldr alt res1 (map (λ (p) (p input)) (rest ps))))))

;(define (both

;(define (p-and [ps : (Listof (Parser 'a))]) : (Parser 'a)
;  (λ (input)
;    (let ([res1 ((first ps) input)])
;      (

(define (p-seq [p1 : (Parser 'a)] [p2 : (Parser 'b)]) : (Parser ('a * 'b))
  (λ (input) (do (p1 input)
               (λ (res1) (do (p2 (fst res1))
                           (λ (res2) (p-result (fst res2) (pair (snd res1) (snd res2)))))))))

(define (p-append [r1 : (ParseResult Char)] [r2 : (ParseResult String)]) : (ParseResult String)
  (type-case (ParseResult Char) r1
    [(ok p1) (type-case (ParseResult String) r2
               [(ok p2) (if (string=? (fst p1) (fst p2))
                            (ok (pair (fst p1) (string-append (char->string (snd p1)) (snd p2))))
                            (err))]
               [(err) (err)])]
    [(err) (err)]))

(define (p-alt [p1 : (Parser 'a)] [p2 : (Parser 'a)]) : (Parser 'a)
  (λ (input) (alt (p1 input) (p2 input))))

(define (p-many [p : (Parser 'a)]) : (Parser (Listof 'a))
  (p-alt (p-many1 p) (pure '())))

(define (p-many1 [p : (Parser 'a)]) : (Parser (Listof 'a))
  (λ (input) (type-case (ParseResult 'a) (p input)
               [(ok first) (type-case (ParseResult (Listof 'a)) ((p-many p) (fst first))
                             [(ok rest) (ok (pair (fst rest) (append (list (snd first)) (snd rest))))]
                             [(err) (err)])]
               [(err) (err)])))

;  sergio was here >:) (it took me 3 hours to type this in dvorak)

(define (p-digit [s : String]) : (ParseResult 'a)
    ((p-or (list (char/p #\1)
                 (char/p #\2)
                 (char/p #\3)
                 (char/p #\4)
                 (char/p #\5)
                 (char/p #\6)
                 (char/p #\7)
                 (char/p #\8)
                 (char/p #\9)
                 (char/p #\0))) s))

(define (left-p [l : (Parser 'a)] [r : (Parser 'b)]); : (Parser 'a)
  (λ (input) (do (l input)
               (λ (result1) (do (r (fst result1))
                               (λ (result2) (p-result (fst result2) (snd result1))))))))

(define (right-p [l : (Parser 'a)] [r : (Parser 'b)]) : (Parser 'b)
  (λ (input) (do (l input)
               (λ (result1) (do (r (fst result1))
                               (λ (result2) (ok result2)))))))
                              
(define (p-number [s : String]) : (ParseResult Number)
  (do ((p-many1 p-digit) s)
    (λ (char-list)
      (p-result (fst char-list)
                (foldl (λ (acc x) (+ (* 10 x) acc)) 0 (map char->num (snd char-list)))))))

(define (p-color [s : String]) : (ParseResult Color)
  (alt (p-grayscale-color s) (p-rgb-color s)))

(define (p-grayscale-color [s : String]) : (ParseResult Color)
  (do ((left-p (right-p (char/p #\() p-number) (char/p #\))) s)
    (λ (result) (p-result (fst result) (grayscale-color (snd result))))))

(define (p-rgb-color [s : String]) : (ParseResult Color)
  (do ((left-p (right-p (char/p #\() p-number) (char/p #\,)) s)
    (λ (n1) (do ((left-p p-number (char/p #\,)) (fst n1))
                  (λ (n2) (do ((left-p p-number (char/p #\))) (fst n2))
                            (λ (n3) (p-result (fst n3) (rgb-color (snd n1) (snd n2) (snd n3))))))))))

(define (p-binary-op [s : String] [sym : Char] [f : (Color Color -> 'a)]) : (ParseResult 'a)
  (do (p-color s)
    (λ (col1) (do ((right-p (char/p sym) p-color) (fst col1))
               (λ (col2) (p-result (fst col2) (f (snd col1) (snd col2))))))))
       
(define (p-add [s : String]) : (ParseResult Operation)
  (p-binary-op s #\+ (λ (c1 c2)
                       (add c1 (color c2)))))

(define (p-multiply [s : String]) : (ParseResult Operation)
  (p-binary-op s #\* (λ (c1 c2)
                       (multiply c1 (color c2)))))

(define (p-subtract [s : String]) : (ParseResult Operation)
  (p-binary-op s #\- (λ (c1 c2)
                       (subtract c1 (color c2)))))

(define (p-divide [s : String]) : (ParseResult Operation)
  (p-binary-op s #\/ (λ (c1 c2)
                       (divide c1 (color c2)))))

(define (p-op [s : String]) : (ParseResult Operation)
  (do ((p-or (list p-add p-multiply p-subtract p-divide)) s)
    (λ (result) (ok result))))

(define ops : (Parser Char)
  (p-or (list (char/p #\+)
        (char/p #\*)
        (char/p #\/)
        (char/p #\-))))

(define (half-op [s : String]) : (ParseResult (Char * Color))
  (do ((p-seq ops p-color) s)
    (λ (result) (p-result (fst result) (snd result)))))

;(define (half-op [s : String]) : (ParseResult (Char * Color))
;  (do (ops s)
;    (λ (char-res) (do (p-color (fst char-res))
;                    (λ (color-res) (p-result (fst color-res) (pair (snd char-res) (snd color-res))))))))

;(define (p-op2 [s : String]) : (ParseResult Operation)
;  (do (p-op s)
;    (λ (result) (do (half-op (fst result))
;                  (λ (char-color) (p-result (fst char-color)
;                                            [let ([ch (fst (snd char-color))] [col (snd (snd char-color))])
;                                               [cond
;                                                 [(char=? ch #\+) (add col (operation (snd result)))]
;                                                 [(char=? ch #\*) (multiply col (operation (snd result)))]
;                                                 [(char=? ch #\/) (divide col (operation (snd result)))]
;                                                 [(char=? ch #\-) (subtract col (operation (snd result)))]]]))))))

;(define (concat-op [s : String] [op : Operation]) : (ParseResult Operation)
;  (do (half-op s)
;    (λ (char-color) (p-result (fst char-color)
;                                            [let ([ch (fst (snd char-color))] [col (snd (snd char-color))])
;                                               [cond
;                                                 [(char=? ch #\+) (add col (operation op))]
;                                                 [(char=? ch #\*) (multiply col (operation op))]
;                                                 [(char=? ch #\/) (divide col (operation op))]
;                                                 [(char=? ch #\-) (subtract col (operation op))]]]))))

(define (concat-op-list [first : Operation] [lst : (Listof (Char * Color))]) : Operation
  (foldl (λ (char-color acc)
           [let ([ch (fst char-color)] [col (snd char-color)])
             [cond
               [(char=? ch #\+) (add col (operation acc))]
               [(char=? ch #\*) (multiply col (operation acc))]
               [(char=? ch #\/) (divide col (operation acc))]
               [(char=? ch #\-) (subtract col (operation acc))]]]) first lst))

(define (p-many1-op [s : String]) : (ParseResult Operation)
  (do (p-op s)
    (λ (result) (do ((p-many1 (p-seq ops p-color)) (fst result))
                  (λ (result2) (p-result (fst result2) (concat-op-list (snd result) (snd result2))))))))

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

(eval-op (add (grayscale-color 1) (operation (add (grayscale-color 2) (color (rgb-color 1 2 3))))))

(do (p-many1-op "(255,0,42)+(42,42,42)*(456)/(2)")
  (λ (op) (p-result (fst op) (eval-op (snd op)))))
