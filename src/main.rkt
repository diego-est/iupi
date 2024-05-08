#lang plait
;; Useful utilities
(define (id x) x)
(define (const a)
  (lambda (b) a))

(define (first-char [s : String]) : (Optionof Char)
  (if (empty? (string->list s))
      [none]
      [some (first (string->list s))]))

(define (string-tail [s : String])
  (if (empty? (string->list s))
      ""
      [list->string (rest (string->list s))]))

(define-type (ParseResult 'a)
  [success (r : (Optionof (String * 'a)))]
  [failure])

;; Language implementation
(define-type U8
  [num (n : Number)])

(define-type ImgOps
  [interpolate (f : Number)]
  [rotate-left]
  [rotate-right]
  [mirror]
  [transpose]
  [setter (c : Color)])

(define-type Color
  [hex-color (n : Number)] ;; TODO: Hex should turn a string of #FFA123 into a Number
  [int-color (n : Number)])

(define-type NumOps
  [addR (n : Number)]
  [addL (n : Number)])

(define-type Expr
  [img-op (op : ImgOps)]
  [num-op (op : NumOps)]
)

;; Original s-expr parser implementation
;(define (parse e)
;  [cond
;    [(s-exp-match? `& e) (rotate-left)]
;    [(s-exp-match? `(NUMBER) e) (setter (int-color (get-SetNum e)))]
;    [(s-exp-match? `<> e) (mirror)]
;    [else (rotate-left)]
;  ]
;)

(define (char/p [c : Char])
  (lambda (s)
    (type-case (Optionof Char) (first-char s)
      [(some x) (if (char=? x c) (some (pair (string-tail s) c)) (none))]
      [(none) (none)])))

(define (parse-string p s)
  (p s))


;; The following functor and applicative implementations for char/p were taken
;;  from the prelude implementation of haskell at:
;;  https://hackage.haskell.org/package/base-4.19.1.0/docs/src/Data.Functor.html

;; char/p functor
(define (fmap f p)
  (lambda (input)
  (type-case (Optionof (String * 'a)) (p input)
    [(some x) (some (pair (fst x) (f (snd x))))]
    [(none) (none)])))

;; char/p applicative
(define (pure x) : (String -> (Optionof (String * 'a)))
  (lambda (input)
    (some (pair input x))))

(define (seq [p1 : (String -> (Optionof (String * ('a -> 'b))))] [p2 : (String -> (Optionof (String * 'a)))])
  (lambda (input)
    (type-case (Optionof (String * ('a -> 'b))) (p1 input)
      [(some x) (type-case (Optionof (String * 'a)) (p2 input)
                  [(some y) (some (pair (fst y) ((snd x) (snd y))))]
                  [(none) (none)])]
      [(none) (none)])))

;(define (sequenceA [lst : (Listof (String -> (Optionof (String * 'a))))])
;  (if (empty? lst) (pure (list)) (seq (fmap cons (first lst)) (sequenceA (rest lst)))))
  

;; some tests
(map char/p (string->list "null"))
((fmap (const #\a) (char/p #\n)) "nice")