#lang plait

(require "utilities.rkt")
(require "types.rkt")
(require "combinators.rkt")

;----- char/p implementation -----;
; Creates a parser that parses a single character (c)
(define (char/p [c : Char]) : (Parser Char)
  (λ (s)
    (type-case (Optionof Char) (first-char s)
      [(some x) (if (char=? x c) (ok (pair (string-tail s) c)) (err))]
      [(none) (err)])))

;; char/p monoid implementation ;;
; Though technically this operation should be commutative, it is not and that is
; why there are (m-append) and (m-prepend) functions
; Creates a parser that concatenates a (Parser Char) with a (Parser String) so
; that it parses the char first and then the string.
(define (m-append [p1 : (Parser Char)] [p2 : (Parser String)]) : (Parser String)
  (λ (input)
    (type-case (ParseResult Char) (p1 input)
      [(ok r1) (type-case (ParseResult String) (p2 (fst r1))
                   [(ok r2) (ok (pair (fst r2) (string-append (snd r2) (list->string (list (snd r1))))))]
                   [(err) (err)])]
      [(err) (err)])))

; Creates a parser that concatenates a (Parser String) with a (Parser Char) so
; that it parses the string first and then the char
(define (m-prepend [p1 : (Parser String)] [p2 : (Parser Char)]) : (Parser String)
  (λ (input)
    (type-case (ParseResult String) (p1 input)
      [(ok r1) (type-case (ParseResult Char) (p2 (fst r1))
                   [(ok r2) (ok (pair (fst r2) (string-append (snd r1) (list->string (list (snd r2))))))]
                   [(err) (err)])]
      [(err) (err)])))

;----- string/p implementation -----;
; Creates a parser that parses a string (s)
(define (string/p [s : String]) : (Parser String)
  (foldr (flip m-prepend) (pure "") (reverse (map char/p (string->list s)))))

;---- Parsers -----;
;; Numeric Parsers ;;

; Parses a single digit from the input
(define (digit/p [s : String]) : (ParseResult 'a)
    ((or/p (list (char/p #\1)
                 (char/p #\2)
                 (char/p #\3)
                 (char/p #\4)
                 (char/p #\5)
                 (char/p #\6)
                 (char/p #\7)
                 (char/p #\8)
                 (char/p #\9)
                 (char/p #\0))) s))

; Parses an entire number from the input
(define (p-number [s : String]) : (ParseResult Number)
  (do ((many1/p digit/p) s)
    (λ (char-list)
      (p-result (fst char-list)
                (foldl (λ (acc x) (+ (* 10 x) acc)) 0 (map char->num (snd char-list)))))))

; TODO: implement parser for floats

;; Language Functionality Parsers ;;
; Parses a color.
(define (p-color [s : String]) : (ParseResult Color)
  (alt (p-grayscale-color s) (p-rgb-color s)))

; Parses a Grayscale Color.
; A correctly formed color has the following RegEx: "([0-9]+)"
(define (p-grayscale-color [s : String]) : (ParseResult Color)
  (do ((left/p (right/p (char/p #\() p-number) (char/p #\))) s)
    (λ (result) (p-result (fst result) (grayscale-color (snd result))))))

; Parses an RGB Color.
; A correctly formed color has the following RegEx: "([0-9]+,[0-9]+,[0-9]+)"
(define (p-rgb-color [s : String]) : (ParseResult Color)
  (do ((left/p (right/p (char/p #\() p-number) (char/p #\,)) s)
    (λ (n1) (do ((left/p p-number (char/p #\,)) (fst n1))
                  (λ (n2) (do ((left/p p-number (char/p #\))) (fst n2))
                            (λ (n3) (p-result (fst n3) (rgb-color (snd n1) (snd n2) (snd n3))))))))))

; Helper function for binary operation parsers. Parses a symbol (sym) and runs
; (f) on the result to build the resulting Color.
(define (p-binary-op [s : String] [sym : Char] [f : (Color Color -> 'a)]) : (ParseResult 'a)
  (do (p-color s)
    (λ (col1) (do ((right/p (char/p sym) p-color) (fst col1))
               (λ (col2) (p-result (fst col2) (f (snd col1) (snd col2))))))))

; Addition parser.
(define (p-add [s : String]) : (ParseResult Operation)
  (p-binary-op s #\+ (λ (c1 c2)
                       (add c1 (color c2)))))

; Multiplication parser.
(define (p-multiply [s : String]) : (ParseResult Operation)
  (p-binary-op s #\* (λ (c1 c2)
                       (multiply c1 (color c2)))))

; Subtraction parser.
(define (p-subtract [s : String]) : (ParseResult Operation)
  (p-binary-op s #\- (λ (c1 c2)
                       (subtract c1 (color c2)))))

; Division parser.
(define (p-divide [s : String]) : (ParseResult Operation)
  (p-binary-op s #\/ (λ (c1 c2)
                       (divide c1 (color c2)))))

; TODO: create parsers for the other operations

; Parser that can parse any  operation.
(define (p-op [s : String]) : (ParseResult Operation)
  (do ((or/p (list p-add p-multiply p-subtract p-divide)) s) ; TODO: Add
   ; implemented parsers to this list
    (λ (result) (ok result))))

; Parser that succeeds if any operation is found
(define ops : (Parser Char)
  (or/p (list (char/p #\+)
        (char/p #\*)
        (char/p #\/)
        (char/p #\-)))) ; TODO: add other operations

; Parser that parses an operation followed by a color.
(define (half-op [s : String]) : (ParseResult (Char * Color))
  (do ((seq/p ops p-color) s)
    (λ (result) (p-result (fst result) (snd result)))))
