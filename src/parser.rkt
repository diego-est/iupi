#lang plait

(require "utilities.rkt")
(require "types.rkt")
(require "combinators.rkt")

; TODO: DEPRECATED
; Flips the arguments for function (a)
(define (flip a)
  (λ (x y) (a y x)))

;----- char/p implementation -----;
; Creates a parser that parses a single character (c)
(define (char/p [c : Char]) : (Parser Char)
  (λ (s)
    (type-case (Optionof Char) (first-char s)
      [(some x) (if (char=? x c) (ok (pair (string-tail s) c)) (err))]
      [(none) (err)])))

;; char/p monoid implementation ;;
; Though technically this operation should be commutative, it is not and that is
; why there (m-prepend) is named (m-prepend)

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
