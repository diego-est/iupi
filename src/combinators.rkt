#lang plait

(require "utilities.rkt")
(require "types.rkt")

;----- Parser 'a applicative -----;
; The following applicative implementations for (Parser 'a) were taken
; from the prelude implementation of haskell at:
; https://hackage.haskell.org/package/base-4.19.1.0/docs/src/GHC.Base.html
; Creates a parser that parses (x) regardless of input
(define (pure [x : 'a]) : (Parser 'a)
  (λ (input)
    (ok (pair input x))))

;----- Parser Combinators -----;
; Creates a parser that runs the entire list of parsers through an input and
; returns an (ok) variant if any of them succeed
(define (or/p [ps : (Listof (Parser 'a))]) : (Parser 'a)
  (λ (input)
    (let ([res1 ((first ps) input)])
    (foldr alt res1 (map (λ (p) (p input)) (rest ps))))))

; Creates a parser out of two parsers that will sequence them.
(define (seq/p [p1 : (Parser 'a)] [p2 : (Parser 'b)]) : (Parser ('a * 'b))
  (λ (input) (do (p1 input)
               (λ (res1) (do (p2 (fst res1))
                           (λ (res2) (return (fst res2) (pair (snd res1) (snd res2)))))))))

; Creates a parser that parses either (p1) or (p2)
(define (alt/p [p1 : (Parser 'a)] [p2 : (Parser 'a)]) : (Parser 'a)
  (λ (input) (alt (p1 input) (p2 input))))

; Creates a parser that will parse the input 0 or more times. Like using "*" in
; a RegEx.
(define (many/p [p : (Parser 'a)]) : (Parser (Listof 'a))
  (alt/p (many1/p p) (pure '())))

; Creates a parser that parses the input 1 or more times. Like using "+" in a
; RegEx.
(define (many1/p [p : (Parser 'a)]) : (Parser (Listof 'a))
  (λ (input) (type-case (ParseResult 'a) (p input)
               [(ok first) (type-case (ParseResult (Listof 'a)) ((many/p p) (fst first))
                             [(ok rest) (ok (pair (fst rest) (append (list (snd first)) (snd rest))))]
                             [(err) (err)])]
               [(err) (err)])))

; Creates a parser out of two parsers that sequentially applies them. But will
; only return the result of the first parser with the rest of the input from the
; second parser.
(define (left/p [l : (Parser 'a)] [r : (Parser 'b)]) : (Parser 'a)
  (λ (input) (do (l input)
               (λ (result1) (do (r (fst result1))
                               (λ (result2) (return (fst result2) (snd result1))))))))

; Creates a parser out of two parsers that sequentially applies them. But will
; only return the result of the second parser.
(define (right/p [l : (Parser 'a)] [r : (Parser 'b)]) : (Parser 'b)
  (λ (input) (do (l input)
               (λ (result1) (do (r (fst result1))
                               (λ (result2) (ok result2)))))))
