#lang plait

(require "types.rkt")
;----- Helper functions -----;

;; String-Char Functions ;;
; Gets the first Char from a String
(define (first-char [s : String]) : (Optionof Char)
  (if (empty? (string->list s))
      [none]
      [some (first (string->list s))]))

; Returns the tail of a String
(define (string-tail [s : String]) : String
  (if (empty? (string->list s))
      ""
      [list->string (rest (string->list s))]))

; Converts a Char to a Number
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

;; List Functions ;;
; Gets the maximum number from a list
(define (maxel [lst : (Listof Number)]) : Number
  (foldl (λ (a b) (if  (> a b) a b)) (first lst) lst))

; Gets the minimum element from a list
(define (minel [lst : (Listof Number)]) : Number
  (foldl (λ (a b) (if (< a b) a b)) (first lst) lst))

; Turns a list of digits into a Digits
(define (digitlist->digits [digitlist : (Listof Digit)]) : Digits
  (foldl (λ (dig acc) (number dig acc)) (empty-digit) digitlist))

; Turns a list of decimals into a Decimal
(define (digitlist->decimals [digitlist : (Listof Digit)]) : Decimals
  (foldr (λ (dig acc) (decimals dig acc)) (empty-decimal) digitlist))

; Turn a number into a digits
(define (number->digits [n : Number]) : Digits
  (if (= n 0) (empty-digit) (number (digit (modulo n 10)) (number->digits (floor (/ n 10))))))

; Sums all three components

;; Miscellaneous Functions ;;
; Compute n modulo 256
(define (mod256 n)
  (modulo n 256))

; Absolute value function
(define (abs [n : Number]) : Number
  (if (< n 0) (* -1 n) n))

; Rounds a number to the nearest whole number
(define (round [n : Number]) : Number
  (floor (+ n 0.5)))

; Interpolate two numbers
(define (interpolate-num [start : Number] [end : Number] [percent : Number]) : Number
  (+ (* (- end start) percent) start))

;----- Parser Utilities -----;
; This is shorthand notation for using a (ParseResult 'a) in case it exists
; If the (ParseResult 'a) is of the (err) variant it simply returns (err)
(define (do [p : (ParseResult 'a)] [f : ((String * 'a) -> (ParseResult 'b))]) : (ParseResult 'b)
  (type-case (ParseResult 'a) p
    [(ok res) (f res)]
    [(err) (err)]))

; This is shorthand notation for building a (ParseResult 'a)
(define (return [left : String] [right : 'a]) : (ParseResult 'a)
  (ok (pair left right)))

; Chooses the (ok) variant of either res1 or res2 and returns it.
(define (alt [res1 : (ParseResult 'a)] [res2 : (ParseResult 'a)]) : (ParseResult 'a)
  (type-case (ParseResult 'a) res1
    [(ok x) (ok x)]
    [(err) res2]))
