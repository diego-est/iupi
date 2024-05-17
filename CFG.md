The following is a context-free grammar for the `iupi` language.

```txt
<language> ::= <unary operation> | <binary operation> | <color>

<expr> ::= <binary operation> | <color>

<unary operation> ::= <linear invert> | <value invert>

<binary operation> ::= <add> | <multiply> | <subtract> | <divide>
    <interpolate> | <max> | <min>

<add> ::= <color> + <expr>
<multiply> ::= <color> * <expr>
<subtract> ::= <color> - <expr>
<divide> ::= <color> / <expr>
<interpolate> ::= <color> <float> <expr>
<max> ::= <color> ^ <expr>
<min> ::= <color> ! <expr>

<value invert> ::= <expr> <^>
<linear invert> ::= <expr> <|>

<float> ::= .<decimals>

<decimals> ::= <digit><decimals> | <epsilon>

<digits> ::= <digits><digit> | <epsilon>
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

<color> ::= (<digits>,<digits>,<digits>)

<epsilon> ::= ''
```
