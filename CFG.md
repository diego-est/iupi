The following is a context-free grammar for the `iupi` language.

```txt
<expr> ::= <numOps> | <imgOps> | <expr><expr>

<numOps> ::= <addL> | <addR> | <mulL> | <mulR> | <subL> | <subR> | <divL> | <divR>
<addR> ::= +,
<addL> ::= ,+
<mulR> ::= *,
<mulL> ::= ,*
<subR> ::= -,
<subL> ::= ,-
<divR> ::= /,
<divL> ::= ,/

<imgOps> ::= <interpolate> | <rotateR> | <rotateL> | <mirror>

<interpolateR> ::= %(<float>)
<interpolateL> ::= (<float>)%

<rotateR> ::= <^
<rotateL> ::= >^
<mirror> ::= <>
<transpose> ::= &

<set> ::= (<colorHex>) | (<colorU8>)
<copyL> ::=
<copyR> ::=
<swap> ::=

<moveR> ::= ->
<moveL> ::= <-
<moveD> ::= \/
<moveU> ::= ^

<printInt> ::=
<printChar> ::=
<receiveByte> ::=
<receiveRGB> ::= "' | '"

<float> ::= <digits>.<digits>
<colorHex> ::= #<hex><hex><hex><hex><hex><hex>
<colorU8> ::= <U8>, <U8>, <U8>
<digits> ::= <digit> | <digit><digits>
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<hex> ::= A | B | C | D | E | F | <digit>
<U8> ::= 

<if> ::= <set>?{<expr>}
<while> ::= <set>#{<expr>}
<forEach> ::= <set>${<expr>}
```
