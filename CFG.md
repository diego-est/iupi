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
<transpose> ::=

<set> ::= (<colorHex>) | (<colorU8>)
<copyL> ::=
<copyR> ::=
<swap> ::=

<moveR> ::=
<moveL> ::=
<moveD> ::=
<moveU> ::=

<printInt> ::=
<printChar> ::=
<receiveByte> ::=
<receiveRGB> ::=

<float> ::= <digits>.<digits>
<colorHex> ::= #<hex><hex><hex><hex><hex><hex>
<colorU8> ::=
<digits> ::= <digit> | <digit><digits>
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<hex> ::= A | B | C | D | E | F | <digit>
<U8> ::=

<if> ::= <set>?{<expr>}
<while> ::= <set>#{<expr>}
<forEach> ::= <set>${<expr>}
```

## Attributes
Interesting lexeme attributes:
- `<colorHex>`: can be converted into a full 32-bit integer

### Synthesization
| Non-terminals | Synthesized attributes| Inherited attributes  |
|---------------|-----------------------|-----------------------|
| `<colorHex>`  |                       | `--`                  |
| `<hex>`       | Val                   | `--`                  |
| `<+,>`        |                       | `--`                  |

### Base Attributes
| Terminals | Attribute | Value |
|-----------|-----------|-------|
| `<digit> ::= 0` | Val | 0     |
| `<digit> ::= 1` | Val | 1     |
| `<digit> ::= 2` | Val | 2     |
| `<digit> ::= 3` | Val | 3     |
| `<digit> ::= 4` | Val | 4     |
| `<digit> ::= 5` | Val | 5     |
| `<digit> ::= 6` | Val | 6     |
| `<digit> ::= 7` | Val | 7     |
| `<digit> ::= 8` | Val | 8     |
| `<digit> ::= 9` | Val | 9     |
| `<hex> ::= A` | Val   | 10    |
| `<hex> ::= B` | Val   | 11    |
| `<hex> ::= C` | Val   | 12    |
| `<hex> ::= D` | Val   | 13    |
| `<hex> ::= E` | Val   | 14    |
| `<hex> ::= F` | Val   | 15    |
