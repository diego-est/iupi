The following is a context-free grammar for the `iupi` language.

```txt
<epsilon> ::= ''
<expr> ::= <operation> | <color>

<operation> ::= <add> | <multiply> | <subtract> | <divide> | <value invert> | <linear invert> | <interpolate> | <contrast> | <max> | <min>
<add> ::= <color> + <expr>
<multiply> ::= <color> * <expr>
<subtract> ::= <color> - <expr>
<divide> ::= <color> / <expr>
<value invert> ::= <expr> <^>
<linear invert> ::= <expr> <|>
<interpolate> ::= <color> <float> <expr>
<hue shift> ::= <expr> & <integer>
<max> ::= <color> ^ <expr>
<min> ::= <color> ! <expr>

<float> ::= .<digits> | <digits>.<digits>
<integer> ::= <digits>
<digits> ::= <digit><digits> | <epsilon>
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<hex> ::= A | B | C | D | E | F | <digit>

<color> ::= <RGB color> | <grayscale color>
<RGB color> ::= (<integer>,<integer>,<integer>)
<grayscale color> ::= (<integer>)

<if> ::= <color> = <color> {<expr>} {<expr>}
```

## Attributes
Interesting lexeme attributes:

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
