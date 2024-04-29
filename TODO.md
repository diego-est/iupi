Only some of these will be implemented ;-;

### Numeric Operators
- [ ] Addition:
    - [ ] `+,` Add right pixel to current pixel (int)
    - [ ] `,+` Add left pixel to current pixel (int)
- [ ] Multiplication, Subtraction and Division:
    - [ ] Same as addition but with `-`, `*`, and `/`, instead of `+`
### Photographic Operators
- [ ] Interpolate:
    - [ ] `%(FLOAT)` Interpolate current pixel with right pixel's color and `FLOAT` percent
    - [ ] `(FLOAT)%` Interpolate current pixel with left pixel's color and `FLOAT` percent
- [ ] Rotate:
    - [ ] `<^` Rotate 90 degrees right
    - [ ] `>^` Rotate 90 degrees left
    - [ ] `<>` Mirror
- [ ] Transpose:
    - [ ] `&` Transpose every pixel in the image
- [ ] Set:
    - [ ] `(R, G, B)` Sets the current pixel to the color `(R, G, B)`
    - [ ] `(X, Y, (R, G, B))` Set the pixel at `(X, Y)` to the color `(R, G, B)`
    - [ ] `(X, Y)<->` Swaps the current pixel with pixel at `(X, Y)`
    - [ ] `]>` Copy current pixel to the right
    - [ ] `]<` Copy current pixel to the left
### Other Operators
- [ ] Pixel Buffer Movement:
    - [ ] `->` Move right 1 pixel
    - [ ] `<-` Move left 1 pixel
    - [ ] `^` Move up 1 pixel
    - [ ] `\/` Move down 1 pixel
- [ ] CLI manipulation:
    - [ ] `,` Print current pixel's int
    - [ ] `_` Print current pixel's char
    - [ ] `'` Receive a single byte from the user
    - [ ] `"` Receive three bytes from the user
### Control Flow Constructs
- [ ] `(R, G, B)?{EXPR}` Executes `EXPR` if current pixel is of `(R, G, B)` color (if statement)
- [ ] `(R, G, B)#{expr}` Executes `EXPR` as long as current pixel is of `(R, G, B)` color (while loop)
- [ ] `(R, G, B)${EXPR}` Run `EXPR` through every pixel in sequence until current pixel is of `(R, G, B)` color (for loop)


