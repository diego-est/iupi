# IUPI
**I**mage **U**nit **P**rocessing **I**nterface

IUPI is an image processing interface employing simple operators.

It is a glorified turing machine. The tape in this case is the image you give the program. The operations you do to the tape are simple photographic operators.

**Why?**
Using an image as a tape means that each space on the tape is a single pixel on
the image. Each pixel has a `Red`, `Green` and `Blue` value. Which means that each piece of tape has `255^3` different values! This with the use of common image manipulation algorithms yields a thought provoking language!

## Usage

To execute your program simply provide your program with the image file that you want to use.
```
iupi <program.TODO> <image-file>
```

## Operators

### Numeric Operators
- Addition:
    - `+,` Add right pixel to current pixel (int)
    - `,+` Add left pixel to current pixel (int)
- Multiplication, Subtraction and Division:
    - Same as addition but with `-`, `*`, and `/`, instead of `+`
### Photographic Operators
- Interpolate:
    - `%(FLOAT)` Interpolate current pixel with right pixel's color and `FLOAT` percent
    - `(FLOAT)%` Interpolate current pixel with left pixel's color and `FLOAT` percent
- Rotate:
    - `<^` Rotate 90 degrees right
    - `>^` Rotate 90 degrees left
    - `<>` Mirror
- Transpose:
    - `&` Transpose every pixel in the image
- Set:
    - `(R, G, B)` Sets the current pixel to the color `(R, G, B)`
    - `(X, Y, (R, G, B))` Set the pixel at `(X, Y)` to the color `(R, G, B)`
    - `(X, Y)<->` Swaps the current pixel with pixel at `(X, Y)`
    - `]>` Copy current pixel to the right
    - `]<` Copy current pixel to the left
### Other Operators
- Pixel Buffer Movement:
    - `->` Move right 1 pixel
    - `<-` Move left 1 pixel
    - `^` Move up 1 pixel
    - `\/` Move down 1 pixel
- CLI manipulation:
    - `,` Print current pixel's int
    - `.` Print current pixel's float
    - `_` Print current pixel's char
    - `'` Receive a single byte from the user
    - `"` Receive two bytes from the user
    - `"'` or `'"` Receive three bytes from the user (1 RGB pixel)
    - `""` receive four bytes from user (1 ARGB pixel)
### Control Flow Constructs
- `(R, G, B)=?{EXPR}` Executes `EXPR` if current pixel is of `(R, G, B)` color (if statement)
- `(R, G, B)=?#{expr}` Executes `EXPR` as long as current pixel is of `(R, G, B)` color (while loop)
- `(R, G, B)=?${EXPR}` Run `EXPR` through every pixel in sequence until current pixel is of `(R, G, B)` color (for loop)

## CFG
TODO

## To-do:
Please visit: [to-do list](./TODO.md)
