# Squares

This function generates a generative art ggplot object using
pattern-filled squares

## Usage

``` r
squares(
  n = 7,
  line_col = "white",
  pattern_col = "white",
  pattern_fill = "black",
  pattern_size = 0.4,
  linewidth = 1.5,
  s = 1234
)
```

## Arguments

- n:

  Number of squares per row. Default 7

- line_col:

  Colour of lines between squares. Default "white".

- pattern_col:

  Colour of pattern lines. Default "white".

- pattern_fill:

  Colour of pattern background. Default "black".

- pattern_size:

  Size of pattern. Default 0.4.

- linewidth:

  Size of lines between squares. Default 1.5.

- s:

  Random seed. Default 1234.

## Value

A ggplot object.

## Examples

``` r
squares()
```
