# Distort

This function generates a generative art ggplot object using polygons

## Usage

``` r
distort(
  n_x = 5,
  n_y = 5,
  deg_jitter = 0.4,
  col_palette = c("#4D4D4D", "#888888", "#AEAEAE", "#CCCCCC", "#E6E6E6"),
  line_col = "transparent",
  linewidth = 1,
  bg_col = "#333333",
  s = 1234
)
```

## Arguments

- n_x:

  Number of columns. Default 4.

- n_y:

  Number of rows. Default 4.

- deg_jitter:

  Degree of jitter. Default 0.4.

- col_palette:

  Vector of colours. Must be at least length 4. Default
  `c("#4D4D4D", "#888888", "#AEAEAE", "#CCCCCC", "#E6E6E6")`

- line_col:

  Colour of lines. Default `"transparent"`.

- linewidth:

  Width of lines. Default 1.

- bg_col:

  Background colour. Default `"#333333"`.

- s:

  Random seed. Default 1234.

## Value

A ggplot object.

## Examples

``` r
distort()
```
