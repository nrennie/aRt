# Rectangles

This function generates a generative art ggplot object featuring
multiple coloured rectangles.

## Usage

``` r
rectangles(
  n = 100,
  max_height = 7,
  max_width = 5,
  size = 2,
  main_col = "lightgrey",
  col_palette = c("#80BA5A", "#E68310", "#008695", "#CF1C90", "#F97B72", "#4B4B8F"),
  bg_col = "white",
  weight = 0.25,
  s = 1234
)
```

## Arguments

- n:

  Number of rectangles. Default 100.

- max_height:

  Maximum height of rectangle. Default 7.

- max_width:

  Maximum width of rectangle. Default 5.

- size:

  Line width of rectangles. Default 2.

- main_col:

  Colour of non-highlighted rectangles. Default `"lightgrey"`.

- col_palette:

  Vector of colours. Default
  `c("#80BA5A", "#E68310", "#008695", "#CF1C90", "#F97B72", "#4B4B8F")`.

- bg_col:

  Background colour. Default `"white"`.

- weight:

  Sampling weight. Default 0.25.

- s:

  Seed value. Default 1234.

## Value

A ggplot object

## Examples

``` r
rectangles()
```
