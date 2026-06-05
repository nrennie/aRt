# Bubbles

This function generates a generative art ggplot object consisting of
circles filled with ellipses.

## Usage

``` r
bubbles(
  num_circles = 20,
  main_col = "black",
  col_palette = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A"),
  col_prob = 0.3,
  bg_col = "white",
  s = 1234
)
```

## Arguments

- num_circles:

  Number of circles. Default 20.

- main_col:

  Colour of non-highlighted rectangles. Default "black".

- col_palette:

  Vector of colours. Default
  `c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A")`.

- col_prob:

  Probability of choosing colour from `col_palette` instead of
  `main_col`.

- bg_col:

  Background colour. Default "white".

- s:

  Seed value. Default 1234.

## Value

A ggplot object

## Examples

``` r
bubbles()
```
