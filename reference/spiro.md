# Spiro

This function generates a generative art ggplot object in a spirograph
style.

## Usage

``` r
spiro(
  n_x = 10,
  n_y = 10,
  d = 10,
  R = 4,
  r = 1,
  linewidth = 0.5,
  col_palette = "white",
  bg_col = "grey20",
  s = 1234
)
```

## Arguments

- n_x:

  Number of spirals per row. Default 10.

- n_y:

  Number of spirals per column. Default 10.

- d:

  Diameter. Default 10.

- R:

  Outer radius. Default 4.

- r:

  Inner radius. Default 1.

- linewidth:

  Width on lines. Default 0.5.

- col_palette:

  Vector of colours. Default "white".

- bg_col:

  Background colour. Default "grey20".

- s:

  Seed value. Default 1234.

## Value

A ggplot object

## Examples

``` r
spiro()
```
