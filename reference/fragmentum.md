# Fragmentum

This function generates a coloured generative art ggplot object using
randomly tessellated polygons.

## Usage

``` r
fragmentum(
  n_x = 10,
  n_y = 10,
  deg_jitter = 0.1,
  linewidth = 0.5,
  line_col = "black",
  bg_col = "black",
  col_palette = c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"),
  s = 1234
)
```

## Arguments

- n_x:

  Number of polygons per row. Default 10.

- n_y:

  Number of polygons per column. Default 10.

- deg_jitter:

  Numeric between 0 and 0.5 specifying the degree of jitter. Default
  0.1.

- linewidth:

  Width of lines between polygons. Default 0.5.

- line_col:

  Colour of lines between polygons. Default `"black"`.

- bg_col:

  Background colour. Default `"black"`.

- col_palette:

  Vector of colours. Can be any length. Default
  `c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B")`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
fragmentum()
```
