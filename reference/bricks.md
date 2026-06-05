# Bricks

This function generates a coloured generative art ggplot object using
polygons.

## Usage

``` r
bricks(
  n_y = 20,
  col_palette = c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C"),
  bg_col = "gray97",
  s = 1234
)
```

## Arguments

- n_y:

  Number of rows. Default 20.

- col_palette:

  Vector of colours. Can be any length. Default
  `c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C")`.

- bg_col:

  Background colour. Default `"gray97"`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
bricks()
```
