# Polygons

This function generates a coloured generative art ggplot object using
polygons.

## Usage

``` r
polygons(
  n_x = 12,
  n_y = 18,
  gap_size = 0.5,
  deg_jitter = 0.1,
  col_palette = c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C"),
  random = TRUE,
  bg_col = "gray97",
  s = 1234
)
```

## Arguments

- n_x:

  Number of polygons per row. Default 12.

- n_y:

  Number of polygons per column. Default 18.

- gap_size:

  Numeric between 0 and 1 specifying the size of the gap in the
  polygons. Default 0.5.

- deg_jitter:

  Numeric between 0 and 0.5 specifying the degree of jitter. Default
  0.1.

- col_palette:

  Vector of colours. Can be any length. Default
  `c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C")`.

- random:

  Boolean for whether colours should be random or ordered. Default TRUE.

- bg_col:

  Background colour. Default "gray97".

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
polygons()
```
