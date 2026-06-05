# Mosaic

This function generates a generative art ggplot object from voronoi
tiles.

## Usage

``` r
mosaic(
  n = 10,
  col_palette = c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E"),
  line_col = "white",
  line_size = 1,
  x_means = c(0, 10, 5),
  y_means = c(0, 7, 8),
  xy_var = 2,
  s = 1234
)
```

## Arguments

- n:

  Number of points to generate tiles from. Default 100.

- col_palette:

  Vector of colours to fill tiles with, Default
  `c("#4B3F72", "#CBB3BF", "#FFC857", "#119DA4", "#19647E")`.

- line_col:

  Colour of lines between tiles, Default "white".

- line_size:

  Thickness of lines between tiles. Default 1.

- x_means:

  Vector of any number of means for the x-coordinate. Default
  `c(0, 10, 5)`.

- y_means:

  Vector of any number of means for the y-coordinate. Default
  `c(0, 7, 8)`.

- xy_var:

  Numeric variance of x and y points. Default 2.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
mosaic()
```
