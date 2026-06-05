# Stackture

This function generates a coloured generative art ggplot object using
overlapping semi-transparent circles.

## Usage

``` r
stackture(
  n_x = 8,
  n_y = 8,
  min_height = 1,
  max_height = 1.5,
  min_width = 1,
  max_width = 1.5,
  interpolate = TRUE,
  col_palette = c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7"),
  bg_col = "#004B67",
  alpha = 1,
  s = 1234
)
```

## Arguments

- n_x:

  Number of columns in grid. Default 8.

- n_y:

  Number of rows in grid. Default 8.

- min_height:

  minimum height.

- max_height:

  maximum height.

- min_width:

  minimum width.

- max_width:

  maximum width.

- interpolate:

  Boolean indicating if colours should be interpolated. Default `TRUE`.

- col_palette:

  Vector of colours. Default
  `c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7")`.

- bg_col:

  Background colour. Default `"#004B67"`.

- alpha:

  Transparency. Default 1.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
stackture()
```
