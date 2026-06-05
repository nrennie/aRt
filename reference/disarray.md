# Disarray

This function generates a generative art ggplot object using polygons

## Usage

``` r
disarray(
  n_x = 4,
  n_y = 4,
  col_palette = c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C"),
  grid_col = "white",
  grid_width = 1,
  s = 1234
)
```

## Arguments

- n_x:

  Number of columns. Default 4.

- n_y:

  Number of rows. Default 4.

- col_palette:

  Vector of colours. Must be at least length 4. Default
  `c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C")`

- grid_col:

  Colour of grid lines. Default "white".

- grid_width:

  Linewidth of grid lines. Default 1.

- s:

  Random seed. Default 1234.

## Value

A ggplot object.

## Examples

``` r
disarray()
```
