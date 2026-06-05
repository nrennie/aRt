# Shatter

This function generates a generative art ggplot object using polygons.

## Usage

``` r
shatter(
  n_x = 25,
  n_y = 25,
  decay = 0.8,
  col_palette = "black",
  bg_col = "gray97",
  s = 1234
)
```

## Arguments

- n_x:

  Number of polygons per row. Default 25.

- n_y:

  Number of polygons per column. Default 25.

- decay:

  Numeric between 0 and 1 specifying the rate of decay if square shapes.
  Default 0.8.

- col_palette:

  colour palette. Default "black".

- bg_col:

  Single colour for background. Default "gray97".

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
shatter()
```
