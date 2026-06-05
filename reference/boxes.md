# Boxes

This function generates a coloured generative art ggplot object from
treemaps.

## Usage

``` r
boxes(
  n = 100,
  perc = 0.1,
  col_palette = c("#D2FBD4", "#A5DBC2", "#7BBCB0", "#559C9E", "#3A7C89", "#235D72",
    "#123F5A"),
  bg_col = "black",
  s = 1234
)
```

## Arguments

- n:

  Number of boxes

- perc:

  Relationship between box size and colour. Value between 0 and 1 where
  0 represents randomness and 1 perfect identical. Default 0.1.

- col_palette:

  Vector of colours. Default
  `"#D2FBD4" "#A5DBC2" "#7BBCB0" "#559C9E" "#3A7C89" "#235D72" "#123F5A"`.

- bg_col:

  Background colour. Default "black".

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
boxes()
```
