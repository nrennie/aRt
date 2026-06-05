# Sunsets

This function creates generative art using faceted segments.

## Usage

``` r
sunsets(
  num_bars = 6,
  n = 500,
  col_palette = c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"),
  bg_col = "#413C58",
  vertical = FALSE,
  fade_vertical = FALSE,
  alpha = 1,
  s = 1234
)
```

## Arguments

- num_bars:

  Number of bars. Default 6.

- n:

  Number of lines per bar. Default 500.

- col_palette:

  Colour palette. Default
  `c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B")`.

- bg_col:

  Background colour. Default `"#413C58"`.

- vertical:

  Boolean indicating whether bars should be vertical. Default FALSE.

- fade_vertical:

  Boolean indicating whether the colouring should be vertical. Default
  FALSE.

- alpha:

  Transparency of coloured bars. Default 1.

- s:

  Random seed. Default 1234.

## Value

A ggplot object.

## Examples

``` r
sunsets()
```
