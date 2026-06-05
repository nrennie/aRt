# Tiles

This function generates a coloured generative art ggplot object using
square polygons.

## Usage

``` r
tiles(
  n_x = 12,
  n_y = 12,
  col_palette = c("#67322e", "#99610a", "#6e948c", "#2c6b67", "#122c43"),
  s = 1234
)
```

## Arguments

- n_x:

  Number of polygons per row. Default 12.

- n_y:

  Number of polygons per column. Default 18.

- col_palette:

  Vector of colours. Default
  `c("#67322e", "#99610a", "#6e948c", "#2c6b67", "#122c43")` which is
  the Veronese palette from MetBrewer.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
tiles()
```
