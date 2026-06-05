# Divide

This function generates a coloured generative art ggplot object from
intersecting lines.

## Usage

``` r
divide(
  num_lines = 30,
  col_palette = c("#552000", "#8a4d00", "#c17d17", "#f8b150", "#f5f5f5", "#93c6e1",
    "#5f93ac", "#2e627a", "#00344a"),
  rayshade = FALSE,
  s = 1234
)
```

## Arguments

- num_lines:

  Number of intersecting lines. Default `30`.

- col_palette:

  Vector of colours. Default
  `c("#552000", "#8a4d00", "#c17d17", "#f8b150", "#f5f5f5", "#93c6e1", "#5f93ac", "#2e627a", "#00344a")`.

- rayshade:

  Boolean determining whether the returned plot should be converted to
  three dimensional using rayshader. If `TRUE`, `{rayshader}` is
  required to be installed. Default `FALSE`.

- s:

  Seed value. Default `1234`.

## Value

A ggplot object.

## Examples

``` r
divide()
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
```
