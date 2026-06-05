# Stacked

This function generates a coloured generative art ggplot object using 3D
square polygons.

## Usage

``` r
stacked(
  n_x = 5,
  n_y = 5,
  col_palette = c("#e76254", "#ef8a47", "#ffd06f", "#ffe6b7", "#aadce0", "#72bcd5",
    "#376795"),
  rayshade = FALSE,
  shadow_intensity = 0.5,
  sunangle = 315,
  s = 1234
)
```

## Arguments

- n_x:

  Number of polygons per row. Default 5.

- n_y:

  Number of polygons per column. Default 5.

- col_palette:

  Vector of colours. Default
  `c("#e76254", "#ef8a47", "#ffd06f", "#ffe6b7", "#aadce0", "#72bcd5", "#376795")`
  which is the Hiroshige palette from MetBrewer.

- rayshade:

  Boolean determining whether the returned plot should be converted to
  three dimensional using rayshader. If `TRUE`, `{rayshader}` is
  required to be installed. Default `FALSE`.

- shadow_intensity:

  Intensity of shading for 3D elements, Default 0.5.

- sunangle:

  Angle of the sun. Default 315.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
stacked()
```
