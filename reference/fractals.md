# Fractals

This function generates a generative art ggplot object using fractal
patterns. Inspired by https://www.r-bloggers.com/2010/08/fractals-in-r/

## Usage

``` r
fractals(
  N = 25,
  col_palette = c("#9b332b", "#b64f32", "#f7c267", "#b9b9b8", "#5d6174", "#41485f"),
  shift = 0,
  left = -1,
  right = 1,
  y_param = 3,
  resolution = 0.005,
  dist_max = 4
)
```

## Arguments

- N:

  Number of iterations. Default 25.

- col_palette:

  Vector of colours. Default
  `c("#9b332b", "#b64f32", "#f7c267", "#b9b9b8", "#5d6174", "#41485f")`.

- shift:

  Offset of y-values. Default 0.

- left:

  Start range of x-axis. Default -1.

- right:

  End range of x-axis. Default 1.

- y_param:

  Rate of y growth. Default 3.

- resolution:

  Resolution of grid. Default 0.005.

- dist_max:

  Size of center area. Default 4.

## Value

A ggplot object.

## Examples

``` r
fractals()
```
