# Fading

This function generates a coloured generative art ggplot object using
voronoi tiles.

## Usage

``` r
fading(
  n_layers = 6,
  n_points = 10,
  col_palette = c("#FCDE9C", "#FAA476", "#F0746E", "#E34F6F", "#DC3977", "#B9257A",
    "#7C1D6F"),
  s = 1234
)
```

## Arguments

- n_layers:

  Number of layers. Default 6.

- n_points:

  Number of points per layer area. Default 10.

- col_palette:

  Vector of colours. Default
  `c("#FCDE9C", "#FAA476", "#F0746E", "#E34F6F", "#DC3977", "#B9257A", "#7C1D6F"`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
fading()
```
