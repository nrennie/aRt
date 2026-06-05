# Black Hole

This function generates a generative art ggplot object using points.

## Usage

``` r
black_hole(
  r_max = c(50, 150, 250, 350),
  n = 10000,
  lim = 300,
  col_palette = c("#FCDE9C", "#FAA476", "#F0746E", "#E34F6F", "#DC3977", "#B9257A",
    "#7C1D6F"),
  bg_col = "black",
  size = 0.01,
  a = 0.5,
  s = 1234
)
```

## Arguments

- r_max:

  Vector of radii for the internal circle. Default
  `c(50, 150, 250, 350)`.

- n:

  Number of points per circle. Default 10000.

- lim:

  Numeric specifying size of grid. Default 400.

- col_palette:

  Vector of colours (or single colour). Default
  `c("#FCDE9C", "#FAA476", "#F0746E", "#E34F6F", "#DC3977", "#B9257A", "#7C1D6F")`.

- bg_col:

  Background colour. Default "black".

- size:

  Size of points. Default 0.01.

- a:

  Transparency of points. Default 0.5.

- s:

  Random seed. Default 1234.

## Value

A ggplot object.

## Examples

``` r
black_hole()
```
