# Wander

This function generates a generative art ggplot object using random
walks

## Usage

``` r
wander(
  n_lines = 100,
  n_points = 350,
  r_outer = 8,
  r_inner = 3,
  line_var = 0.01,
  deg_jitter = 0.1,
  linewidth = 0.1,
  bg_col = "#462255",
  col_palette = c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C"),
  n_cols = 20,
  s = 123
)
```

## Arguments

- n_lines:

  Number of lines. Default 100.

- n_points:

  Number of points. Default 350.

- r_outer:

  Radius of outer circle. Default 8.

- r_inner:

  Radius of inner circle. Default 3.

- line_var:

  Variance of random walk noise. Default 0.01.

- deg_jitter:

  Degree of jitter for multiple lines. Default 0.1.

- linewidth:

  Width of lines. Default 0.1.

- bg_col:

  Background colour. Default "#462255".

- col_palette:

  Vector of colours. Default
  `c("#FF8811", "#9DD9D2", "#046E8F", "#D44D5C")`

- n_cols:

  Number of colours to create. Default 20.

- s:

  Random seed. Default 123.

## Value

A ggplot object.

## Examples

``` r
wander()
```
