# Abacus

This function generates a generative art ggplot object using points and
lines.

## Usage

``` r
abacus(
  nx = 30,
  ny = 100,
  max_size = 2,
  col_palette = "black",
  bg_col = "white",
  s = 123
)
```

## Arguments

- nx:

  Number of lines in x direction. Default 30.

- ny:

  Number of points per line. Default 100.

- max_size:

  Maximum size of points. Default 2.

- col_palette:

  Vector of colours (or single colour). Default "black".

- bg_col:

  Background colour. Default "white".

- s:

  Random seed. Default 123.

## Value

A ggplot object.

## Examples

``` r
abacus()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the aRt package.
#>   Please report the issue to the authors.
```
