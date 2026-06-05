# Riley

This function generates a coloured generative art ggplot object from
intersecting lines. Inspired by Balm by Bridget Riley.

## Usage

``` r
riley(n_x = 9, n_y = 9, offset = 3, main_col = "black", bg_col = "white")
```

## Arguments

- n_x:

  Number of circles in x-direction. Default 9.

- n_y:

  Number of circles in y-direction. Default 9.

- offset:

  Numeric giving offset of y-axis. Default 3.

- main_col:

  Main colour. Default "black".

- bg_col:

  Background colour. Default "white".

## Value

A ggplot object.

## Examples

``` r
riley()
```
