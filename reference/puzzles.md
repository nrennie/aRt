# Puzzles

This function generates a coloured generative art ggplot object from
treemaps.

## Usage

``` r
puzzles(
  n = 200,
  num_groups = 30,
  col_palette = c("#0e7c7b", "#17bebb", "#d4f4dd", "#d62246", "#4b1d3f"),
  bg_col = "white",
  s = 1234
)
```

## Arguments

- n:

  Number of boxes. Default 200.

- num_groups:

  Number of larger boxes. Default 30.

- col_palette:

  Vector of colours. Default
  `c("#0e7c7b", "#17bebb", "#d4f4dd", "#d62246", "#4b1d3f")`.

- bg_col:

  Background colour. Default `"white"`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
puzzles()
```
