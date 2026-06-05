# Network

This function generates a generative art ggplot object using a minimum
spanning tree.

## Usage

``` r
network(
  n_x = 7,
  n_y = 7,
  prop = 0.3,
  col_palette = c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6"),
  bg_col = "white",
  bg_line_col = "grey70",
  line_col = "black",
  s = 1234
)
```

## Arguments

- n_x:

  Number of columns. Default 7.

- n_y:

  Number of rows. Default 7.

- prop:

  Proportion of squares to be nodes. Default 0.3.

- col_palette:

  Colour palette. Default
  `c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6")`.

- bg_col:

  Background colour. Default `"white"`.

- bg_line_col:

  Background line colour. Default `"grey70"`.

- line_col:

  Line colour. Default `"black"`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
network()
```
