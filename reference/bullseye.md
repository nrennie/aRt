# Bullseye

This function generates a layered generative art ggplot object using
polar bar charts.

## Usage

``` r
bullseye(
  col_palette = c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6"),
  bg_col = "white",
  alpha = 0.3,
  s = 1234
)
```

## Arguments

- col_palette:

  Colour palette. Default
  `c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6")`.

- bg_col:

  Background colour. Default `"white"`.

- alpha:

  Transparency. Default 0.3.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
bullseye()
```
