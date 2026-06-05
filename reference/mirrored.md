# Mirrored

This function generates a coloured generative art ggplot object from
rectangles.

## Usage

``` r
mirrored(
  n = 15,
  w = 4,
  col_palette = c("#420f75", "#7640a9", "#ad72d6", "#e7a8fb", "#f5f5f5", "#f8b150",
    "#c17d17", "#8a4d00", "#552000"),
  s = 1234
)
```

## Arguments

- n:

  Number of boxes per quadrant. Default 15.

- w:

  Weighting towards first colour of palette. Minimum of 2. Default 4.

- col_palette:

  Vector of colours. Default
  `c("#420f75", "#7640a9", "#ad72d6", "#e7a8fb", "#f5f5f5", "#f8b150", "#c17d17", "#8a4d00", "#552000")`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
mirrored()
```
