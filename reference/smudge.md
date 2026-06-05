# Smudge

This function generates a coloured generative art ggplot object from
contours.

## Usage

``` r
smudge(
  n = 25,
  binwidth = 0.01,
  col_palette = c("#552000", "#8a4d00", "#c17d17", "#f8b150", "#f5f5f5", "#93c6e1",
    "#5f93ac", "#2e627a", "#00344a"),
  s = 1234
)
```

## Arguments

- n:

  Number of grid boxes. Default 25.

- binwidth:

  Binwidth for colours. Default 0.01.

- col_palette:

  Vector of colours. Default
  `c("#552000", "#8a4d00", "#c17d17", "#f8b150", "#f5f5f5", "#93c6e1", "#5f93ac", "#2e627a", "#00344a")`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
smudge()
```
