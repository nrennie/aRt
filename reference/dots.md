# Dots

This function generates a coloured generative art ggplot object using
polar coordinates.

## Usage

``` r
dots(
  n_x = 50,
  n_y = 100,
  jitter_size_width = 0.5,
  jitter_size_height = 0.5,
  col_palette = c("#F3E0F7", "#E4C7F1", "#D1AFE8", "#B998DD", "#9F82CE", "#826DBA",
    "#63589F"),
  bg_col = "#63589f",
  s = 1234
)
```

## Arguments

- n_x:

  Number of rotational points. Default 50.

- n_y:

  Number of outwards points. Default 100.

- jitter_size_width:

  Size of jitter width. Default 0.5.

- jitter_size_height:

  Size of jitter height. Default 0.5.

- col_palette:

  Vector of colours. Default
  `c("#F3E0F7", "#E4C7F1", "#D1AFE8", "#B998DD", "#9F82CE", "#826DBA", "#63589F")`.

- bg_col:

  Background colour. Default `"#63589f"`.

- s:

  Seed value. Default 1234.

## Value

A ggplot object.

## Examples

``` r
dots()
```
