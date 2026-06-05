# Draw a generative locker-grid plot

Builds a grid of randomly sized, optionally subdivided rectangles with
rounded corners, coloured from a supplied palette.

## Usage

``` r
lockers(
  n_col = 5,
  min_rows = 5,
  max_rows = 9,
  r = 3,
  subdivide_prob = 0.2,
  min_c = 3,
  max_c = 8,
  flip = FALSE,
  col_palette = c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"),
  bg_col = "black",
  linewidth = 1,
  s = 1234
)
```

## Arguments

- n_col:

  Number of columns. Defaults to 5.

- min_rows:

  Minimum number of rows per column. Defaults to 5.

- max_rows:

  Maximum number of rows per column. Defaults to 9.

- r:

  Corner radius in points passed to
  [`ggchicklet::geom_rrect()`](https://rdrr.io/pkg/ggchicklet/man/geom_rrect.html).
  Defaults to 3.

- subdivide_prob:

  Probability that any given cell is horizontally subdivided. Defaults
  to 0.2.

- min_c:

  Minimum number of subdivisions when a cell is divided. Defaults to 3.

- max_c:

  Maximum number of subdivisions when a cell is divided. Defaults to 8.

- flip:

  Logical. If `TRUE`, axes are flipped via
  [`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).
  Defaults to `FALSE`.

- col_palette:

  Character vector of colours used to build the fill palette. Defaults
  to `c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B")`.

- bg_col:

  Background and border colour. Defaults to `"black"`.

- linewidth:

  Width of the border lines. Defaults to 1.

- s:

  Random seed for reproducibility. Defaults to 1234.

## Value

A `ggplot` object.

## Examples

``` r
lockers()
```
