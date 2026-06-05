# Changelog

## aRt 1.5.2

- Deprecate `attraction()`, `sunbursts()`, `streams()`, `flow_fields()`,
  `waves()`, `circular()`, `squiggles()`, `stripes()`, `circles()`,
  `criss_cross()`, `moire()`, and `spirals()`.
- Use vectors of colours for inputs
- Small fixes for consistency
- Add [`lockers()`](https://nrennie.github.io/aRt/reference/lockers.md)
- Rename `riso_circles()` and
  [`riso()`](https://nrennie.github.io/aRt/reference/riso.md)
- Use R Markdown for README examples

## aRt 1.5.1

- Use `col_palette` for argument name
- Refactor
  [`bullseye()`](https://nrennie.github.io/aRt/reference/bullseye.md) to
  allow palettes

## aRt 1.5.0

- use {withr} for random seeds
- `mosiac()` no longer adds blank space
- remove `shells()`
- remove `static()`
- remove `connected()`
- remove `heart()`
- remove `infinity()`
- remove `contours()` (now in new package)
- remove `imploding_hexagon()`
- remove `mosaic_sketch()`
- rename `random_tessellation()` as
  [`fragmentum()`](https://nrennie.github.io/aRt/reference/fragmentum.md)
- rename `split_grid()` as
  [`disarray()`](https://nrennie.github.io/aRt/reference/disarray.md)
- rename `split_jitter()` as
  [`distort()`](https://nrennie.github.io/aRt/reference/distort.md)

## aRt 1.4.7

- Add
  [`stackture()`](https://nrennie.github.io/aRt/reference/stackture.md)

## aRt 1.4.6

- Add [`network()`](https://nrennie.github.io/aRt/reference/network.md)

## aRt 1.4.5

- Use remote dependency for {ggvoronoi}

## aRt 1.4.4

- Add `riso_circles()`
- Add
  [`crosshatch()`](https://nrennie.github.io/aRt/reference/crosshatch.md)
- Add `imploding_hexagon()`
- Add `split_grid()`
- Add `split_jitter()`
- Add [`wander()`](https://nrennie.github.io/aRt/reference/wander.md)
- Add [`rings()`](https://nrennie.github.io/aRt/reference/rings.md)
- Add
  [`mirrored()`](https://nrennie.github.io/aRt/reference/mirrored.md)
- Add [`chaos()`](https://nrennie.github.io/aRt/reference/chaos.md)
- Add `criss_cross()`
- Refactor `contours()` to use {tidyterra}
- Refactor `spirals()` to remove {ExtDist} dependency
- Add {terra} and {tidyterra} to Imports
- Update linting

## aRt 1.4.3

- Add `random_tessellation()`
- Add [`sunsets()`](https://nrennie.github.io/aRt/reference/sunsets.md)
- Add [`spiro()`](https://nrennie.github.io/aRt/reference/spiro.md)
- Rename `windows()` to `window_boxes()`
- Rename [`lines()`](https://rdrr.io/r/graphics/lines.html) to
  [`perpendicular()`](https://nrennie.github.io/aRt/reference/perpendicular.md)

## aRt 1.4.2

- Add
  [`gradients()`](https://nrennie.github.io/aRt/reference/gradients.md)
- Move {ambient} to Suggests

## aRt 1.4.1

- Fix: bug introduced in
  [`divide()`](https://nrennie.github.io/aRt/reference/divide.md) when
  switching to native pipe
- Fix: change to using `after_stat()`
- Fix: use `quietly = TRUE` when checking packages

## aRt 1.4.0

- Add [`shatter()`](https://nrennie.github.io/aRt/reference/shatter.md)
- Fix missing namespace in
  [`polygons()`](https://nrennie.github.io/aRt/reference/polygons.md)
- Move {rayshader}, {deldir}, {elevatr}, {metR}, {particles} from
  Imports to Suggests
- Add {PrettyCols} to Suggests
- Lines in [`lines()`](https://rdrr.io/r/graphics/lines.html) are now
  centre aligned
- Remove {magrittr} from Imports and use native pipe
- Bump R version in Depends to 4.1

## aRt 1.3.3

- Add `windows()`

## aRt 1.3.2

- Fix bug in `flow_fields()`
- Update `mosaic_sketch()` to use `linewidth`
- Minimum ggplot2 version 3.4.0

## aRt 1.3.1

- Fix spelling of `granularity` in `flow_fields()`

## aRt 1.3.0

- add {lwgeom}, {cowplot}, {sf}, {grdevices} to Imports
- add [`divide()`](https://nrennie.github.io/aRt/reference/divide.md)
  function
- add [`riley()`](https://nrennie.github.io/aRt/reference/riley.md)
  function
- add `squiggles()` function
- add [`smudge()`](https://nrennie.github.io/aRt/reference/smudge.md)
  function
- add [`lines()`](https://rdrr.io/r/graphics/lines.html) function
- add `moire()` function

## aRt 1.2.9

- add `contours()` function
- add [`stacked()`](https://nrennie.github.io/aRt/reference/stacked.md)
  function
- add {metR}, {elevatr}, {raster}, and {rayshader} dependencies
- Minor bug fix to
  [`tiles()`](https://nrennie.github.io/aRt/reference/tiles.md)

## aRt 1.2.8

- add [`puzzles()`](https://nrennie.github.io/aRt/reference/puzzles.md)
  function

## aRt 1.2.7

- add
  [`black_hole()`](https://nrennie.github.io/aRt/reference/black_hole.md)
  function
- move internal functions to different files

## aRt 1.2.6

- add [`abacus()`](https://nrennie.github.io/aRt/reference/abacus.md)
  function
- add `mosaic_sketch()` function

## aRt 1.2.5

- add [`mosaic()`](https://nrennie.github.io/aRt/reference/mosaic.md)
  function

## aRt 1.2.4

- add [`squares()`](https://nrennie.github.io/aRt/reference/squares.md)
  function
- fix linting

## aRt 1.2.3

- add
  [`fractals()`](https://nrennie.github.io/aRt/reference/fractals.md)
  function

## aRt 1.2.2

- update pkgdown website

## aRt 1.2.1

- update `waves()` to use vector of colours

## aRt 1.2.0

- add `streams()` function
- Move {MetBrewer} to Suggests
- Move {RColorBrewer} to Suggests
- Functions take colour vectors instead of palette names as inputs

## aRt 1.1.9

- update license

## aRt 1.1.8

- add `sunbursts()` function
- add panel function
- add {pkgdown website}

## aRt 1.1.7

- add GitHub actions
- fix CI

## aRt 1.1.6

- Fix issues with `flow_fields()` rendering
- Add custom `ggplot2` theme
- add aRtData dependency
- add
  [`blending()`](https://nrennie.github.io/aRt/reference/blending.md)
  function

## aRt 1.1.5

- Switch `flow_fields()` to cartesian coords

## aRt 1.1.4

- Add `flow_fields()`
- Fix missing `ggplot2::` in circles
- Fix `no visible binding for global variable "new_group"`

## aRt 1.1.3

- Installation bug fix

## aRt 1.1.2

- [`bubbles()`](https://nrennie.github.io/aRt/reference/bubbles.md)
  function fixed

## aRt 1.1.1

- [`bubbles()`](https://nrennie.github.io/aRt/reference/bubbles.md) has
  been added.

## aRt 1.1.0

- [`rectangles()`](https://nrennie.github.io/aRt/reference/rectangles.md)
  has been added.
- News.md file now included.
