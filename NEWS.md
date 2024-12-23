# aRt 1.5.0

* use {withr} for random seeds
* `mosiac()` no longer adds blank space
* remove `shells()`
* remove `static()`
* remove `connected()`
* remove `heart()`
* remove `infinity()`
* remove `contours()` (now in new package)
* remove `imploding_hexagon()`
* remove `mosaic_sketch()`
* rename `random_tessellation()` as `fragmentum()`
* rename `split_grid()` as `disarray()`
* rename `split_jitter()` as `distort()`

# aRt 1.4.7

* Add `stackture()`

# aRt 1.4.6

* Add `network()`

# aRt 1.4.5

* Use remote dependency for {ggvoronoi}

# aRt 1.4.4

* Add `riso_circles()`
* Add `crosshatch()`
* Add `imploding_hexagon()`
* Add `split_grid()`
* Add `split_jitter()`
* Add `wander()`
* Add `rings()`
* Add `mirrored()`
* Add `chaos()`
* Add `criss_cross()`
* Refactor `contours()` to use {tidyterra}
* Refactor `spirals()` to remove {ExtDist} dependency
* Add {terra} and {tidyterra} to Imports
* Update linting

# aRt 1.4.3

* Add `random_tessellation()`
* Add `sunsets()`
* Add `spiro()`
* Rename `windows()` to `window_boxes()`
* Rename `lines()` to `perpendicular()`

# aRt 1.4.2

* Add `gradients()`
* Move {ambient} to Suggests

# aRt 1.4.1

* Fix: bug introduced in `divide()` when switching to native pipe
* Fix: change to using `after_stat()`
* Fix: use `quietly = TRUE` when checking packages 

# aRt 1.4.0

* Add `shatter()`
* Fix missing namespace in `polygons()`
* Move {rayshader}, {deldir}, {elevatr}, {metR}, {particles} from Imports to Suggests
* Add {PrettyCols} to Suggests
* Lines in `lines()` are now centre aligned
* Remove {magrittr} from Imports and use native pipe
* Bump R version in Depends to 4.1

# aRt 1.3.3

* Add `windows()`

# aRt 1.3.2

* Fix bug in `flow_fields()`
* Update `mosaic_sketch()` to use `linewidth`
* Minimum ggplot2 version 3.4.0

# aRt 1.3.1

* Fix spelling of `granularity` in `flow_fields()`

# aRt 1.3.0

* add {lwgeom}, {cowplot}, {sf}, {grdevices} to Imports
* add `divide()` function
* add `riley()` function
* add `squiggles()` function
* add `smudge()` function
* add `lines()` function
* add `moire()` function

# aRt 1.2.9

* add `contours()` function
* add `stacked()` function
* add {metR}, {elevatr}, {raster}, and {rayshader} dependencies
* Minor bug fix to `tiles()`

# aRt 1.2.8

* add `puzzles()` function

# aRt 1.2.7

* add `black_hole()` function
* move internal functions to different files

# aRt 1.2.6

* add `abacus()` function
* add `mosaic_sketch()` function

# aRt 1.2.5

* add `mosaic()` function

# aRt 1.2.4

* add `squares()` function
* fix linting

# aRt 1.2.3

* add `fractals()` function

# aRt 1.2.2

* update pkgdown website

# aRt 1.2.1

* update `waves()` to use vector of colours

# aRt 1.2.0

* add `streams()` function
* Move {MetBrewer} to Suggests
* Move {RColorBrewer} to Suggests
* Functions take colour vectors instead of palette names as inputs

# aRt 1.1.9

* update license

# aRt 1.1.8

* add `sunbursts()` function
* add panel function
* add {pkgdown website}

# aRt 1.1.7

* add GitHub actions
* fix CI

# aRt 1.1.6

* Fix issues with `flow_fields()` rendering
* Add custom `ggplot2` theme
* add aRtData dependency
* add `blending()` function

# aRt 1.1.5

* Switch `flow_fields()` to cartesian coords

# aRt 1.1.4

* Add `flow_fields()`
* Fix missing `ggplot2::` in circles
* Fix `no visible binding for global variable "new_group"`

# aRt 1.1.3

* Installation bug fix

# aRt 1.1.2

* `bubbles()` function fixed

# aRt 1.1.1

* `bubbles()` has been added.

# aRt 1.1.0

* `rectangles()` has been added.
* News.md file now included.

