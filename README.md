<h1 align="center">
aRt</h1>

aRt is an R package to create generative art. Install using:
``` r
devtools::install_github("nrennie/aRt")
```

<h2 align="center">
circular</h2>

The `circular()` function produces circular generative art produced by many random paths from the centre of the circle.

``` r
circular(n=2, main_col="black", bg_col="white", s=56)
circular(n=10, main_col="black", bg_col="white", s=56)
circular(n=100, main_col="black", bg_col="white", s=56)
```
<p align="center">
<img src="/images/circular_n2.jpeg?raw=true" width="30%">
<img src="/images/circular_n10.jpeg?raw=true" width="30%">
<img src="/images/circular_n100.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
boxes</h2>

The `boxes()` function produces generative art of many boxes.

``` r
boxes(n=100, perc=0.1, col_palette="DarkMint", bg_col="black", s=1234)
boxes(n=20, perc=0.1, col_palette="DarkMint", bg_col="black", s=1234)
boxes(n=100, perc=0.5, col_palette="Magenta", bg_col="black", s=1234)
```
<p align="center">
<img src="/images/boxes_n100_p1.jpeg?raw=true" width="30%">
<img src="/images/boxes_n20_p1.jpeg?raw=true" width="30%">
<img src="/images/boxes_n100_p5.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
bullseye</h2>

The `bullseye()` function produces generative art using layered polar bar charts.

``` r
bullseye(main_col="black", bg_col="white", s=1234)
bullseye(main_col="white", bg_col="black", s=1234)
bullseye(main_col="black", bg_col="white", s=2021)
```
<p align="center">
<img src="/images/bullseye_1234.jpeg?raw=true" width="30%">
<img src="/images/bullseye_1234n.jpeg?raw=true" width="30%">
<img src="/images/bullseye_2021.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
fading</h2>

The `fading()` function produces generative art using voronoi tiles.

``` r
fading(n_layers=6, n_points=10, col_palette="SunsetDark", s=1234)
fading(n_layers=6, n_points=1, col_palette="Sunset", s=1234)
fading(n_layers=10, n_points=10, col_palette="SunsetDark", s=1234)
```
<p align="center">
<img src="/images/fading_6_10.jpeg?raw=true" width="30%">
<img src="/images/fading_6_1.jpeg?raw=true" width="30%">
<img src="/images/fading_10_10.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
connected</h2>

The `connected()` function produces generative art using voronoi tiles.

``` r
connected(n=100, n_geom=10, random=F, col_palette="RdPu", bg_col="#ae217e", s=1234)
connected(n=100, n_geom=10, random=T, col_palette="RdPu", bg_col="#ae217e", s=1234)
connected(n=250, n_geom=2, random=F, col_palette="RdPu", bg_col="#ae217e", s=1234)
```
<p align="center">
<img src="/images/connected_100_10F.jpeg?raw=true" width="30%">
<img src="/images/connected_100_10T.jpeg?raw=true" width="30%">
<img src="/images/connected_250_2F.jpeg?raw=true" width="30%">
</p>


<h2 align="center">
vortex</h2>

The `vortex()` function produces circular vortex generative art produced by a line plot and polar coordinates.

``` r
vortex(n=25, start_val=90, col_scheme="mono", bg_col="black", s=1234)
vortex(n=100, start_val=90, col_scheme="mono", bg_col="black", s=1234)
vortex(n=25, start_val=90, col_scheme="rainbow", bg_col="black", s=1234)
```
<p align="center">
<img src="/images/vortex_n25_m.jpeg?raw=true" width="30%">
<img src="/images/vortex_n100_m.jpeg?raw=true" width="30%">
<img src="/images/vortex_n25_r.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
infinity</h2>

The `infinity()` function draws an infinity symbol from many overlapping lines.

``` r
infinity(n=25, col_scheme="mono", bg_col="black", s=1234)
infinity(n=100, col_scheme="mono", bg_col="black", s=1234)
infinity(n=25, col_scheme="rainbow", bg_col="black", s=1234)
```
<p align="center">
<img src="/images/infinity_n25_m.jpeg?raw=true" width="30%">
<img src="/images/infinity_n100_m.jpeg?raw=true" width="30%">
<img src="/images/infinity_n25_r.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
heart</h2>

The `heart()` function draws a heart from many overlapping lines.

``` r
heart(n=25, col_scheme="mono", bg_col="black", s=1234)
heart(n=100, col_scheme="mono", bg_col="black", s=1234)
heart(n=25, col_scheme="rainbow", bg_col="black", s=1234)
```
<p align="center">
<img src="/images/heart_n25_m.jpeg?raw=true" width="30%">
<img src="/images/heart_n100_m.jpeg?raw=true" width="30%">
<img src="/images/heart_n25_r.jpeg?raw=true" width="30%">
</p>



<h2 align="center">
static</h2>

The `static()` function produces generative art reminiscent of the noise displayed on analog televisions when no transmission signal is received.

``` r
static(perc=0.01, n=500, s=1234)
static(perc=0.1, n=500, s=1234)
static(perc=0.3, n=500, s=1234)
```
<p align="center">
<img src="/images/static_p01_n500.jpeg?raw=true" width="30%">
<img src="/images/static_p10_n500.jpeg?raw=true" width="30%">
<img src="/images/static_p30_n500.jpeg?raw=true" width="30%">
</p>


<h2 align="center">
stripes</h2>

The `stripes()` function produces generative art consisting of rows of vertical stripes.

``` r
stripes(perc=0, n=3, s=1234)
stripes(perc=0.5, n=3, s=1234)
stripes(perc=1, n=3, s=1234)
```
<p align="center">
<img src="/images/stripes_p00_n3.jpeg?raw=true" width="30%">
<img src="/images/stripes_p50_n3.jpeg?raw=true" width="30%">
<img src="/images/stripes_p100_n3.jpeg?raw=true" width="30%">
</p>

<h2 align="center">
spirals</h2>

The `spirals()` function produces generative art consisting of dots arranged in a spiral.

``` r
spirals(perc=0.2, s=1234)
spirals(perc=0.5, s=1234)
spirals(perc=0.8, s=1234)
```
<p align="center">
<img src="/images/spirals_p2.jpeg?raw=true" width="30%">
<img src="/images/spirals_p5.jpeg?raw=true" width="30%">
<img src="/images/spirals_p8.jpeg?raw=true" width="30%">
</p>

