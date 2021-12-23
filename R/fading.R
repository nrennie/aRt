#' Fading
#'
#' This function generates a coloured generative art ggplot object using voronoi tiles.
#'
#' @param n_layers Number of layers. Default 6.
#' @param n_points Number of points per layer area. Default 10.
#' @param col_palette Colour palette from rcartocolor. Default "SunsetDark".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggforce
#' @import rcartocolor
#' @import tibble
#' @import dplyr
#' @export

fading <- function(n_layers=6, n_points=10, col_palette="SunsetDark", s=1234){
  #generate data
  n_points <- n_points*(n_layers:1)
  x_widths <- 2*(n_layers:1)
  x_lower <- cumsum(2*(n_layers:1))
  x_upper <- x_lower + x_widths
  y_widths <- 4*(n_layers:1)
  y_lower <- cumsum(4*(n_layers:1))
  y_upper <- y_lower + y_widths
  y <- unlist(lapply(1:n_layers, function(i) round(stats::runif(n_points[i], y_lower[i], y_upper[i]), 1)))
  x <- round(-0.5*y + unlist(lapply(1:n_layers, function(i) round(stats::runif(n_points[i], x_lower[i], x_upper[i]), 1))), 1)
  z <- y + stats::rnorm(length(x), 0, 0.5)
  df <- tibble(x=x, y=y, z=z) %>% filter(!is.na(y))

  #make plot
  p <- ggplot(df, aes(x=x, y=y, group = -1L)) +
    geom_voronoi_tile(aes(fill=z)) +
    coord_cartesian(expand=F) +
    scale_fill_carto_c("", type = "diverging", palette = col_palette, direction = -1) +
    theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.position="none",
        plot.margin = unit(c(-0.5,-0.5,-0.5,-0.5), "cm"), #top, right, bottom, left
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank(),
        axis.ticks.x= element_blank(),
        axis.ticks.y= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
  suppressWarnings(print(p))
}















