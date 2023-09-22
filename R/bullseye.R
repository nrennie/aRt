#' Bullseye
#'
#' This function generates a layered generative art ggplot object using polar
#' bar charts.
#'
#' @param main_col Colour scheme of art. One of `c("mono", "rainbow)`.
#' Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

bullseye <- function(main_col = "black", bg_col = "white", s = 1234) {
  # generate data
  set.seed(s)
  df1 <- data.frame(id = seq(1, 20), value = sample(seq(-20, 100), 20, replace = TRUE))
  df2 <- data.frame(id = seq(1, 20), value = sample(seq(-20, 100), 20, replace = TRUE))
  df3 <- data.frame(id = seq(1, 40), value = sample(seq(-20, 100), 40, replace = TRUE))
  df4 <- data.frame(id = seq(1, 40), value = sample(seq(-20, 100), 40, replace = TRUE))
  df5 <- data.frame(id = seq(1, 60), value = sample(seq(-20, 100), 60, replace = TRUE))
  df6 <- data.frame(id = seq(1, 60), value = sample(seq(-20, 100), 60, replace = TRUE))
  df7 <- data.frame(id = seq(1, 80), value = sample(seq(-20, 100), 80, replace = TRUE))
  df8 <- data.frame(id = seq(1, 80), value = sample(seq(-20, 100), 80, replace = TRUE))
  # make plot layers
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df1, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 1, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = df2, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 0.2, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::ylim(-20, 100) +
    ggplot2::coord_polar(start = 0) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"), # top, right, bottom, left
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df3, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 1, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = df4, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 0.2, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::ylim(-30, 100) +
    ggplot2::coord_polar(start = 45) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"), # top, right, bottom, left
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  p3 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df5, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 1, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = df6, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 0.2, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::ylim(-40, 100) +
    ggplot2::coord_polar(start = 90) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"), # top, right, bottom, left
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  p4 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df7, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 1, fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = df8, mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity", width = 0.2, fill = ggplot2::alpha(main_col, 0.4)
    ) +
    ggplot2::ylim(-50, 100) +
    ggplot2::coord_polar(start = 135) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = "transparent"),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      legend.position = "none",
      plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"), # top, right, bottom, left
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  # join plots
  p <- p1 +
    patchwork::inset_element(p2, left = 0, bottom = 0, right = 1, top = 1) +
    patchwork::inset_element(p3, left = 0, bottom = 0, right = 1, top = 1) +
    patchwork::inset_element(p4, left = 0, bottom = 0, right = 1, top = 1) &
    ggplot2::theme(plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"))
  p
}
