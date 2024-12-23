#' Bullseye
#'
#' This function generates a layered generative art ggplot object using polar
#' bar charts.
#'
#' @param main_col Colour scheme of art. Default "black".
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' bullseye()
#' @export

bullseye <- function(main_col = "black",
                     bg_col = "white",
                     s = 1234) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      df1 <- data.frame(id = seq(1, 20), value = sample(seq(-20, 100), 20, replace = TRUE), grp = 1)
      df2 <- data.frame(id = seq(1, 20), value = sample(seq(-20, 100), 20, replace = TRUE), grp = 2)
      df3 <- data.frame(id = seq(1, 40), value = sample(seq(-20, 100), 40, replace = TRUE), grp = 3)
      df4 <- data.frame(id = seq(1, 40), value = sample(seq(-20, 100), 40, replace = TRUE), grp = 4)
      df5 <- data.frame(id = seq(1, 60), value = sample(seq(-20, 100), 60, replace = TRUE), grp = 5)
      df6 <- data.frame(id = seq(1, 60), value = sample(seq(-20, 100), 60, replace = TRUE), grp = 6)
      df7 <- data.frame(id = seq(1, 80), value = sample(seq(-20, 100), 80, replace = TRUE), grp = 7)
      df8 <- data.frame(id = seq(1, 80), value = sample(seq(-20, 100), 80, replace = TRUE), grp = 8)
      plot_data <- do.call(rbind, list(df1, df2, df3, df4, df5, df6, df7, df8))
      plot_data
    }
  )
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 1),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 1,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 2),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 0.2,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::ylim(-20, 100) +
    ggplot2::coord_polar(start = 0) +
    theme_aRt(bg_col, -0.5)
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 3),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 1,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 4),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 0.2,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::ylim(-30, 100) +
    ggplot2::coord_polar(start = 45) +
    theme_aRt("transparent", -0.5)
  p3 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 5),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 1,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 6),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 0.2,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::ylim(-40, 100) +
    ggplot2::coord_polar(start = 90) +
    theme_aRt("transparent", -0.5)
  p4 <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 7),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 1,
      fill = ggplot2::alpha(main_col, 0.3)
    ) +
    ggplot2::geom_bar(
      data = dplyr::filter(plot_data, grp == 8),
      mapping = ggplot2::aes(x = as.factor(.data$id), y = .data$value),
      stat = "identity",
      width = 0.2,
      fill = ggplot2::alpha(main_col, 0.4)
    ) +
    ggplot2::ylim(-50, 100) +
    ggplot2::coord_polar(start = 135) +
    theme_aRt("transparent", -0.5)
  p <- p1 +
    patchwork::inset_element(p2, left = 0, bottom = 0, right = 1, top = 1) +
    patchwork::inset_element(p3, left = 0, bottom = 0, right = 1, top = 1) +
    patchwork::inset_element(p4, left = 0, bottom = 0, right = 1, top = 1) &
    ggplot2::theme(plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"))
  return(p)
}
