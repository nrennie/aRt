#' Draw Ellipse Circle
#'
#' This function generates data for an ellipse or circle.
#'
#' @param x0 x-coordinate of circle centre. Default 0.
#' @param y0 y-coordinate of circle centre. Default 0.
#' @param r vertical radius of circle. Default 5.
#' @param a ratio of horizontal to vertical radii.
#' @param n number of points to generate. Default 1000.
#' @param group group to identify part of same circle. Default 1.
#' @return a tibble
#' @noRd

draw_ellipse_circle <- function(x0 = 0,
                                y0 = 0,
                                r = 5, a = 1,
                                n = 1000,
                                group = 1) {
  theta <- seq(0, 2 * pi, length.out = n)
  tibble::tibble(
    x = a * r * cos(theta) + x0,
    y = r * sin(theta) + y0,
    group = rep(group, n)
  )
}

#' Draw Ellipse in Circle
#'
#' This function generates data for a circle filled with ellipses of the same vertical radius
#'
#' @param x0 x-coordinate of circle centre. Default 0.
#' @param y0 y-coordinate of circle centre. Default 0.
#' @param r vertical radius of circle. Default 5.
#' @param n number of points to generate. Default 1000.
#' @return a tibble
#' @noRd

draw_ellipse_in_circle <- function(x0 = 0,
                                   y0 = 0,
                                   r = 5,
                                   n = 1000) {
  plot_data <- data.frame(x = c(), y = c(), group = c())
  a <- seq(0, 1, 0.05)
  for (i in seq_along(a)) {
    k <- draw_ellipse_circle(x0 = x0, y0 = y0, r = r, a = a[i], n = n, group = i)
    plot_data <- rbind(plot_data, k)
  }
  plot_data
}


#' Bubbles
#'
#' This function generates a generative art ggplot object consisting of circles filled with ellipses.
#'
#' @param num_circles Number of circles. Default 20.
#' @param main_col Colour of non-highlighted rectangles. Default "black".
#' @param col_palette Vector of colours. Default `c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A")`.
#' @param col_prob Probability of choosing colour from `col_palette` instead of `main_col`.
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @examples
#' bubbles()
#' @export

bubbles <- function(num_circles = 20,
                    main_col = "black",
                    col_palette = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A"),
                    col_prob = 0.3,
                    bg_col = "white",
                    s = 1234) {
  plot_data <- withr::with_seed(
    seed = s,
    code = {
      x0 <- sample(1:(4 * num_circles), size = num_circles, replace = FALSE)
      y0 <- sample(1:(4 * num_circles), size = num_circles, replace = FALSE)
      r <- sample(1:(0.75 * num_circles), size = num_circles, replace = TRUE)
      plot_data <- data.frame(x = c(), y = c(), group = c(), group_circle = c())
      for (i in 1:num_circles) {
        k <- draw_ellipse_in_circle(x0 = x0[i], y0 = y0[i], r = r[i]) |>
          dplyr::mutate(
            group_circle = i,
            circle_col = as.character(
              sample(1:(length(col_palette) + 1),
                size = 1,
                prob = c(
                  rep(col_prob / length(col_palette), length(col_palette)),
                  (1 - col_prob)
                )
              )
            )
          )
        plot_data <- rbind(plot_data, k)
      }
      plot_data <- tidyr::unite(
        plot_data,
        col = "new_group",
        .data$group:.data$group_circle,
        sep = ":", remove = FALSE
      )
      plot_data
    }
  )

  pal <- c(col_palette, main_col)
  names(pal) <- 1:(length(col_palette) + 1)
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      group = .data$new_group,
      colour = .data$circle_col
    )
  ) +
    ggplot2::geom_path() +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::coord_fixed() +
    ggplot2::xlim(
      (min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))
    ) +
    ggplot2::ylim(
      (min(c(plot_data$x, plot_data$y))), (max(c(plot_data$x, plot_data$y)))
    ) +
    theme_aRt(bg_col)
  return(p)
}
