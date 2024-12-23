#' Moiré
#'
#' This function creates generative art inspired by the Moiré effect.
#'
#' @param inner_n Number of bottom left circles. Default 20.
#' @param dist Distance between bottom left and top right blocks. Default 4.
#' @param inner_col Colour of bottom left circles. Default "grey40".
#' @param outer_col Colour of top right circles. Default "grey60".
#' @param bg_col Background colour. Default "grey10".
#' @param inner_r Radius of bottom left circles. Default 0.5.
#' @param outer_r Radius of top right circles. Default 0.2.
#' @return A ggplot object.
#' @examples
#' moire()
#' @export

moire <- function(inner_n = 20,
                  dist = 10,
                  inner_col = "grey40",
                  outer_col = "grey60",
                  bg_col = "grey10",
                  inner_r = 0.5,
                  outer_r = 0.2) {
  # inner square
  main1 <- expand.grid(
    x = seq(1, inner_n, by = 2),
    y = seq(1, inner_n, by = 2)
  )
  main2 <- expand.grid(
    x = seq(2, inner_n, by = 2),
    y = seq(2, inner_n, by = 2)
  )
  main_square <- rbind(main1, main2) |>
    tibble::as_tibble()
  # top right
  tr1 <- expand.grid(
    x = seq(inner_n - dist, inner_n + dist + 0.5, by = 1),
    y = seq(inner_n - dist, inner_n + dist + 0.5, by = 1)
  )
  tr2 <- expand.grid(
    x = seq(inner_n - dist + 0.5, inner_n + dist + 0.5, by = 1),
    y = seq(inner_n - dist + 0.5, inner_n + dist + 0.5, by = 1)
  )
  topright <- rbind(tr1, tr2) |>
    tibble::as_tibble()

  # plot
  p <- ggplot2::ggplot() +
    ggforce::geom_circle(
      data = main_square,
      mapping = ggplot2::aes(
        x0 = .data$x,
        y0 = .data$y,
        r = inner_r
      ),
      fill = inner_col, colour = NA
    ) +
    ggforce::geom_circle(
      data = topright,
      mapping = ggplot2::aes(
        x0 = .data$x,
        y0 = .data$y,
        r = outer_r
      ),
      fill = outer_col, colour = NA
    ) +
    ggplot2::coord_fixed() +
    theme_aRt(bg_col)
  q <- cowplot::ggdraw() +
    cowplot::draw_plot(p) +
    ggplot2::theme(plot.background = ggplot2::element_rect(
      fill = bg_col,
      colour = bg_col
    ))
  return(q)
}
