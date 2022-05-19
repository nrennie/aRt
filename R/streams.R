#' Streams
#'
#' This function generates a coloured generative art ggplot object from stream charts.
#'
#' @param bg_col Background colour. Default "white".
#' @param line_col Line colour. Default "white".
#' @param fill_col Vector of fill colours.
#' @param type Rotation of stream. Default "right".
#' @param s Seed value. Default 1234.
#' @importFrom dplyr %>%
#' @return A ggplot object.
#' @export

streams <- function(bg_col = "white",
                    line_col = "white",
                    fill_col = c("#5F4690", "#1D6996", "#38A6A5", "#0F8554",
                                 "#73AF48", "#EDAD08", "#E17C05", "#CC503E",
                                 "#94346E", "#6F4070"),
                    type = "right",
                    s = 1234) {
  if (!(type %in% c("up", "down", "left", "right"))) {
    stop('Type must be one of "up", "down", "left", or "right"')
  }
  set.seed(s)
  # make data
  n <- length(fill_col)
  df <- purrr::map_dfr(.x = 1:n,
                       .f = ~{
                         x <- 1:sample(1:10, 1)
                         tibble::tibble(x = x + sample(1:10, 1)) %>%
                         dplyr::mutate(y = sample(1:10, length(x), replace = TRUE),
                                       z = as.character(.x))
                       })
  # plot
  p <- ggplot2::ggplot(data = df,
                       mapping = ggplot2::aes(x = .data$x,
                                              y = .data$y,
                                              fill = .data$z)) +
    ggstream::geom_stream(color = line_col,
                          sorting = "onset") +
    ggplot2::scale_fill_manual(values = fill_col) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = bg_col,
                                                            colour = bg_col),
                   plot.background = ggplot2::element_rect(fill = bg_col,
                                                           colour = bg_col))
  #rotate
  if (type == "up") {
    p <- p + coord_flip(expand = FALSE)
  }
  else if (type == "left") {
    p <- p + coord_cartesian(expand = FALSE) + scale_x_reverse()
  }
  else if (type == "down") {
    p <- p + coord_flip(expand = FALSE) + scale_x_reverse()
  }
  else if (type == "right") {
    p <- p + coord_cartesian(expand = FALSE)
  }
  return(p)
}
