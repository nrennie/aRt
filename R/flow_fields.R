#' Flow fields
#'
#' This function generates a generative art ggplot object from using particle traces.
#' See also https://www.williamrchase.com/post/flow-fields-12-months-of-art-september/
#'
#' @param n Number of lines. Default 10000.
#' @param granularity How fine to draw the grid. Default 1000.
#' @param x_freq Frequency of x simplex noise. Default 1.
#' @param y_freq Frequency of y simplex noise. Default 1.
#' @param alpha Transparency of lines. Default 1.
#' @param line_col Line colours. Vector (or single element) of colours. Default c("#edf8fb","#bfd3e6","#9ebcda","#8c96c6","#8c6bb1","#88419d","#6e016b")
#' @param bg_col Background colour. Default "white".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @export

flow_fields <- function(n = 10000,
                        granularity = 1000,
                        x_freq = 1,
                        y_freq = 1,
                        alpha = 1,
                        line_col = c("#edf8fb", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#6e016b"),
                        bg_col = "lightgrey",
                        s = 1234) {
  # check if {particles} loaded
  if (!requireNamespace("particles")) {
    stop("Please install {particles} to use this function.")
  } else {
    if (!requireNamespace("ambient")) {
      stop("Please install {ambient} to use this function.")
    } else {
      set.seed(s)
      grid <- ambient::long_grid(seq(1, 10, length.out = granularity),
                                 seq(1, 10, length.out = granularity)) |>
        dplyr::mutate(
          x1 = .data$x + ambient::gen_simplex(x = .data$x, y = .data$y, frequency = x_freq),
          y1 = .data$y + ambient::gen_simplex(x = .data$x, y = .data$y, frequency = y_freq)
        )

      curl <- ambient::curl_noise(ambient::gen_perlin, x = grid$x1, y = grid$y1)
      grid$angle <- atan2(curl$y, curl$x) - atan2(grid$y1, grid$x1)

      field <- as.matrix(grid, grid$x, value = grid$angle)

      sim <- tidygraph::create_empty(n) |>
        particles::simulate(alpha_decay = 0, setup = particles::aquarium_genesis(vel_max = 0)) |>
        particles::wield(particles::reset_force, xvel = 0, yvel = 0) |>
        particles::wield(particles::field_force, angle = field, vel = 0.1, xlim = c(-5, 5), ylim = c(-5, 5)) |>
        particles::evolve(100, particles::record)

      traces <- data.frame(do.call(rbind, lapply(sim$history, particles::position)))
      names(traces) <- c("x", "y")

      traces <-
        traces |>
        dplyr::mutate(particle = rep(1:n, 100)) |>
        dplyr::group_by(.data$particle) |>
        dplyr::mutate(colour = sample(line_col, 1, replace = TRUE))

      p <- ggplot2::ggplot() +
        ggplot2::geom_path(data = traces,
                           mapping = ggplot2::aes(x = .data$x,
                                                  y = .data$y,
                                                  group = .data$particle,
                                                  colour = .data$colour),
                           size = 0.3,
                           alpha = alpha) +
        ggplot2::coord_cartesian(expand = FALSE) +
        ggplot2::scale_color_identity(guide = "none") +
        ggplot2::theme_void() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                       plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
                       legend.position = "none",
                       plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
      return(p)
    }
  }
}
