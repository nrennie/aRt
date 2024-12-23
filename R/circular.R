#' Random Walk Circular
#'
#' This function generates a greyscale generative art ggplot object.
#'
#' @param n Number of steps from inside to outside. Default 100.
#' @param p Probability of a forward step. Default 0.5.
#' @param lower_limit Inner radius where steps are smaller. Default 10.
#' @param mid_limit Middle radius where steps are larger. Default 100.
#' @param upper_limit Outer radius of circle. Default 100.
#' @param seed Seed value. Default 1234.
#' @noRd

rw_circular <- function(
    n, p = 0.5,
    lower_limit = 10, mid_limit = 90,
    upper_limit = 100, seed = 1234) {
  output <- withr::with_seed(
    seed = seed,
    code = {
      output <- numeric(length = n)
      output[1] <- 0
      for (i in 2:n) {
        if (output[i - 1] < lower_limit) {
          output[i] <- max(
            0 + stats::runif(1, 0, 0.5),
            output[i - 1] +
              sample(c(-1, (stats::rgeom(n = 1, prob = 0.8) + 1)),
                size = 1,
                prob = c(p, 1 - p)
              )
          )
        } else if (output[i - 1] >= lower_limit && output[i - 1] < mid_limit) {
          output[i] <- output[i - 1] +
            sample(c(-1, (stats::rgeom(n = 1, prob = 0.2) + 1)),
              size = 1,
              prob = c(p, 1 - p)
            )
        } else if (output[i - 1] >= mid_limit && output[i - 1] < upper_limit) {
          output[i] <- output[i - 1] +
            sample(c(-1, (stats::rgeom(n = 1, prob = 0.7) + 1)),
              size = 1,
              prob = c(p, 1 - p)
            )
        } else if (output[i - 1] >= upper_limit) {
          output[i] <- upper_limit
        }
      }
      output
    }
  )
  return(output)
}


#' Circular
#'
#' This function generates an abstract circular generative art ggplot object.
#'
#' @param n Number of steps from inside to outside. Default 100.
#' @param main_col Colour of lines. Default black.
#' @param bg_col Background colour. Default white.
#' @param s Seed value. Default 56.
#' @return A ggplot object.
#' @examples
#' circular()
#' @export

circular <- function(n = 100,
                     main_col = "black",
                     bg_col = "white",
                     s = 56) {
  if (n < 1) {
    stop("n must be an integer greater than 1")
  }

  plot_data <- withr::with_seed(
    seed = s,
    code = {
      output_mat <- matrix(NA, nrow = 360, ncol = n)
      for (i in 1:360) {
        output_mat[i, ] <- rw_circular(
          n = n, p = 0.5, lower_limit = 10, mid_limit = 90,
          upper_limit = 100, seed = i * s
        )
      }
      colnames(output_mat) <- 1:n
      plot_data <- tibble::tibble(val = 1:360, tibble::as_tibble(output_mat))
      plot_data <- tidyr::pivot_longer(plot_data, cols = 2:(n + 1))
      plot_data
    }
  )

  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(x = .data$val, y = .data$name, group = .data$val)
  ) +
    ggplot2::geom_line(colour = main_col) +
    ggplot2::coord_polar(start = 0) +
    theme_aRt(bg_col)
  return(p)
}
