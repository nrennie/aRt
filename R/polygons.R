#' Choose n different levels.
#'
#' This function generates a vector of length size containing n different
#' factors.
#'
#' @param n Number of different factor levels.
#' @param size Length of vector to output.
#' @param seed Seed value. Default 1234.
#' @return A factor vector of length size containing n different numbers.
#' @noRd

n_col_select <- function(n, size, random = FALSE, s = 1234) {
  final_output <- withr::with_seed(
    seed = s,
    code = {
      r <- sample(1:n)
      o1 <- size * (r / sum(r))
      output <- round(o1)
      if (sum(output) < size) {
        output[1] <- output[1] + (size - sum(output))
      }
      if (sum(output) > size) {
        output[1] <- output[1] - (size - sum(output))
      }
      final_output <- rep(1:n, times = output)
      if (random == TRUE) {
        final_output <- factor(sample(final_output, size = size, replace = FALSE))
      } else {
        final_output <- factor(final_output)
      }
      final_output
    }
  )
  return(final_output)
}


#' Polygons
#'
#' This function generates a coloured generative art ggplot object using
#' polygons.
#'
#' @param n_x Number of polygons per row. Default 12.
#' @param n_y Number of polygons per column. Default 18.
#' @param gap_size Numeric between 0 and 1 specifying the size of the gap in
#' the polygons. Default 0.5.
#' @param deg_jitter Numeric between 0 and 0.5 specifying the degree of jitter.
#' Default 0.1.
#' @param colours Vector of colours. Can be any length. Default
#' `c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C")`.
#' @param rand Boolean for whether colours should be random or ordered.
#' Default FALSE.
#' @param bg_col Background colour. Default "gray97".
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @examples
#' polygons()
#' @export

polygons <- function(n_x = 12,
                     n_y = 18,
                     gap_size = 0.5,
                     deg_jitter = 0.1,
                     colours = c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C"),
                     rand = FALSE,
                     bg_col = "gray97",
                     s = 1234) {
  if (n_x < 1 || n_y < 1) {
    stop("Number of rows and columns must be at least 1")
  }
  if (gap_size < 0 || gap_size > 1) {
    stop("gap_size must be between 0 and 1")
  }
  if (deg_jitter < 0 || deg_jitter > 0.5) {
    stop("deg_jitter must be between 0 and 0.5")
  }
  datapoly <- withr::with_seed(
    seed = s,
    code = {
      n_x <- round(n_x)
      n_y <- round(n_y)
      x1 <- rep(1:n_x, times = n_y) + stats::runif(n_x * n_y, 0, deg_jitter)
      x2 <- rep(2:(n_x + 1), times = n_y) - stats::runif(n_x * n_y, 0, deg_jitter)
      x3 <- rep(2:(n_x + 1), times = n_y) - stats::runif(n_x * n_y, 0, deg_jitter)
      x4 <- rep(1:n_x, times = n_y) + stats::runif(n_x * n_y, 0, deg_jitter)
      x <- c(matrix(c(x1, x2, x3, x4), byrow = TRUE, nrow = 4, ncol = length(x1)))
      y1 <- rep(1:n_y, each = n_x) + stats::runif(n_x * n_y, 0, deg_jitter)
      y2 <- rep(1:n_y, each = n_x) + stats::runif(n_x * n_y, 0, deg_jitter)
      y3 <- rep(2:(n_y + 1), each = n_x) - stats::runif(n_x * n_y, 0, deg_jitter)
      y4 <- rep(2:(n_y + 1), each = n_x) - stats::runif(n_x * n_y, 0, deg_jitter)
      y <- c(matrix(c(y1, y2, y4, y3), byrow = TRUE, nrow = 4, ncol = length(y1)))
      id <- rep(1:(n_x * n_y), each = 4)
      positions <- data.frame(x = x, y = y, id = id)
      if (rand == FALSE) {
        col_choice <- n_col_select(n = length(colours), size = n_x * n_y, s = s)
      } else {
        col_choice <- sample(colours, size = n_x * n_y, replace = TRUE)
      }
      values <- data.frame(id = unique(id), cols = col_choice)
      datapoly <- merge(values, positions, by = c("id"))
      holes <- do.call(rbind, lapply(split(datapoly, datapoly$id), function(df) {
        df$x <- df$x + gap_size * (mean(df$x) - df$x)
        df$y <- df$y + gap_size * (mean(df$y) - df$y)
        df
      }))
      datapoly$subid <- 1L
      holes$subid <- 2L
      datapoly <- rbind(datapoly, holes)
      datapoly
    }
  )
  p <- ggplot2::ggplot(
    data = datapoly,
    mapping = ggplot2::aes(x = .data$x, y = .data$y)
  ) +
    ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        group = .data$id,
        fill = .data$cols,
        subgroup = .data$subid
      ),
      colour = NA
    ) +
    ggplot2::scale_fill_manual(values = colours) +
    theme_aRt(bg_col)
  return(p)
}
