# Global variables
utils::globalVariables(c("density", ".", ".data"))



#' Generates a random hex code
#'
#' @param n Number of hex codes to generate
#' @return Character string of hex codes
#' @noRd
random_hex <- function(n) {
  generate_hex <- function() {
    choices <- sample(c(as.character(0:9), LETTERS[1:6]), size = 6, replace = TRUE)
    output <- paste0("#", stringr::str_flatten(choices))
    return(output)
  }
  hex <- replicate(n = n, generate_hex(), simplify = TRUE)
  return(hex)
}



#' aRt theme
#'
#' Custom ggplot2 theme for aRt objects
#'
#' @noRd
theme_aRt <- function(bg_col, padding = 0) { # nolint
  ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, colour = bg_col
      ),
      legend.position = "none",
      plot.margin = ggplot2::unit(
        c(padding, padding, padding, padding), "cm"
      ),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "cm")
    )
}
