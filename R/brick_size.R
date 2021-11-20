#' Brick size
#'
#' This function generates a vector of length n which adds to 100.
#'
#' @param n Length of vector to output.
#' @return A factor vector of length n, containing bricks sizes.
#' @noRd
#'

brick_size <- function(n){
  size <- 100
  r <- sample(1:n)
  o1 <- size*(r/sum(r))
  output <- round(o1)
  if (sum(output) < size){
    output[1] <- output[1] + (size - sum(output))
  }
  if (sum(output) > size){
    output[1] <- output[1] - (size - sum(output))
  }
  return(output)
}

