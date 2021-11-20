#' Choose n different levels.
#'
#' This function generates a vector of length size containing n different factors.
#'
#' @param n Number of different factor levels.
#' @param size Length of vector to output.
#' @param seed Seed value. Default 1234.
#' @return A factor vector of length size containing n different numbers.
#' @noRd
#'

n_col_select <- function(n, size, random=F, s=1234){
  r <- sample(1:n)
  o1 <- size*(r/sum(r))
  output <- round(o1)
  if (sum(output) < size){
    output[1] <- output[1] + (size - sum(output))
  }
  if (sum(output) > size){
    output[1] <- output[1] - (size - sum(output))
  }
  final_output <- rep(1:n, times=output)
  if (random == T){
    return(factor(sample(final_output, size=size, replace=F)))
  } else{
    return(factor(final_output))
  }
}

