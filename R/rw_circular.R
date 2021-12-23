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
#'
#'

rw_circular <- function(n, p=0.5, lower_limit=10, mid_limit=90, upper_limit=100, seed=1234){
  set.seed(seed)
  output <- numeric(length=n)
  output[1] <- 0
  for (i in 2:n){
    if (output[i-1] < lower_limit) {
      output[i] <- max(0+stats::runif(1,0,0.5), output[i-1] + sample(c(-1, (stats::rgeom(n=1, prob=0.8)+1)), size=1, prob=c(p, 1-p)))
    } else if(output[i-1] >= lower_limit & output[i-1] < mid_limit) {
      output[i] <- output[i-1] + sample(c(-1, (stats::rgeom(n=1, prob=0.2)+1)), size=1, prob=c(p, 1-p))
    } else if(output[i-1] >= mid_limit & output[i-1] < upper_limit) {
      output[i] <- output[i-1] + sample(c(-1, (stats::rgeom(n=1, prob=0.7)+1)), size=1, prob=c(p, 1-p))
    } else if (output[i-1] >= upper_limit){
      output[i] <- upper_limit
    }
  }
  return(output)
}
