#' Stripes
#'
#' This function generates a generative art ggplot object featuring rows of stripes.
#'
#' @param perc Percentage of data points to be sorted. Default 0.5.
#' @param n Number of rows. Default 3.
#' @param s Seed value. Default 1234.
#' @return A ggplot object
#' @export

stripes <- function(perc=0.5, n=3, s=1234){
  if(perc < 0 | perc > 1) stop('perc not between 0 and 1')
  if(n < 1) stop('n must be a positive integer')
  set.seed(s)
  plot_df <- matrix(NA, ncol=1000, nrow=n)
  for (i in 1:n){
    k <- runif(1000)
    vals <- sample(x=1:1000, size=perc*1000, replace=F)
    k[sort(vals)] <- sort(k[vals])
    plot_df[i,] <- k
  }
  colnames(plot_df) <- 1:ncol(plot_df)
  rownames(plot_df) <- 1:nrow(plot_df)
  plot_data <- tibble::tibble(times=1:nrow(plot_df), tibble::as_tibble(plot_df))
  plot_data <- tidyr::pivot_longer(plot_data, cols=2:(ncol(plot_df)+1))
  p <- ggplot2::ggplot(data=plot_data, ggplot2::aes(x=times, y=as.numeric(name), fill=value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_flip(expand=F) +
    ggplot2::scale_fill_gradient(low="#a1dab4", high="#253494", na.value = "white", limits=c(0,1)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"),
                   plot.background = ggplot2::element_rect(fill = "transparent"),
                   axis.text=ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title=ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
                   legend.position="none",
                   legend.key = ggplot2::element_blank())
  return(p)
}
