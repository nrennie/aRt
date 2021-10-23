#' Static
#'
#' This function generates a greyscale generative art ggplot object.
#'
#' @param perc Percentage of data points to be non-NA. Default 0.1.
#' @param n Number of squares. Default 500.
#' @param s Seed value. Default 1234.
#' @return A ggplot object.
#' @import ggplot2
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @export

static <- function(perc=0.1, n=500, s=1234){
  if(perc < 0 | perc > 1) stop('perc not between 0 and 1')
  if(n < 1) stop('n must be a positive integer')
  set.seed(s)
  plot_df <- matrix(NA, ncol=n, nrow=n)
  colnames(plot_df) <- 1:ncol(plot_df)
  rownames(plot_df) <- 1:nrow(plot_df)
  plot_data <- tibble(times=1:nrow(plot_df), tibble::as_tibble(plot_df)) %>%
    pivot_longer(cols=2:(ncol(plot_df)+1))
  plot_data$value[sample(x=1:(ncol(plot_df)*nrow(plot_df)), size=round(perc*ncol(plot_df)*nrow(plot_df)), replace=F)] <- runif(round(perc*ncol(plot_df)*nrow(plot_df)))
  p <- ggplot(data=plot_data, aes(x=times, y=name, fill=value)) +
    geom_tile() +
    coord_cartesian(expand=F) +
    scale_fill_gradient(low="gray27", high="gray95", na.value = "gray27") +
    theme(panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title=element_blank(),
          plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm"),
          legend.position="none",
          legend.key = element_blank())
  return(p)
}



