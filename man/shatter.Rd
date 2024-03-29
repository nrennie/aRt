% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shatter.R
\name{shatter}
\alias{shatter}
\title{Shatter}
\usage{
shatter(
  n_x = 25,
  n_y = 25,
  decay = 0.8,
  colour = "black",
  bg_col = "gray97",
  s = 1234
)
}
\arguments{
\item{n_x}{Number of polygons per row. Default 25.}

\item{n_y}{Number of polygons per column. Default 25.}

\item{decay}{Numeric between 0 and 1 specifying the rate of decay if square shapes. Default 0.8.}

\item{colour}{Single colour for fill colour of polygons. Default "black".}

\item{bg_col}{Single colour for background. Default "gray97".}

\item{s}{Seed value. Default 1234.}
}
\value{
A ggplot object.
}
\description{
This function generates a generative art ggplot object using polygons.
}
