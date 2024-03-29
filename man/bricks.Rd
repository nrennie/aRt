% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bricks.R
\name{bricks}
\alias{bricks}
\title{Bricks}
\usage{
bricks(
  n_y = 20,
  colours = c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C"),
  bg_col = "gray97",
  s = 1234
)
}
\arguments{
\item{n_y}{Number of rows. Default 20.}

\item{colours}{Vector of colours. Can be any length. Default c("#9B1D20", "#3D2B3D", "#CBEFB6", "#635D5C").}

\item{bg_col}{Background colour. Default "gray97".}

\item{s}{Seed value. Default 1234.}
}
\value{
A ggplot object.
}
\description{
This function generates a coloured generative art ggplot object using polygons.
}
