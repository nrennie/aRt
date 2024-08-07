% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network.R
\name{network}
\alias{network}
\title{Network}
\usage{
network(
  n_x = 7,
  n_y = 7,
  prop = 0.3,
  col_palette = c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6"),
  bg_col = "white",
  bg_line_col = "grey70",
  line_col = "black",
  s = 1234
)
}
\arguments{
\item{n_x}{Number of columns. Default 7.}

\item{n_y}{Number of rows. Default 7.}

\item{prop}{Proportion of squares to be nodes. Default 0.3.}

\item{col_palette}{Colour palette. Default \code{c("#E01A4F", "#F15946", "#F9C22E", "#53B3CB", "#7DCFB6")}.}

\item{bg_col}{Background colour. Default "white".}

\item{bg_line_col}{Background line colour. Default "grey70".}

\item{line_col}{Line colour. Default "black".}

\item{s}{Seed value. Default 1234.}
}
\value{
A ggplot object.
}
\description{
This function generates a generative art ggplot object using a minimum
spanning tree.
}
