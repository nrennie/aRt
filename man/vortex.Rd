% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vortex.R
\name{vortex}
\alias{vortex}
\title{Vortex}
\usage{
vortex(n = 25, start_val = 90, col_scheme = "mono", bg_col = "black", s = 1234)
}
\arguments{
\item{n}{Number of points. Default 25.}

\item{start_val}{Starting position for polar coordinates. Default 90.}

\item{col_scheme}{Colour scheme of art. One of c("mono", "rainbow). Default 0.1.}

\item{bg_col}{Background colour. Default "black".}

\item{s}{Seed value. Default 1234.}
}
\value{
A ggplot object.
}
\description{
This function generates a greyscale or rainbow coloured generative art ggplot object shaped like a vortex.
}
