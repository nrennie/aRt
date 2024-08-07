% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stacked.R
\name{stacked}
\alias{stacked}
\title{Stacked}
\usage{
stacked(
  n_x = 5,
  n_y = 5,
  col_palette = MetBrewer::met.brewer("Hiroshige", 9),
  rayshade = FALSE,
  shadow_intensity = 0.5,
  sunangle = 315,
  s = 1234
)
}
\arguments{
\item{n_x}{Number of polygons per row. Default 5.}

\item{n_y}{Number of polygons per column. Default 5.}

\item{col_palette}{Vector of colours. Default Hiroshige palette from MetBrewer.}

\item{rayshade}{Boolean determining whether the returned plot should be converted to
three dimensional using rayshader. If \code{TRUE}, \code{{rayshader}} is required to be installed.
Default \code{FALSE}.}

\item{shadow_intensity}{Intensity of shading for 3D elements, Default 0.5.}

\item{sunangle}{Angle of the sun. Default 315.}

\item{s}{Seed value. Default 1234.}
}
\value{
A ggplot object.
}
\description{
This function generates a coloured generative art ggplot object using 3D square polygons.
}
