% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smudge.R
\name{smudge}
\alias{smudge}
\title{Smudge}
\usage{
smudge(
  n = 25,
  binwidth = 0.01,
  col_palette = PrettyCols::prettycols("TangerineBlues"),
  s = 1234
)
}
\arguments{
\item{n}{Number of grid boxes. Default 25.}

\item{binwidth}{Binwidth for colours. Default 0.01.}

\item{col_palette}{Vector of colours. Default
\code{PrettyCols::prettycols("TangerineBlues")}.}

\item{s}{Seed value. Default 1234.}
}
\value{
A ggplot object.
}
\description{
This function generates a coloured generative art ggplot object from
contours.
}
