% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bb_plotBase.R
\name{bb_plotBase}
\alias{bb_plotBase}
\title{adds a plot created with baseR to a BentoBox layout}
\usage{
bb_plotBase(
  plot,
  x,
  y,
  width,
  height,
  params = NULL,
  just = c("left", "top"),
  default.units = "inches"
)
}
\arguments{
\item{plot}{plot formula of base R plotting functions; ex: p <- ~plot(1:10) + abline(v = 2)}

\item{x}{A numeric or unit object specifying x-location.}

\item{y}{A numeric or unit object specifying y-location.}

\item{width}{A numeric or unit object specifying width.}

\item{height}{A numeric or unit object specifying height.}

\item{params}{an optional "bb_params" object space containing relevant function parameters}

\item{just}{A string or numeric vector specifying the justification of the plot relative to its (x, y) location}

\item{default.units}{A string indicating the default units to use if x, y, width, or height are only given as numeric vectors}
}
\description{
adds a plot created with baseR to a BentoBox layout
}
