% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{mdy2mmyyyy}
\alias{mdy2mmyyyy}
\title{convert m/d/yy to mm/dd/yyyy}
\usage{
mdy2mmyyyy(x)
}
\arguments{
\item{x}{character date to be formatted}
}
\description{
Pads dates in preparation for POSIX coercion
}
\examples{
x <- "5/5/15"
mdy2mmyyyy(x)
}

