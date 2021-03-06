% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcplot.R
\name{tsplot}
\alias{tsplot}
\title{Time-series plot}
\usage{
tsplot(dt, params, bwfw, pwsw, inout, inclegend = TRUE, tofile = FALSE,
  print_xaxis = TRUE, print_main = TRUE)
}
\arguments{
\item{dt}{data.frame}

\item{params}{character vector of column names}

\item{bwfw}{character choice if "fw" or "bw"}

\item{pwsw}{character choice of "pw" or "sw"}

\item{inout}{character choice of "in" or "out"}

\item{inclegend}{logical include legend?}

\item{tofile}{logical save plot to disk?}

\item{print_xaxis}{logical print xaxis?}

\item{print_main}{logical print main label?}
}
\description{
Time-series plot
}
\examples{
\dontrun{
cfieldall <- read.csv("/home/jose/Documents/Science/Data/peatcollapse/fieldallv9.csv",
stringsAsFactors = FALSE)
cfieldall$collect_date <- as.POSIXct(cfieldall$collect_date)
tsplot(cfieldall, params = names(cfieldall)[c(9,12,13,14,16,18,19,22)],
bwfw = "bw", pwsw = "pw", tofile = FALSE, inout = "in", inclegend = FALSE)
}
}

