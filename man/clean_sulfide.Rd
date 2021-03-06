% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab.R
\name{clean_sulfide}
\alias{clean_sulfide}
\title{Clean sulfide data}
\usage{
clean_sulfide(sulfpath = file.path("Raw", "lab"), sheet_nums = NA)
}
\arguments{
\item{sulfpath}{character file path to an .xlsx file}

\item{sheet_nums}{numeric sheet indices containing raw data}
}
\description{
Clean sulfide data
}
\examples{
\dontrun{
sulfide <- clean_sulfide(sulfpath = file.path("Raw", "lab"))$fielddt
}
}

