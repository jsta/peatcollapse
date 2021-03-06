% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab.R
\name{create_labdb}
\alias{create_labdb}
\title{Create sqlite database of peatcollapse lab data}
\usage{
create_labdb(eddpath = file.path("Raw", "lab", "EDD"),
  dbname = "pc_eddlab.db", tablename = "eddlab")
}
\arguments{
\item{eddpath}{character file.path to edd data folder}

\item{dbname}{character file name of output SQLite database}

\item{tablename}{character name of internal database table}
}
\description{
Create sqlite database of peatcollapse lab data
}
\examples{
\dontrun{
create_labdb()
}
}

