% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statmodeTable.R
\name{statmodeTable}
\alias{statmodeTable}
\title{computes the mode of data}
\usage{
statmodeTable(x)
}
\arguments{
\item{x}{a matrix or data.frame}
}
\value{
a 2-length vector with the mode value at the first entry and frequency at the second entry.
}
\examples{
xe <- rpois(30,12.3)
xe <- matrix(xe, nrow = 5, ncol = 6)
statmodeTable(x = xe)
}
