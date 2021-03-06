% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccSums.R
\name{ccSums}
\alias{ccSums}
\title{\code{c}heck and \code{c}orrect row-wise sums}
\usage{
ccSums(x, total, cols = NULL, ef = NA, fna = 0)
}
\arguments{
\item{total}{\code{s}um \code{c}olumn to be compared with \link{base::rowSums}}

\item{cols}{column number}

\item{ef}{The symbol used to fill empty rows.}

\item{fna}{fill NA's with 0 (default).}
}
\value{
A list with:
\code{x} numeric data.frame or matrix with argument \code{x} corrected
 \code{total} a numeric vector with the totals corrected.
 \code{uT} a numeric table (data.frame or matrix) with \code{total} appended at the right of \code{x}
}
\examples{
flag <- 0
xe <- data.frame(a = 1:7, b = flag, c = flag)
xe[2, 1] <- flag
xe[3, 2] <- 3
xe[3, 3] <- 6
xe[4, 2:3] <- NA
xe[4, 1] <- 0
xe[5, 2] <- NA
xe[6, 1:3] <- NA
xe[7, 1] <- NA
print(xe)
ccSums(x = xe[, 1:2], total = xe[,3], ef = -99.009)
}
