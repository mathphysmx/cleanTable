% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pase.R
\name{pase}
\alias{pase}
\title{Verify a multiple choice question with optional answer. See question 6.3 of comercio.xlsx}
\usage{
pase(x, opts, skipFlag, ec = NA, fw = -99.009, fc = -1111, col = 1,
  kc = FALSE, skipTo = 1, ...)
}
\arguments{
\item{x}{table (data.frame/matrix) from data comes.}

\item{opts}{Posible valid, non-empty values for the column \code{col} to be analyzed.}

\item{col}{Column number to be analized.}

\item{kc}{keep coments in the options cell}

\item{skipTo}{Number of lines to skip after \code{col} (non-inclusive).}

\item{...}{Further arguments passed to \link{setEmptyCellsValue}}

\item{shiftFlag}{The \code{opts} which means skip.}
}
\value{
corrected \code{x}
}
\examples{
xe <- matrix(1:20, nrow = 5, ncol = 4)
xe[1, 1] <- NA
xe[2, 2] <- NA
xe[2, 3] <- NA
xe[3, 2] <- NA
print(xe)
pase(x = xe, opts = 1:5, skipFlag = 4,
     col = 1, skipTo = 3, kc = FALSE,
     fw = -999, ef = 2)
}
