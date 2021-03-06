% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pase1.R
\name{pase1}
\alias{pase1}
\title{Verify a multiple choice question with optional answer. See question 2.1 of comercio.xlsx}
\usage{
pase1(x, opts, skipFlag, ec = NA, fw = -99.009, fc = -1111, col = 1,
  kc = FALSE, ...)
}
\arguments{
\item{x}{table (data.frame/matrix) from data comes.}

\item{opts}{Posible valid, non-empty values for the column \code{col} to be analyzed.}

\item{ec}{filling symbol for other cases as "doesn't apply" or empty}

\item{col}{Integer. Column number (after the \code{opts} column) to be analized.}

\item{kc}{keep content in column 2 in case the opts (1st column) cell has a valid \code{opts}.}

\item{...}{Further arguments passed to \link{setEmptyCellsValue}}

\item{shiftFlag}{The \code{opts} which means skip.}
}
\examples{
xe <- matrix(1:10, nrow = 5, ncol = 2)
xe[1, 1] <- NA
xe[3, 1] <- 2
xe[3, 2] <- NA
print(xe)
pase1(x = xe, opts = 1:5, skipFlag = 2,
     col = 1, ec = NA, kc = FALSE,
     fw = -99.009, ef = 4)
}
