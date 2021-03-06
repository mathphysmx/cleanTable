% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maq.R
\name{maq}
\alias{maq}
\title{Check if all the answers are according to the optios available}
\usage{
maq(x, dupCheck = TRUE, ...)
}
\arguments{
\item{x}{A matrix of data.frame to be checked if it contains any of \code{opts} in \code{mcq}.}

\item{dupCheck}{if \code{TRUE} (default), remove (set to \code{NA}) duplicated value row-wisely.}

\item{...}{further parameters passed to \link{mcq}}
}
\value{
corrected vector
}
\details{
The name stands for 'multiple answer questions'
}
\examples{
xe <- data.frame(x = c(1:4, 4), y = c(2:5,4))
print(xe)
maq(x = xe, opts = 3:4)
maq(x = xe, opts = 3:4, fw = NA, inv_fw = NULL)
maq(x = xe, opts = 3:4, fw = NA, inv_fw = NA)
}
