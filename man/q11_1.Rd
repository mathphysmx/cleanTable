% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/q11_1.R
\name{q11_1}
\alias{q11_1}
\title{Does the job in question 11 of Comercio.xlsx}
\usage{
q11_1(x, sc, ec, fw = 0, col_fw = 1, cols = NULL)
}
\arguments{
\item{x}{data.frame or matrix with data. The last column is assumed to be the total column.}

\item{sc}{Character. start column name.}

\item{ec}{Character. End column name. This is the column of totals.}

\item{col_fw}{corresponding column to add values with in case the computed sum is less than given by \code{x}. Default to \code{1}.}

\item{cols}{columns to be used to check the sum. It overrides \code{sc} and \code{ec}}
}
\examples{
q11_1(x = df1, sc = "P11_1_1", ec = "P11_1_6")
cpos <- getPosIndex(cnames = names(df1),
                    sc = "P11_1_1", ec = "P11_1_6")
q11_1(x = df1, cols = cpos)
}
