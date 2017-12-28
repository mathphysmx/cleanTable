#' @title Does the job in question 15 in comercio.xlsx
#' @param x data.frame or matrix with data. The last column is assumed to be the total column.
#' @param col_fw corresponding column to add values with in case the computed sum is less than given by \code{x}. Default to \code{1}.
#' @param cols columns to be used to check the sum. It overrides \code{sc} and \code{ec}
#' @param ... further arguments passed to \code{q11_1}
#' @export
#' @examples
#' cpos <- getPosIndex(cnames = names(df1),
#'                     sc = "P15_1_1_1",
#'                     ec = "P15_1_5_2") # this (ec) is the column of "total"
#' x[, cpos] <- q15_1(x = df1, cPos = cpos,
#'                    nr = 5, nc = 2)

# IMPROVEMENTS: use \code{sc} and \code{ec}
# q15_1(x = df1, sc = "P15_1_1_1", ec = "P15_1_5_2",
#       nr = 5, nc = 2)

q15_1 <- function(x, sc, ec, nr = 1, nc = 1,
                  cPos, ...){

  xf <- x[, cPos]

  # cPos <- getPosIndex(cnames = names(xf), sc = sc, ec = ec)
  for(i in 1:nc){
    # i <- 1
    colID <- seq.int(from = i, to = length(cPos), by = nc)
    xf[, colID] <- q11_1(x = xf, cols = colID, ...)
  }

  return(xf)
}
