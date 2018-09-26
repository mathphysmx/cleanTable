#' @title Check if any row of a table is empty
#' @details Check if any row of a table is empty
#' @param ...  \code{ef} arguments passed to \link{setEmptyCellsValue}.
#' @param x table (data.frame/2D-matrix) to be checked for completely empty rows.
#' @return a list with \code{TF}, a logical vector with TRUE value corresponding with complete empty rows, and \code{ID} giving the row number of the empty rows.
#' @export
#'
#' @seealso if in the example \code{flag = NA}
#'
#' x[ complete.cases(x),]
#'
#' x[!complete.cases(x),]
#' @examples
#' flag <- NA
#' xe <- data.frame(a = 3:5, b = flag, c = flag)
#' xe[2, 1] <- flag
#' xe[1, 2] <- 0
#' xe[1, 1] <- 0
#' xe[3, 2] <- 3
#' print(xe)
#' EmptyRow(x = xe, ef = c(flag, 0))

EmptyRow <- function(x, ef = c(NA, -999, 0)){

  xf <- x
  xf <- setEmptyCellsValue(xf, ef = ef, fw = NA)
  TF <- apply(is.na(x),1, all) # TF = TRUE/FALSE
  ID <- which(TF)
  if(prod(TF)) print("All rows are empty, i.e, it is an empty table")

  return(list(TF = TF, ID = ID))
}
