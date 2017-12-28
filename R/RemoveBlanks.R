#' @title cut Remove or delete Leading / Trailing Whitespace blanks of a matrix or data.frame with character columns
#' @param x data.frame or matrix
#' @export

RemoveBlanks <- function(x){

  xf <- x

  colClass <- sapply(xf, class)
  chcol <- as.numeric(which(colClass == "character", arr.ind = T))
  xf[, chcol] <- sapply(xf[, chcol], trimws)

  return(xf)
}
