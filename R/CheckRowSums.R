#' @title Checks that evey row sums \code{total}
#' @param x data.frame or matrix
#' @param total The true total value.
#' @param cols integer vector with the columns of \code{x} to be checked.
#' @return a logical vector with \code{TRUE} in the rows wich are equal to total
#' @export
#' @examples
#' CheckRowSums(iris[1:5,1:3], total = 10)
#' CheckRowSums(iris[1:5,1:3], total = 10, cols = c(1,2))
#' CheckRowSums(iris[1:5,1:3], total = rowSums(iris[1:5,1:3]))

CheckRowSums <- function(x, total = 100, cols = NULL){

  xf <- x

  if(!is.null(cols)){
    xf <- xf[, cols]
  }

  Su <- rowSums(xf,  na.rm = TRUE)
  Su_TF <- Su == total
  return(Su_TF)
}
