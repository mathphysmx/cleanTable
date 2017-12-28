#' @title count the number of empty cells in each row in order to check for large missing values in a row.
#'
#' @param x data.frame or matrix
#' @param ... further argumente passed to \link{setEmptyCellsValue}
#' @details count the number of empty cells in each row in order to check for large missing values in a row.
#' @export
#'
#' @examples
#' xe <- matrix(data = NA, nrow = 4, ncol = 3)
#' xe[1, ] <- 1
#' xe[2, 2:3] <- 2
#' xe[3,   3] <- 3
#' print(xe)
#' countEmptyCells(xe)
#'
#' xe <- matrix(data = -99, nrow = 4, ncol = 3)
#' xe[1, ] <- 1
#' xe[2, 2:3] <- 2
#' xe[3,   3] <- 3
#' print(xe)
#' countEmptyCells(xe, ef = -99)
# IMPROVEMENTS: make an image.plot() to show empty cells.

countEmptyCells <- function(x, ...){
  xf <- x

  xf <- setEmptyCellsValue(xf, ...)
  counts <- rowSums(is.na(xf))
  names(counts) <- 1:nrow(xf)
  barplot.default(counts, main = "", horiz = TRUE,
                  ylab = "Row number", xlab = "Empty cells")

  return(cbind(1:nrow(xf), counts))
}
