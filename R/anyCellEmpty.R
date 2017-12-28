#' @title Check if any cell of a vector is empty
#' @details See \link{stats::complete.cases} for data.frames/matrix. incompleteBlocks <- (1:nrow(x))[!complete.cases(x)]
#' @param flag What is meant to be "empty". \code{flag} could be a vector but all of it elements must be different of the posible valid values of \code{x}.
#' @param x table (data.frame/2D-matrix) to be checked
#' @export
#' @seealso if in the example
#' x[ complete.cases(x),]
#' x[!complete.cases(x),]
#' and the function EmptyRow.R
#' @examples
#' xe <- c(3,3,3, 0, -999, NA)
#' anyCellEmpty(xe, flag = c(0, NA, -999))

#' xe <- 1:5
#' anyCellEmpty(xe)

# IMPROVEMENTS:
# accept different object classes as values of \code{flag}.
# For example flag <- list(a = 0, b = NA, c = "")

anyCellEmpty <- function(x, flag = c(0, NA, -999)){
  # is the cell/block "empty"?
  if(sum(match(x = flag, table = x,  nomatch = FALSE)))
  {
    filled <- TRUE
  } else {
    filled <- FALSE
  }
  return(filled)
}
