#' @title Look for the index in an R object
#' @param cnames \code{c}olumn \code{names} of the entire table
#' @param sc names of the \code{s}tarting \code{c}olumn of the sub-table
#' @param ec names of the \code{e}nd \code{c}olumn column of the sub-table
#' @return An integer for the position (column number in case of data.frame or matrix) of a subset indicated by (column, row, etc.) names
#' @export
#' @examples
#' names(iris)
#' getPosIndex(cnames = names(iris),
#'                       sc = "Sepal.Width", ec = "Petal.Width")
#' getPosIndex(cnames = names(iris),
#'                       sc = "Sepal.Width")

getPosIndex <- function(cnames, sc, ec = NULL){
  sID <- which(cnames == sc)
  ifelse(is.null(ec),
         eID <- sID,
         eID <- which(cnames == ec)
  )
  nID <- sID:eID
  return(nID)
}
