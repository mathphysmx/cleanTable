#' @title comparison between two vectors
#' @param x,y the two vectors to be compared entry-by-entry.
#' @return a list with
#' \describe{
#'   \item{g}{which \code{x} values are greater than \code{y}}
#'   \item{l}{which \code{x} values are lower   than \code{y}}
#' }
#' @export
#' @examples
#' xe <- c(1, 3, 6)
#' ye <- c(1, 2, 7)
#' TotalCompare(xe, ye)

TotalCompare <- function(x, y){
  return(x = list(g = which(x > y), l = which(x < y)))
}
