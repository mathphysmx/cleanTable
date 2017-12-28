#' @title computes the mode of data
#' @param x a matrix or data.frame
#' @return a 2-length vector with the mode value at the first entry and frequency at the second entry.
#' @export
#' @examples
#' xe <- rpois(30,12.3)
#' xe <- matrix(xe, nrow = 5, ncol = 6)
#' statmodeTable(x = xe)

statmodeTable <- function(x){

  xf <- x
  library(gdata)
  dat <- na.omit(unmatrix(as.matrix(xf)))

  library(plyr)
  fT <- count(dat)
  maxV <- which.max(fT[,2])

  return(fT[maxV,])
}


