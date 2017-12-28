#' @title Check if all the answers are according to the optios available
#' @param x A matrix of data.frame to be checked if it contains any of \code{opts} in \code{mcq}.
#' @param dupCheck if \code{TRUE} (default), remove (set to \code{NA}) duplicated value row-wisely.
#' @param ... further parameters passed to \link{mcq}
#' @return corrected vector
#' @export
#' @details The name stands for 'multiple answer questions'
#' @examples
#' xe <- data.frame(x = c(1:4, 4), y = c(2:5,4))
#' print(xe)
#' maq(x = xe, opts = 3:4)
#' maq(x = xe, opts = 3:4, fw = NA, inv_fw = NULL)
#' maq(x = xe, opts = 3:4, fw = NA, inv_fw = NA)

maq <- function(x, dupCheck = TRUE, ...){

  xf <- x

  ncols <- ncol(xf)
  for(i in 1:ncols){
    xf[, i] <- mcq(x = xf[, i], ...)
  }

  if(dupCheck){
    for(i in 1:nrow(xf)){ # IMPROVEMENTS use s/apply() instead of this loop
      # i <- 5
      xf[i, duplicated(unlist(xf[i, ]))] <- NA
    }
  }

  return(xf)
}
