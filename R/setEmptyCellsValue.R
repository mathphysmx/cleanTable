#' @title Changes multiple invalid values for a unique invalid value
#' @param ef Empty Flag number,character, symbol ("", "(N/A)", NA, ...), etc. What is meant to be "empty". \code{flag} could be a vector but all of itS elements must be different of the posible valid values of \code{x}.
#' @param x Vector with multiple empty symbols.
#' @param fw The unique number,character, symbol (NA, Inf), etc. which represents empty cells. What is the symbol meaning empty cell?. Default to \code{NA}
#' @export
#' @examples
#' xe <- c(-999, 1, 0, NA)
#' print(xe)
#' setEmptyCellsValue(xe)
#' setEmptyCellsValue(xe, fw = 1000)
#' # example 2
#' xe <- data.frame(a = 3:6, b = 0, c = 0, d = 0, e = 0)
#' xe[2, 1] <- 0
#' xe[1, 4] <- 3
#' xe[2, 3] <- 3
#' xe[3, 3] <- 3
#' xe[3, 5] <- 3
#' xe[4, 2] <- 3
#' print(xe)
#' setEmptyCellsValue(x = xe, ef = 0)

# IMPROVEMENTS make characters, NA, numerics, and other \code{typeof} being possible as \code{ef}
# IMPROVEMENTS:
# accept different object classes as values of \code{flag}.
# For example flag <- list(a = 0, b = NA, c = "")

setEmptyCellsValue <- function(x, ef = c(NA, -999, 0), fw = NULL){

  xf <- x

  for(i in 1:length(ef)){
    xf[xf ==  ef[i]] <- NA
  }
  if(!is.null(fw))
    xf[is.na(xf)] <- fw

  return(xf)
}
