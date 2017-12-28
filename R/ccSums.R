#' @title \code{c}heck and \code{c}orrect row-wise sums
#' @param total \code{s}um \code{c}olumn to be compared with \link{base::rowSums}
#' @param ef The symbol used to fill empty rows.
#' @param fna fill NA's with 0 (default).
#' @param cols column number
#' @return  A list with:
#' \code{x} numeric data.frame or matrix with argument \code{x} corrected
#'  \code{total} a numeric vector with the totals corrected.
#'  \code{uT} a numeric table (data.frame or matrix) with \code{total} appended at the right of \code{x}
#' @export
#' @examples
#' flag <- 0
#' xe <- data.frame(a = 1:7, b = flag, c = flag)
#' xe[2, 1] <- flag
#' xe[3, 2] <- 3
#' xe[3, 3] <- 6
#' xe[4, 2:3] <- NA
#' xe[4, 1] <- 0
#' xe[5, 2] <- NA
#' xe[6, 1:3] <- NA
#' xe[7, 1] <- NA
#' print(xe)
#' ccSums(x = xe[, 1:2], total = xe[,3], ef = -99.009)

# IMPROVEMENTS:
# Let be the empty rows have a different label of "ef".
# let the rows with zeros conserve them

ccSums <- function(x, total, cols = NULL, ef = NA, fna = 0){

  xf <- x

  total[is.na(total)] <- 0

  ZinT <- is.element(0, total)
  if(ZinT)
    na <- which(total == 0)

  ifelse(is.null(cols),
         xn <- xf,
         xn <- xf[, cols])

  xn[is.na(xn)] <- fna
  Su <- rowSums(xn,  na.rm = TRUE)
  Su_TF <- Su == total

  total[!Su_TF] <- Su[!Su_TF]

  if(ZinT)
    total[na] <- Su[na]

  uT <- cbind(xn, total)
  TFr <- EmptyRow(x = uT, ef = 0)$TF
  uT[TFr,] <- ef
  xn[TFr,] <- ef
  total[TFr] <- ef

  if(!is.null(cols)){
    xf[, cols] <- xn
  } else
    xf <- xn

  results <- list(x = xf, total = total, uT = uT)

  return(results)
}
