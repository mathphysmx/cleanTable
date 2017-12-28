#' @title Verify a multiple choice question with optional answer. See question 6.3 of comercio.xlsx
#' @param x table (data.frame/matrix) from data comes.
#' @param opts Posible valid, non-empty values for the column \code{col} to be analyzed.
#' @param shiftFlag The \code{opts} which means skip.
#' @param col Column number to be analized.
#' @param kc keep coments in the options cell
#' @param skipTo Number of lines to skip after \code{col} (non-inclusive).
#' @param ... Further arguments passed to \link{setEmptyCellsValue}
#' @return corrected \code{x}
#' @export
#' @examples
#' xe <- matrix(1:20, nrow = 5, ncol = 4)
#' xe[1, 1] <- NA
#' xe[2, 2] <- NA
#' xe[2, 3] <- NA
#' xe[3, 2] <- NA
#' print(xe)
#' pase(x = xe, opts = 1:5, skipFlag = 4,
#'      col = 1, skipTo = 3, kc = FALSE,
#'      fw = -999, ef = 2)
# IMPROVEMENTS: add another row in col 1 with a value not in opts
# xe = rbind(xe, c(-7, NA, 13, 20))


pase <- function(x, opts, skipFlag,
                  ec = NA, # filling symbol for other cases as "doesn't apply" or empty
                  fw = -99.009, # cell whose data is not available
                  fc = -1111, # cell needed to be checked
                  col = 1, kc = FALSE,
                  skipTo = 1,
                  ...){

  xf <- x

  if(!is.element(el = skipFlag, set = opts)){
    stop("skipFlag is not valid option")
  }

  #   if(skipTo > ncol(xf)){
  #     stop("skip length must be not greater than the number columns")
  #   }

  xf <- setEmptyCellsValue(xf, fw = NA, ...)

  endc <- col + skipTo - 1 # end column

  TF1 <- is.na(xf[, col])
  TFb <- EmptyRow(xf[, (col + 1):endc], ...)$TF
  TFe <- is.element(el = xf[, col],
                   set = setdiff(x = opts, y = skipFlag))

  xf[TF1 & !TFb, col] <- fw

  xf[TF1 &  TFb, col] <- skipFlag

  TFf <- which(xf[, col] == skipFlag)
  xf[TFf, (col + 1):endc] <- ec

  return(xf)
}
