#' @title Verify a multiple choice question with optional answer. See question 2.1 of comercio.xlsx
#' @param x table (data.frame/matrix) from data comes.
#' @param opts Posible valid, non-empty values for the column \code{col} to be analyzed.
#' @param shiftFlag The \code{opts} which means skip.
#' @param col Integer. Column number (after the \code{opts} column) to be analized.
#' @param ec filling symbol for other cases as "doesn't apply" or empty
#' @param kc keep content in column 2 in case the opts (1st column) cell has a valid \code{opts}.
#' @param ... Further arguments passed to \link{setEmptyCellsValue}
#' @export
#' @examples
#' xe <- matrix(1:10, nrow = 5, ncol = 2)
#' xe[1, 1] <- NA
#' xe[3, 1] <- 2
#' xe[3, 2] <- NA
#' print(xe)
#' pase1(x = xe, opts = 1:5, skipFlag = 2,
#'      col = 1, ec = NA, kc = FALSE,
#'      fw = -99.009, ef = 4)
# IMPROVEMENTS: consider values not in opts in the first column.
pase1 <- function(x, opts, skipFlag,
                  ec = NA, # filling symbol for other cases as "doesn't apply" or empty
                fw = -99.009, # cell whose data is not available
                fc = -1111, # cell needed to be checked
                 col = 1, kc = FALSE,
                 # skipTo = 1,
                 ...){

  xf <- x

  if(!is.element(el = skipFlag, set = opts)){
    stop("skipFlag is not valid option")
  }

#   if(skipTo > nrow(xf)){
#     stop("skip length must be not greater than the number columns")
#   }

  xf <- setEmptyCellsValue(xf, fw = NA,...)

  TF <- is.element(el = xf[, col],
                   set = setdiff(x = opts, y = skipFlag))
  if(!kc){
    xf[TF, col + 1] <- ec
  }

  sk <- which(xf[, col] == skipFlag)
  xf[sk, col + 1][is.na(xf[sk, col + 1])] <- fw

  TFb <- is.na(xf[, col])
  xf[TFb, col    ][is.na(xf[TFb, col + 1])] <- fw
  # xf[TFb, col + 1][is.na(xf[TFb, col + 1])] <- ec

    return(xf)
}
