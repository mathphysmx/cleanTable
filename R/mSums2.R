#' @title Check and correct Multiple sums
#' @param x Table to be checked. data not need to be contigous but must be equally spaced.
#' @param sc,scT names of the \code{s}tarting \code{c}olumn of the table \code{x} for data to be checked \code{sc} and for the totals, \code{scT}
#' @param ec,scT names of the \code{e}nd \code{c}olumn column of the table \code{x} for data to be checked \code{sc} and for the totals, \code{scT}
#' @param nr Block length
#' @param nc number of blocks. Default to 1, i.e., a the totals must be a vector.
#' @param ... further arguments passed to \link{ccSums}
#' @export
#' @return A list with
#' \describe{
#'   \item{x}{input \code{x} but data checked}
#'   \item{cPos}{column numbers representing \code{sc} and \code{ec} of data \code{x}}
#'   \item{cPosT}{column numbers representing \code{sc} and \code{ec} of the totals}

#' }
#' @details \code{total} columns must be contiguous, and data blocks too.
#' @examples
#' xe <- as.data.frame(matrix(c(-1:-5, 33301:33305,
#'                              -1:-5, 33301:33305,
#'                              1:10), # totals
#'                            nrow = 5, ncol = 6))
#' flag <- NA
#' xe[1, 1] <- flag
#' xe[2, 1] <- flag
#' xe[2, 3] <- flag
# # xe[2, 5] <- flag
#' print(xe)
#' mSums2(xe, sc = "V1", ec = "V4",
#'       scT = "V5", ecT = "V6",
#'       nr = 2, nc = 2, ef = -99.009)

# xls_P8 <- mSums2(x = df1,
#                 sc = "P8_1_1_1", ec = "P8_1_4_4",
#                 scT = "P8_1_T_1", ecT = "P8_1_T_4",
#                 nr = 4, nc = 4) # nr = 4, means 4 questions (8_1_1:8_1_4)
# x[, c(xls_P8$cpos, xls_P8$cposT)] <- xls_P8$x[, c(xls_P8$cpos, xls_P8$cposT)]
# IMPROVEMENTS: Accept excel column name "A,...,Z,AA, AB,..." or column numbers
# change bl by nr (number of rows in the question), nb by nc (number of columns in the question)

mSums2 <- function(x, sc, ec, scT, ecT, nr = 1, nc = 1,...){

  xf <- x
  #   sc <-  # 1st (leftmost) column name
  #   ec <-  # End column name
  #   nr <- 4 # block length. skip
  cPos <- getPosIndex(cnames = names(xf), sc = sc, ec = ec)

  # TOTAL(S)
  #   scT <-  # 1st (leftmost) column name
  #   ecT <-  # End column name
  cPosT <- getPosIndex(cnames = names(xf), sc = scT, ec = ecT)

  #   if(ncol(xf) != length(c(cPos, cPosT)))
  #     stop("number of columns of x is different from totals and data")

  xf[, cPosT] <- setEmptyCellsValue(x = xf[, cPosT], ef = NA, fw = 0)

  for(i in 1:nc){
    # i <- 1
    colID <- seq.int(from = cPos[i], to = tail(cPos, 1), by = nc)

    xn <-  ccSums(x = xf[, colID], total = xf[, cPosT[i]], ...)

    tc <- TotalCompare(xf[, cPosT[i]], xn$total)
    if(length(tc$l)){
      xf[, cPosT[i]][tc$l] <- xn$total[tc$l]
    }
    xf[, colID]    <- xn$x
  }

  res <- list(x = xf, cPos = cPos, cPosT = cPosT)

  return(res)
}
