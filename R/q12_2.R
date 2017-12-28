#' @title Verify question 12.2 of comercio.xlsx
#' @param x data.frame or matrix
#' @param skipFlag what to skip
#' @param opts valid options
#' @export
#' @examples
#' cpos <- getPosIndex(cnames = names(df1),
#'                     sc = "P12_2_1", ec = "P12_2_0")
#' q12_2(x = df1[, cpos], skipFlag = 6, opts = 1:6)


q12_2 <- function(x, skipFlag, opts){

  xf <- x

  vals <- 1:(ncol(xf)-1)
  opt  <-    ncol(xf)
  xf[, vals] <- checkClass(x = x[, vals])
  xf[, vals] <- maq(x = xf[, vals], dupCheck = TRUE,
                    opts = opts,
                    fw = NA, inv_fw = NA)
  xf[, vals] <- MoveCells2left(df = xf[, vals],
                              fillwith = NA, ef = c(NA,0))
  fc <- sort(unique(which(skipFlag == xf[, vals], arr.ind = T)[,1]))
  # Set to \code{NA} the corresponding elements in 12.2_4 which have a value different from \code{skipFlag}.
  if(length(fc)){
    opA <- as.logical(rowSums((skipFlag == xf[, vals]), na.rm = TRUE))
    xf[!opA, opt] <- NA
  }

  return(xf)
}
