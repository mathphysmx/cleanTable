#' @title Does the job in question 11 of Comercio.xlsx
#' @param x data.frame or matrix with data. The last column is assumed to be the total column.
#' @param sc Character. start column name.
#' @param ec Character. End column name. This is the column of totals.
#' @param col_fw corresponding column to add values with in case the computed sum is less than given by \code{x}. Default to \code{1}.
#' @param cols columns to be used to check the sum. It overrides \code{sc} and \code{ec}
#' @export
#' @examples
#' q11_1(x = df1, sc = "P11_1_1", ec = "P11_1_6")
#' cpos <- getPosIndex(cnames = names(df1),
#'                     sc = "P11_1_1", ec = "P11_1_6")
#' q11_1(x = df1, cols = cpos)

# IMPROVEMENTS: set col_fw default as the column more frequent (statistical mode)
# IMPROVEMENTS: Check for outliers
q11_1 <- function(x, sc, ec, fw = 0, col_fw = 1,
                  cols = NULL){

  xf <- x

  if(!is.null(cols)){
    cpos <- cols
  } else {
    cpos <- getPosIndex(cnames = names(xf),
                        sc = sc,
                        ec = ec)
  }
  xfr <- checkClass(x = x[, cpos])
  xfr <- setEmptyCellsValue(x = xfr,
                                  ef = NA, fw = fw)

  # totalsCol <- tail(cpos, 1)
  totalsCol <- ncol(xfr)
  xTemp <- ccSums(x = xfr[, -totalsCol],
                  total = xfr[, totalsCol],
                  ef = 0)
  tc <- TotalCompare(xfr[, totalsCol], xTemp$total)
  if(length(tc$l)){
    xfr[, totalsCol][tc$l] <- xTemp$total[tc$l]}
  if(length(tc$g)){
    DifTot <- xfr[, totalsCol][tc$g] - xTemp$total[tc$g]
    xfr[, col_fw][tc$g] <- xfr[, col_fw][tc$g] + DifTot}

  return(xfr)
}
