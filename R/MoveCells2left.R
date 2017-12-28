#' @title Move filled cells to the leftmost empty cells.
#' @param df table (data.frame/2D-matrix) to be checked
#' @param ... \code{ef} arguments passed to \link{setEmptyCellsValue}
#' @param fillwith what is the character/number to fill the "empty" cells. Default the same as \code{flag}
#' @return a data.frame/2D-matrix
#' @export
#' @references http://stackoverflow.com/questions/23285215/advanced-dataframe-manipulation-in-r-shifting-cells-to-the-left
#' @examples
#' # example 1
#' xe <- data.frame(x=c("l","m",NA,NA,"p"),
#'                  y=c(NA,"b","c",NA,NA),
#'                  z=c("u",NA,"w","x","y"))
#' print(xe)
#' MoveCells2left(df = xe, ef = NA)
#
#' # example 2
#' xe <- data.frame(a = 3:6, b = 0, c = 0, d = 0, e = 0)
#' xe[2, 1] <- 0
#' xe[1, 4] <- 3
#' xe[2, 3] <- 3
#' xe[3, 3] <- 3
#' xe[3, 5] <- 3
#' xe[4, 2] <- 3
#' print(xe)
#' MoveCells2left(df = xe, ef = 0)

MoveCells2left <- function(df,
                           fillwith = NA,
                           shift = 1, ef = c(NA, -999, 0)){

  df <- setEmptyCellsValue(x = df, fw = NA, ef = ef)

  df2 <- as.data.frame(t(apply(df, 1,
                              function(x){
                                return(c(x[!is.na(x)], x[is.na(x)]))
                                }
  )))
  colnames(df2) = colnames(df)

  df2[is.na(df2)] <- fillwith

  return(df2)
}
