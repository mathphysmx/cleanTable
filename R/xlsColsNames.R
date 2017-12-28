#' @title Generates a vector with the characters of excel spreadsheets (A, B, ..., Z, AA, AB,...).
#'
#' @param n The number of columns of the spreadsheet.
#' @param all Logical. If \code{length(n) == 1}.
#' @return a character vector of length \code{n} with the excel column index
#' @details Generates a vector with the (english) characters of excel spreadsheets (A, B, ..., Z, AA, AB,...).
#' @export
#'
#' @examples
#' xlsColsNames(n = 4)
#' xlsColsNames(n = c(2,5))
#' xlsColsNames(n = 60)
# IMPROVEMENTS:  @param all
xlsColsNames <- function(n, all = FALSE){

  nf <- n

  nlength <- length(n)
  if(nlength != 1){
    nf <- max(n)
  }

  as <- floor(nf/26)
  ID <- vector(mode = "character", length = nf)
  ID[1:26] <- LETTERS
  if(as){
    for(i in 1:as){
      ID[ (i*26 + 1) :  ((i+1) * 26)] <- paste(LETTERS[i], LETTERS, sep = "")
    }
  }
  ID <- ID[1:nf]

  if(nlength != 1){
    ID <- ID[n]
  }

  return(ID)
}
