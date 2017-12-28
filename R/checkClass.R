#' @title look for non-numeric columns and correct them to numeric
#' @details look for non-numeric columns and correct them to numeric by the function \link{rmNonNumeric}
#' @param x A data.frame (not matrix).
#' @param cols specific columns of \code{x}. If \code{NULL} (default), the complete \code{x} is class-checked.
#' @return a numeric vector, data.frame or matrix with
#' @export
#'
#' @examples
#' checkClass(head(iris))
#' print(head(iris))

checkClass <- function(x, cols = NULL){

  xf <- x

  ifelse(is.null(cols),
         xn <- xf,
         xn <- xf[, cols])

  xC <- sapply(xn, class) # classes of the columns of xf
  print(xC)
  TF <- xC == "numeric"
  cC <- which(TF == FALSE)

  if(length(cC) != 0){
    for(i in 1:length(cC)){
      # i <- 1
      xn[, cC[i]] <- rmNonNumeric(xn[, cC[i]])
    }
  }

  ifelse(test = is.null(cols),
         xf <- xn, xf[, cols] <- xn)

  return(xf)
}
