#' @title computes the mode of data
#' @details from http://stats.stackexchange.com/questions/157661/how-to-calculate-mean-median-mode-std-dev-from-distribution
#' @param x cathegorical data.
#' @return mode of x.
#' @export
#' @examples
#' xe <- rpois(30,12.3)
#' statmode(x = xe)
#' names(statmode(x = xe))

statmode <- function(x){

  xf <- x

  tail(sort(table(xf)),1)   #1: category and count; if multimodal this only gives one

  w <- table(xf)
  w[max(w)==w] #2: category and count; this can find more than one mode

  # which.max(table(x)) #3: category and *position in table*; only finds one mode

  return(w[max(w)==w])
}


