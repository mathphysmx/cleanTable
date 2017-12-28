#' @title Multiple choice questions check.
#'
#' @param x table (data.frame/matrix) from data comes.
#' @param opts options
#' @param fw fillwith
#' @param fc fill with to check visually
#' @details All \code{x}, \code{opts}, \code{fw} and \code{fc} must be the same class
#' @return
#' \code{x} with elements different of \code{opts} or blanks (NA) filled with \code{fc}, and \code{NA}'s are filled with \code{fw}
#' @export
#'
#' @examples
#' xe <- c(3, "o.9", 8)
#' mcq(xe, 1:3)
#'
#' xe <- c(3, 0, 8, NA, 8)
#' mcqMODE(x = xe, opts = 0:3, fw = -99.009)

mcqMODE <- function(x, opts,
                    fill = NULL, args = NULL, # ... pass to do.call()
                # fc = -1111,
                fw = NA){

  xf <- x

  if(is.numeric(xf) != is.numeric(opts))
    stop("xf and opts are not both of class numeric")
  # stop("xf and opts are not the same class")

  if(fill){
    do.call(what = fill, args = args)
    # do.call(statmode, args = list(x = rpois(30,12.3)))
  }

  # TF <- !is.element(el = xf, set = c(opts, NA))
  # xf[TF] <- fc
  xf[is.na(xf)] <- fw

  return(xf)
}
