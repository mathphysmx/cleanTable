#' @title Multiple choice questions (mcq) check.
#'
#' @param x table (data.frame/matrix) from data comes.
#' @param opts options
#' @param fw fillwith. If \code{inv_fw = NA} it will be overwriten by \code{fw}.
#' @param fc fill with to check visually
#' @param inv_fw change the invalid options with \code{inv_fw}. Default to \code{NA}. If \code{NULL} it keeps the original values.
#' @return  \code{x} with elements different of \code{opts} or blanks (NA) filled with \code{fc}, and \code{NA}'s are filled with \code{fw}
#' @details All \code{x}, \code{opts}, \code{fw} and \code{fc} must be the same class
#' @export
#'
#' @examples
#' xe <- c(3, "o.9", 8)
#' mcq(xe, 1:3)
#'
#' xe <- c(3, 0, 8, NA)
#' mcq(x = xe, opts = 0:3, fw = -99.009)
#' mcq(x = xe, opts = 0:3, fw = NA, inv_fw = NULL)
#' mcq(x = xe, opts = 0:3)
mcq <- function(x, opts,
                inv_fw = NA,
                fw = NA){

  xf <- x

  if(is.numeric(xf) != is.numeric(opts))
    stop("xf and opts are not both of class numeric")
  # stop("xf and opts are not the same class")

  TF <- !is.element(el = xf, set = c(opts, NA))
  if(!is.null(inv_fw)){
    xf[TF] <- inv_fw
  }

  xf[is.na(xf)] <- fw

  return(xf)
}
