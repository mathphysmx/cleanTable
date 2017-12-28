#' @title remove non-numeric symbols. Commas, semicolons, white space between numbers.
#'
#' @param x a string vector
#' @param other list of extra symbols to be replaced with \code{fw}
#' @param fw list. Must be compatible with \code{other}
#'
#' @return a numeric vector or a error mesage if still non-numeric symbols remaining
#' @details remove non-numeric symbols as commas, semicolons, white space between numbers, and changes the letters "o" and "O" by zeros (0). String characters coerce them to NA's
#' @export
#'
#' @examples
#' xe <- c("2,000", "4 5    6", "300OOoo", "4.4", "abc", "ab1")
#' rmNonNumeric(xe)
#' rmNonNumeric(xe, other = list("oO", ","), fw = list(0, ""))

# IMPROVEMENTS
# xe <- c("20T", "3e+5")

rmNonNumeric <- function(x, other = list("oO", ","), fw = list(0, "")){

  xf <- x

  if(is.numeric(xf))
    print("x is already numeric")

  xf <- gsub(pattern = "[ \t\r\n\f\v]", # remove white spaces
             replacement = "",
             x = xf, perl = TRUE)

  if(!is.null(other)){
    for(i in 1:length(other)){
      # i <- 1
      xf <- gsub(pattern = paste("[", other[[i]], "]", sep = ""), # replaces lower case "o" and caps case "O"
                 replacement = as.character(fw[[i]]),
                 x = xf)
    }
  }

  os <- grep(pattern = "[^0-9]", x = xf)
  if(length(os)){
    nonNum <- cbind(os, xf[os])
    print(nonNum)
    # print("there exist other symbols not numbers")
  }

  return(as.numeric(xf))
}
