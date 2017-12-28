#' @title Shift cells (or block of cells) horizontally if the previous cell/block is empty
#' @param x table (data.frame/2D-matrix) to be checked
#' @param bLength Block length. Length (number of cells) of each block. Default to 1, i.e., a single cell is a block.
#' @param fillwith The unique number,character, symbol (NA, Inf), etc. which represents empty cells. What is the symbol meaning empty cell?. If untouched, no change will be done on the cell values
#' @param ef argument passed to \link{EmptyRow}
#' @details depends on EmptyRow.R and MoveCells2left.R
#' @export
#' @return
#' \code{x} with the blocks shifted if the case
#' \code{mcb} a table in which each column represents a block and the value of each cell represents the corresponding column in \code{x} from where the block comes to \code{xn}.
#' @examples
#' # example 1
#' # Check what shiftCellsH does to each row
#' flag <- NA
#' xe <- matrix(data = flag, nrow = 6, ncol = 4)
#' xe[1, 1] <- 1
#' xe[2, 3:4] <- 2
#' xe[3, 3] <- 3
#' xe[4, 3] <- 4
#' xe[4, 4] <- 0
#' xe[5, 2:4] <- 0
#' xe[6, 2] <- 0
#' xe[6, 3:4] <- 6
#' print(xe)
#' shiftCellsH(x = xe, bLength = 2,
#'             ef = c(0, flag), fillwith = -999)

shiftCellsH <- function(x, bLength = 1,
                        fillwith = NULL,
                        ef = c(NA, -999, 0)) {

  xf <- x

  # "b" stands for block, "n" for number
  nrows <- nrow(xf)
  ncols <- ncol(xf)
  nb <- ncols/bLength # nb = number of blocks

  bTable <- matrix(data = rep.int(1:nb, nrows),
                   nrow = nrows, ncol = nb,
                   byrow = TRUE)
  # bTable = Each block corresponds to one column with the column number in filled rows and empty rows will be marked as NA
  bBcol <- seq.int(from = 1,       to = nb * (bLength + 1), by = bLength) # blocks Begginings
  bEcol <- seq.int(from = bLength, to = nb * (bLength + 1), by = bLength) # blocks Ends
    for(i in 1:nb){ # ok1
      # i <- 3
    ER <- EmptyRow(xf[, bBcol[i]:bEcol[i]], ef = ef)
    bTable[ER$TF, i] <- NA # ok1
    }
  mcb <- MoveCells2left(df = bTable, ef = NA) # mcb = move filled cells of bTable to the leftmost empty cell
  ec <- all.equal(bTable, as.matrix(mcb), check.attributes = FALSE)
  if(is.logical(ec))
    print("no changes were made")

  xn <- xf
  xn[,] <- NA
  for(i in 1:nrows){
    for(j in 1:nb){
      ch <- mcb[i, j][,1]
      if(!is.na(ch)) {
        xn[i, bBcol[j]:bEcol[j]] <- xf[i, bBcol[ch]:bEcol[ch]]
      }
    }
  }

  if(!is.null(fillwith))
    xn[is.na(xn)] <- fillwith

  xr <- list(xn = xn, mcb = mcb)
  return(xr)
}
