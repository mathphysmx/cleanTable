#' @title checks for duplicated rows or columns and allows to delete them.
#' @param ID Are the row numbers of the complete table.
#' @return A list with a table (data.frame/matrix) with no duplicated columns, rows and without blanks

# IMPROVEMENTS. If two different columns have the same name, rename them properly. http://stackoverflow.com/questions/29087520/how-to-remove-duplicated-by-name-column-in-data-tables-in-r
DupRowCol <- function(x, ID = 1:nrow(x)){
  
  TF_colNames <- duplicated(names(x))
  "There are duplicated names in the following"
  dup_colNames <- cbind(VarName = names(x)[TF_colNames], ColumnNumber = (1:ncol(x))[TF_colNames])
  dup_colNamesCol <- which(colnames(x) == dup_colNames[,1])
  all.equal(x[, dup_colNamesCol])
  TF_FOLIO <- duplicated(x[,1])
  x[, 1][TF_FOLIO]
  
  
  dRows_TF <- duplicated(  x ) # row duplicated?
  dCols_TF <- duplicated(t(x)) # column duplicated?
  
}