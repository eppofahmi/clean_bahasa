#' Convert multiple variavble as numeric
#' 
#' Convert multiple variable (column) as numeric variable
#' 
#' @param data A data frame
#' @param col_name Mutliple character vectors (column name)
#' 
#' @examples 
#' \dontrun{
#' DF <- data.frame("a" = as.character(0:5),
#' "col_a" = paste(0:5, ".1", sep = ""),
#' "col_b" = letters[1:6],
#' stringsAsFactors = FALSE)
#' 
#' test_01 <- as_numeric(data = DF, col_name = c("col_a", "col_b"))
#' }
#' 
#' @export

as_numeric <- function(data, col_name) {
  # nama kolom
  cols.num <- as.character(col_name)
  # as.numeric
  DF[cols.num] <- sapply(DF[cols.num], as.numeric) 
  return(DF)
}
