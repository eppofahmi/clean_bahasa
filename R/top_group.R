#' Top n by group 
#' 
#' Taking top n by group in a column
#' 
#' @param data A data frame
#' @param groupx A Character group name of a column 
#' @param columnx A Character column name to take 
#' @param n Top n of a column to take 
#' @return Top n by group
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr top_n
#' 
#' @examples 
#' \dontrun{
#' library(cleanBahasa)
#' data <- data.frame(
#' col_1   = runif(90),
#' col_2 = gl(3, 30))
#' 
#' data <- top_group(data, "col_1", "col_2", 5)
#' }
#' 
#' @export

top_group <- function(data, groupx, columnx, n) {
  data <- data %>%
    group_by(as.character(groupx)) %>%
    top_n(n = n, wt = as.character(columnx))
  return(data)
  }