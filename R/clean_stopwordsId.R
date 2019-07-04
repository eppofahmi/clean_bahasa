#' Delete Indonesian stopwords
#'
#' Used to strip out stopwords bahasa Indoensia
#'
#' @param data_name A data frame name
#' @param col_name A column name of a data frame
#' @return A normalised column in a data frame
#'
#' @importFrom textclean mgsub_regex
#' @importFrom textclean replace_white
#' @importFrom dplyr bind_cols
#' @importFrom dplyr data_frame
#' @importFrom stopwords stopwords
#'
#' @examples
#' \dontrun{
#' library(cleanBahasa)

#' sample_text <- data.frame(text = c("di kantor saya sedang ada banyak kerjaan",
#' "kami akan pergi dengan mobil "))
#'
#' test_01 <- clean_text(data_name = sample_text, col_name = "text")
#' }
#' 
#' @export

clean_text <- function(data_name, col_name) {
  data1 <- as.data.frame(data_name)
  column <- as.character(col_name)
  kt_normal <- dplyr::data_frame(stopwords::stopwords(language = "id", source = "stopwords-iso"))
  colnames(kt_normal) <- "from"
  # replace stopwords id
  kt_normal$from <- paste0("\\b", kt_normal$from, "\\b") # excact macth
  pattern1 <- as.character(kt_normal$from)
  replacement1 <- ""
  data2 <- as.character(data_name[, 1])
  data2 <- textclean::mgsub_regex(data2, pattern = pattern1, replacement = replacement1, fixed = FALSE)
  data2 <- data.frame(data2)
  data <- dplyr::bind_cols(data1, data2)
  colnames(data) <- c("before", "after")
  data$after <- textclean::replace_white(data$after)
  return(data)
  }
