#' normal_text
#'
#' Used to nomalised text of a column in a data frame based on user defined
#' normalisation lexicon
#'
#' @param data_name A data frame name
#' @param col_name A column name of a data frame
#' @param nomral_lex A two column data frame contain of actual terms and its normal term
#' @return A normalised column in a data frame
#'
#' @importFrom textclean mgsub_regex
#' @importFrom dplyr bind_cols
#'
#' @examples
#' \dontrun{
#' library(cleanBahasa)

#' sample_text <- data.frame(text = c("ak ad kerjaan", "dia sdh pergi"))
#' kt_normal <- data.frame(from = c("ad", "ak", "sdh"),
#'                         to = c("ada", "aku", "sudah"))
#'
#' test_01 <- normal_text(data_name = sample_text,
#' col_name = "text", nomral_lex = kt_normal)
#' }
#'
#' @export

normal_text <- function(data_name, col_name, nomral_lex) {
  data1 <- as.data.frame(data_name)
  column <- as.character(col_name)
  kt_normal <- nomral_lex

  # normalisation
  kt_normal$from <- paste0("\\b", kt_normal$from, "\\b") # excact macth
  pattern1 <- as.character(kt_normal$from)
  replacement1 <- as.character(kt_normal$to)

  data2 <- as.character(data_name[, 1])

  data2 <- textclean::mgsub_regex(data2, pattern = pattern1, replacement = replacement1, fixed = FALSE)
  data2 <- data.frame(data2)

  data <- dplyr::bind_cols(data1, data2)
  colnames(data) <- c("before", "after")
  return(
    data
    )
  }
