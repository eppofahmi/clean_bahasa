% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_numeric.R
\name{as_character}
\alias{as_character}
\title{Convert multiple variavble as character}
\usage{
as_character(data, col_name)
}
\arguments{
\item{data}{A data frame}

\item{col_name}{Mutliple character vectors (column name)}
}
\description{
Convert multiple variable (column) as numeric character
}
\examples{
\dontrun{
DF <- data.frame("col_a" = as.numeric(0:5),
"col_b" = paste(0:5, ".1", sep = ""),
"col_c" = letters[1:6],
stringsAsFactors = FALSE)

test_01 <- as_character(data = DF, col_name = c("col_a", "col_b"))
}

}
