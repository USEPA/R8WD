

#' Convert a vector into a grammatically correct character element
#'
#' @param x a vector of values
#'
#' @return a character element
#' @export
#'
#' @examples
#' sentencify(1); sentencify(1:2); sentencify(1:5)

sentencify <- function(x) {
  ### turn a char vector into comma/'and'-delimited string
  if (length(x) == 2) {
    collapse_char <- ' and '
  } else {
    collapse_char <- ', '
  }
  x2 <- sub(",([^,]*)$",", and\\1", x = paste0(x, collapse = collapse_char))
  return(x2)
}
