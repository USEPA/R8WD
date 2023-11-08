#' Calculate Relative Percent Difference
#'
#' @param x a vector of two values
#'
#' @return a single numeric value
#' @export
#'
#' @examples
#' rpd(c(10, 20))
rpd <- function(x) {
  # calculate relative percent difference
  val <- abs(diff(na.omit(x))) / mean(x, na.rm = TRUE) * 100
  if (length(na.omit(x)) < 2) {
    val <- NA # has no meaning for n = 1
  }
  if (length(na.omit(x)) > 2) {
    val <- NA # not appropriate for n >2
    cat('Relative percent differences should not be used where n>2\n')
  }
  return(as.numeric(val))
}
