#' Calculate Coefficient of Variation (as percent not decimal fraction, e.g., values fall on a scale of 0-100)
#'
#' @param x a vector of two or more values
#'
#' @return a single numeric value
#' @export
#'
#' @examples
#' cv(c(10, 20))
#' cv(c(10:30))
cv <- function(x) {
  val <- sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE) * 100
  if (length(na.omit(x)) < 2) {
    val <- NA # has no meaning for n = 1
  }
  return(as.numeric(val))
}
