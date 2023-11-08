
#' Calculate root mean squared error
#'
#' @param observed vector of observed values
#' @param predicted vector of predicted values
#'
#' @return a vector of numeric values
#' @export
#'
#' @examples
#' rmse(1:10, 11:20)
rmse <- function(observed, predicted) {
  if (!identical(length(predicted), length(observed))) stop('observed and predicted vectors are different lengths.\n')
  pred <- predicted[!is.na(observed) & !is.na(predicted)]
  obs <- observed[!is.na(observed) & !is.na(predicted)]
  rmse_val <- sqrt(mean((obs - pred)^2))
  return(as.numeric(rmse_val))
}
