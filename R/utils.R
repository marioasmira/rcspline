#' logistic function
#'
#' @param x the value to transform
#' @param max the assymptote of the function
#' @param k the rate of the function
#' @param x_0 the inflection point
#' @return returns the transformed value of the input
#' @export
logis <- function(x, max = 1, k = 1, x_0 = 0) {
  max / (1 + exp(-k * (x - x_0)))
}