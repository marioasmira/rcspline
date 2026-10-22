#' Logistic function
#'
#' Transforms a real value, x, to be between 0 and carrying capacity, max. The
#' growth rate, k, specifies the steepness of the slope. The inflection point,
#' x_0, specifies the point where the function changes from exponential to
#' logarithmic growth.
#'
#' @param x The value to transform
#' @param max The asymptote of the function; carrying capacity
#' @param k The growth rate of the function
#' @param x_0 The inflection point
#' @return Returns the transformed value of the input
#' @export
logis <- function(x, max = 1, k = 1, x_0 = 0) {
  max / (1 + exp(-k * (x - x_0)))
}
