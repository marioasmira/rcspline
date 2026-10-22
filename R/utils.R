#' Logistic function
#'
#' Transforms a real value, `x`, to be between 0 and carrying capacity, `max`.
#' The growth rate, `k`, specifies the steepness of the slope. The inflection
#' point, `x_0`, specifies the point where the function changes from exponential
#' to logarithmic growth.
#' Inverse of [logit()].
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

#' Logit function
#'
#' Log-odds; also used to inverse the function [logis()].
#' Transforms a value between 0 and 1, `p`, to be between 0 and carrying
#' capacity, `max`. The growth rate, `k`, specifies the steepness of the slope.
#' The inflection point, `x_0`, specifies the point where the function changes
#' from exponential to logarithmic growth.
#'
#' @param p The value to transform
#' @param max The asymptote of the function; carrying capacity
#' @param k The growth rate of the function
#' @param x_0 The inflection point
#' @return Returns the transformed value of the input
#' @export
logit <- function(p, max = 1, k = 1, x_0 = 0) {
  if (p / max <= 0) {
    stop("p should be above 0.")
  }
  if (p / max >= 1) {
    stop("p should be smaller than max (default: max = 1).")
  }
  x_0 - (1/k) * log((max / p) - 1)
}
