#' A function to create an M matrix to help with
#' restricted cubic spline calculations
#'
#' @param precision how precise the calculation should be;
#' it will the be the number of rows in the output
#' @param k the number of knots used
#' @param dimensions the number of dimentions to create
#' (this grows the size VERY fast)
#' @param max the maximum value of the x values; default = 1
#' @param first_knot position of the first knot; default = 0.05 * max
#' @param last_knot position of the last knot; default = 0.95 * max
#' @return a matrix of size precision by k
#' @examples
#' make_matrix(1000, 5)
#' @export
make_matrix <- function(
    precision, k, dimensions = 1, max = 1, first_knot = 0.05, last_knot = 0.95
    ) {
  x <- seq(0, max, length.out = precision)
  knots <- seq(first_knot * max, last_knot * max, length.out = k)
  xx <- Hmisc::rcspline.eval(x, knots = knots, inclx = T)
  zero <- rep(1, precision)
  xx <- cbind(zero, xx)
  if (dimensions > 1) {
      multi_xx <- xx
      for (i in 1:(dimensions - 1)) {
          multi_xx <- tensor::tensor(multi_xx, xx)
      }
      return(multi_xx)
  } else {
    return(xx)
  }
}
