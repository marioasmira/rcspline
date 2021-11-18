#' A function that takes a vector and compatable matrix and
#' returns a restricted cubic spline
#'
#' @param vector A vector
#' @param matrix The M matrix to calculate the restricted cubic spline;
#' number of columns should be the same as the length of the vector
#' @return Vector with the sline values; length is the same as the number
#' of rows in matrix
#' @examples
#' M <- make_matrix(1000, 5)
#' spline_1d(v, M)
#' @export
spline_1d <- function(
    vector, matrix
    ) {
  y <- double(length = nrow(matrix))
  for (i in 1:precision) {
    y[i] <- as.numeric(matrix[i, ] %*% t(vector))
  }
  return(y)
}


#' A function that takes a vector and compatable matrix and
#' returns a 2 dimensional restricted cubic spline
#'
#' @param vector A vector
#' @param matrix The M matrix to calculate the restricted cubic spline;
#' number of columns should be the same as the length of the vector
#' @return Vector with the sline values; length is the same as the number
#' of rows in matrix
#' @examples
#' M <- make_matrix(1000, 5, 2)
#' spline_2d(v, M)
#' @export
spline_2d <- function(
    vector, matrix
    ) {
  precision <- dim(matrix)[1]
  k <- dim(matrix)[2]
  y <- array(data = NA, dim = rep(precision, 2))
  for (i in 1:precision) {
    temp_vector <- double()
    for (j in 1:precision) {
      even_more_temp_vector <- numeric()
      for (m in 1:k) {
        even_more_temp_vector <-
          c(even_more_temp_vector, as.vector(matrix[i, m, j, ]))
      }
      value <- as.numeric(even_more_temp_vector %*% t(vector))
      temp_vector <- c(temp_vector, value)
    }
    y[i, ] <- as.numeric(temp_vector)
  }
  return(y)
}
