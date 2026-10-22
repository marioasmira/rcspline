#' Plots one dimensions reaction norms
#'
#' @param matrix Matrix used to calculate the reaction norm
#' @param reac_norm Reaction norm calculated with `nspline()` or `spline_1d()`
#'
#' @returns A `ggplot2` object to modify or plot directly
#' @import ggplot2
#' @export
#'
#' @examples
#' # set parameters
#' p <- 100
#' k <- 5
#' d <- 1
#'
#' # make vector
#' set.seed(123)
#' vec <- rnorm(k^d)
#'
#' # calculate matrix and reaction norm
#' m <- make_matrix(p, k, d)
#' norm <- nspline(vec, m, d, p)
#'
#' # plot
#' plot_1d(m, norm)
#'
#' # or save plot in a variable
#' plot_norm <- plot_1d(m, norm)
plot_1d <- function(
  matrix,
  reac_norm
) {
  # check sizes
  if (nrow(matrix) != length(reac_norm)) {
    stop("The number of matrix rows and the length of the reaction norm should be equal.")
  }

  # make temporary data frame for the plot
  plot_data <- data.frame(
    env = matrix[, 2], # second column is the environment
    pheno = reac_norm
  )

  ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$env, y = .data$pheno)
  ) +
    ggplot2::geom_line()
}
