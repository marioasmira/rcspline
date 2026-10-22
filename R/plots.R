#' Plots one dimensional reaction norms
#'
#' Function takes a matrix made with [make_matrix()] and a reaction norm
#' calculated with [nspline()] (or [spline_1d()]) and creates a `ggplot2`
#' compatible plot.
#'
#' @param matrix_m Matrix used to calculate the reaction norm; made with
#' [make_matrix()]
#' @param reac_norm Reaction norm calculated with [nspline()] or [spline_1d()]
#'
#' @returns A `ggplot2` object
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
  matrix_m,
  reac_norm
) {
  # check sizes
  if (nrow(matrix_m) != length(reac_norm)) {
    stop("The number of matrix rows and the length of the reaction norm should be equal.")
  }

  # make temporary data frame for the plot
  plot_data <- data.frame(
    env = matrix_m[, 2], # second column is the environment
    pheno = reac_norm
  )

  ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$env, y = .data$pheno)
  ) +
    ggplot2::geom_line()
}

#' Plots two dimensional reaction norms
#'
#' Function takes a matrix made with [make_matrix()] and a reaction norm
#' calculated with [nspline()] (or [spline_2d()]) and creates a `ggplot2`
#' compatible plot.
#'
#' @param matrix_m Matrix used to calculate the reaction norm; made with
#' [make_matrix()]
#' @param reac_norm Reaction norm calculated with [nspline()] or [spline_2d()]
#'
#' @returns A `ggplot2` object
#' @import ggplot2
#' @import dplyr
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' # set parameters
#' p <- 100
#' k <- 5
#' d <- 2
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
#' plot_2d(m, norm)
#'
#' # or save plot in a variable
#' plot_norm <- plot_2d(m, norm)
plot_2d <- function(
  matrix_m,
  reac_norm
) {
  # check sizes
  if (nrow(matrix_m) != length(reac_norm)) { # should still work for matrices
    stop("The number of matrix rows and the length of the reaction norm should be equal.")
  }

  # make temporary data frame for the plot
  plot_data <- reac_norm %>%
    tibble::as_tibble(.name_repair = "unique_quiet") %>%
    tibble::rownames_to_column("env1") %>%
    tidyr::pivot_longer(-"env1", names_to = "env2", values_to = "pheno") %>%
    dplyr::mutate(
      env1 = as.numeric(.data$env1),
      env2 = as.numeric(gsub("...", "", .data$env2, fixed = TRUE))
    ) %>%
    dplyr::mutate(
      env1 = matrix_m[.data$env1, 2], # second column is the environment
      env2 = matrix_m[.data$env2, 2]
    )

  ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$env1, y = .data$env2)
  ) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$pheno)) +
    ggplot2::scale_fill_gradient(low = "white", high = "red")
}
