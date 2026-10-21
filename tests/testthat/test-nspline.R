test_that("undefined input parameters throw errors", {
  v <- set.seed(999)
  precision <- 100
  k <- 5
  dimensions <- 1
  vec <- rnorm(k)
  m <- make_matrix(precision, k, dimensions)
  expect_error(nspline(vec, m, -3, precision), "Dimensions and precision should be greater than zero.", fixed = TRUE)
  expect_error(nspline(vec, m, dimensions, -5), "Dimensions and precision should be greater than zero.", fixed = TRUE)
})

test_that("2D linear part of spline is linear", {
  precision <- 100
  k <- 5
  dimensions <- 2
  vec <- rep(0, length.out = k^dimensions)
  vec[1] <- 5
  vec[2] <- 0.4
  x <- seq(0, 1, length.out = precision)
  expect_equal( # columns should be equal to linear function
    nspline(vec, make_matrix(precision, k, dimensions = dimensions), dimensions, precision)[, 1],
    vec[1] + x * vec[2]
  )
  check_equality <- function(m) {
    logic_test <- vector(length = dim(m)[2] - 1)
    for (c in 2:dim(m)[2]) {
      logic_test[c - 1] <- all(m[, 1] == m[, c])
    }
    all(logic_test)
  }
  expect_all_true( # all columns should be equal
    check_equality(nspline(vec, make_matrix(precision, k, dimensions = dimensions), dimensions, precision))
  )
})

test_that("2D function is equal to manual calculations", {
  precision <- 100
  k <- 5
  dimensions <- 2
  set.seed(555)
  vec <- rnorm(k^dimensions)
  knots <- seq(0.05, 0.95, length.out = k)
  x <- seq(0, 1, length.out = precision)
  r_spline_2d <- function(row, col) {
    # make separate 1D vectors
    calc_1d_row <- c(
      1, row,
      basis(row, knots, 2), # j argument starts at 0
      basis(row, knots, 3),
      basis(row, knots, 4)
    )
    calc_1d_col <- c(
      1, col,
      basis(col, knots, 2), # j argument starts at 0
      basis(col, knots, 3),
      basis(col, knots, 4)
    )

    # calculate the tensor of the 1D vectors
    calc_2d <- numeric(length = k^dimensions)
    for (i in 1:k) {
      for (j in 1:k) {
        calc_2d[i + k * (j - 1)] <- calc_1d_row[i] * calc_1d_col[j]
      }
    }
    sum(calc_2d * vec)
  }

  # manually calculate the 2D reaction norm
  calculation <- matrix(0, nrow = precision, ncol = precision)
  for (i in 1:precision) {
    for (j in 1:precision) {
      calculation[i, j] <- r_spline_2d(x[i], x[j])
    }
  }

  expect_equal(nspline(vec, make_matrix(precision, k, dimensions = 2), dimensions, precision), calculation)
})

test_that("3D linear part of spline is linear", {
  precision <- 10
  k <- 5
  dimensions <- 3
  vec <- rep(0, length.out = k^dimensions)
  vec[1] <- 5
  vec[2] <- 0.4
  x <- seq(0, 1, length.out = precision)
  expect_equal( # last dimension should be equal to linear function
    nspline(vec, make_matrix(precision, k, dimensions = dimensions), dimensions, precision)[, 1, 1],
    vec[1] + x * vec[2]
  )
  check_equality <- function(m) {
    logic_test <- vector(length = dim(m)[3] - 1) # size of 3rd dimension
    for (c in 2:dim(m)[3]) {
      logic_test[c - 1] <- all(m[, 1, 1] == m[, 1, c]) # check 3rd dimension is equal
    }
    all(logic_test)
  }
  expect_all_true( # all columns should be equal
    check_equality(nspline(vec, make_matrix(precision, k, dimensions = dimensions), dimensions, precision))
  )
})

test_that("3D function is equal to manual calculations", {
  precision <- 10
  k <- 5
  dimensions <- 3
  set.seed(555)
  vec <- rnorm(k^dimensions)
  knots <- seq(0.05, 0.95, length.out = k)
  x <- seq(0, 1, length.out = precision)
  r_spline_3d <- function(row, col, hei) {
    # make separate 1D vectors
    calc_1d_row <- c(
      1, row,
      basis(row, knots, 2), # j argument starts at 0
      basis(row, knots, 3),
      basis(row, knots, 4)
    )
    calc_1d_col <- c(
      1, col,
      basis(col, knots, 2), # j argument starts at 0
      basis(col, knots, 3),
      basis(col, knots, 4)
    )
    calc_1d_hei <- c(
      1, hei,
      basis(hei, knots, 2), # j argument starts at 0
      basis(hei, knots, 3),
      basis(hei, knots, 4)
    )
    # calculate the tensor of the 1D vectors
    calc_3d <- numeric(length = k^dimensions)
    for (i in 1:k) {
      for (j in 1:k) {
        for (l in 1:k) {
          calc_3d[i + k * (j - 1) + k * k * (l - 1)] <- calc_1d_row[i] * calc_1d_col[j] * calc_1d_hei[l]
        }
      }
    }
    sum(calc_3d * vec)
  }

  # manually calculate the 3D reaction norm
  calculation <- numeric(length = 1000)
  dim(calculation) <- c(precision, precision, precision)
  for (i in 1:precision) {
    for (j in 1:precision) {
      for (l in 1:precision) {
        calculation[i, j, l] <- r_spline_3d(x[i], x[j], x[l])
      }
    }
  }

  expect_equal(nspline(vec, make_matrix(precision, k, dimensions), dimensions, precision), calculation)
})
