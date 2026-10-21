test_that("linear part of spline is linear", {
  precision <- 100
  k <- 5
  vec <- rep(0, length.out = k^2)
  vec[1] <- 5
  vec[2] <- 0.4
  x <- seq(0, 1, length.out = precision)
  expect_equal( # columns should be equal to linear function
    spline_2d(vec, make_matrix(precision, k, dimensions = 2))[, 1],
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
    check_equality(spline_2d(vec, make_matrix(precision, k, dimensions = 2)))
  )
})

test_that("function is equal to manual calculations", {
  precision <- 100
  k <- 5
  set.seed(555)
  vec <- rnorm(k^2)
  knots <- seq(0.05, 0.95, length.out = k)
  x <- seq(0, 1, length.out = precision)
  r_spline_2d <- function(row, col) {
    # make separate 1D vectors
    calc_1d_row <- c(1, row,
      basis(row, knots, 2), # j argument starts at 0
      basis(row, knots, 3),
      basis(row, knots, 4))
    calc_1d_col <- c(1, col,
                     basis(col, knots, 2), # j argument starts at 0
                     basis(col, knots, 3),
                     basis(col, knots, 4))

    # caluclate the tensor of the 1D vectors
    calc_2d <- numeric(length = k^2)
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

  expect_equal(spline_2d(vec, make_matrix(precision, k, dimensions = 2)), calculation)
})
