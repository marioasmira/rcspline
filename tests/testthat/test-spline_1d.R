test_that("linear part of spline is linear", {
  precision <- 100
  k <- 5
  vec <- c(2, 0.3, 0, 0, 0)
  x <- seq(0, 1, length.out = precision)
  expect_equal(spline_1d(vec, make_matrix(precision, k)), vec[1] + x * vec[2])
})

test_that("function is equal to manual calculations", {
  precision <- 100
  k <- 5
  set.seed(444)
  vec <- rnorm(k)
  knots <- seq(0.05, 0.95, length.out = k)
  x <- seq(0, 1, length.out = precision)
  r_spline_1d <- function(row) {
    vec[1] + row * vec[2] +
      basis(row, knots, 2) * vec[3] + # j argument starts at 0
      basis(row, knots, 3) * vec[4] +
      basis(row, knots, 4) * vec[5]
  }
  calculation <- sapply(x, r_spline_1d)
  expect_equal(spline_1d(vec, make_matrix(precision, k)), calculation)
})
