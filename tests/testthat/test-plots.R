test_that("plot_1d outputs a ggplot2 plot", {
  p <- 100
  k <- 5
  d <- 1

  set.seed(231)
  vec <- rnorm(k^d)

  m <- make_matrix(p, k, d)
  norm <- nspline(vec, m, d, p)

  # Check that no message/warning/error is thrown
  expect_silent(plot_1d(m, norm))

  test_plot <- plot_1d(m, norm)

  # First check that you really have a plot
  expect_s3_class(test_plot, "gg")
})

test_that("plot_1d shows error with different sizes", {
  p <- 100
  k <- 5
  d <- 1

  set.seed(231)
  vec <- rnorm(k^d)

  m <- make_matrix(p, k, d)
  norm <- nspline(vec, m, d, p)

  # Check that no message/warning/error is thrown
  expect_error(plot_1d(m, norm[1:5]), "The number of matrix rows and the length of the reaction norm should be equal.", fixed = TRUE)
})
