test_that("make_matrix returns correct dimensions", {
  expect_identical(dim(make_matrix(100, 5)), c(100L, 5L))
  expect_identical(dim(make_matrix(50, 3, 2)), c(2500L, 9L))
  expect_identical(dim(make_matrix(10, 4, 3)), c(1000L, 64L))
})

test_that("first two columns are intercept and linear term", {
  M <- make_matrix(100, 5, min = 0, max = 1)
  expect_true(all(M[, 1] == 1))
  expect_true(all(abs(M[, 2] - seq(0, 1, length.out = 100)) < 1e-10))
})

test_that("undefined input parameters throw errors", {
  expect_error(make_matrix(0, 5, 3), "Integer parameters (precision, k, and dimensions) should be greater than zero.", fixed = TRUE)
  expect_error(make_matrix(10, 0, 3), "Integer parameters (precision, k, and dimensions) should be greater than zero.", fixed = TRUE)
  expect_error(make_matrix(10, 5, 0), "Integer parameters (precision, k, and dimensions) should be greater than zero.", fixed = TRUE)
  expect_error(make_matrix(-10, 5, 3), "Integer parameters (precision, k, and dimensions) should be greater than zero.", fixed = TRUE)
  expect_error(make_matrix(10, -5, 3), "Integer parameters (precision, k, and dimensions) should be greater than zero.", fixed = TRUE)
  expect_error(make_matrix(10, 5, -3), "Integer parameters (precision, k, and dimensions) should be greater than zero.", fixed = TRUE)
})

test_that("negative ranges throw errors", {
  expect_error(make_matrix(10, 5, 1, min = 10, max = 1), "Function not defined for negative ranges (min > max or first_knot > last_knot).", fixed = TRUE)
  expect_error(make_matrix(10, 5, 1, first_knot = 0.9, last_knot = 0.1), "Function not defined for negative ranges (min > max or first_knot > last_knot).", fixed = TRUE)
})

test_that("negative values throw errors", {
  expect_error(make_matrix(10, 5, 1, min = -1), "Function not defined for negative values.", fixed = TRUE)
  expect_error(make_matrix(10, 5, 1, min = -2, max = -1), "Function not defined for negative values.", fixed = TRUE)
  expect_error(make_matrix(10, 5, 1, first_knot = -0.5), "Function not defined for negative values.", fixed = TRUE)
  expect_error(make_matrix(10, 5, 1, first_knot = -2, last_knot = -1), "Function not defined for negative values.", fixed = TRUE)
})
