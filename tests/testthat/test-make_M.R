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

test_that("linear_spaced_array() behaves like seq()", {
  expect_equal(linear_spaced_array(0, 15, 5)[2], seq(0, 15, length.out = 5)[2])
  expect_equal(linear_spaced_array(-10, 15, 9)[7], seq(-10, 15, length.out = 9)[7])
  expect_equal(linear_spaced_array(-20, 0, 5)[3], seq(-20, 0, length.out = 5)[3])
})

test_that("specific values in the matrix are correct", {
  expect_equal(
    basis(
      linear_spaced_array(0, 1, 100)[55],
      linear_spaced_array(0.05, 0.95, 5),
      3 # note the different column number (here it should start at 0)
    ),
    make_matrix(100, 5)[55, 4] # column value starts at 1
  )
  expect_equal(
    basis(
      linear_spaced_array(0, 10, 100)[73],
      linear_spaced_array(0.05 * 10, 0.95 * 10, 7), # need to scale knots too
      2 # note the different column number (here it should start at 0)
    ),
    make_matrix(100, 7, max = 10)[73, 3] # column value starts at 1
  )
  expect_equal(
    basis( # env 1
      linear_spaced_array(0, 1, 100)[43],
      linear_spaced_array(0.05, 0.95, 5),
      2 # note the different column number (here it should start at 0)
    ) *
      basis( # env 2
        linear_spaced_array(0, 1, 100)[33],
        linear_spaced_array(0.05, 0.95, 5),
        3 # note the different column number (here it should start at 0)
      ),
    make_matrix(100, 5, dimensions = 2)[33 + 100 * (43 - 1), 4 + 5 * (3 - 1)] # column value starts at 1
  )
  expect_equal(
    basis( # env 1
      linear_spaced_array(0, 7, 100)[92],
      linear_spaced_array(0.05 * 7, 0.95 * 7, 5), # notice scaling knots
      1 # note the different column number (here it should start at 0)
    ) *
      basis( # env 2
        linear_spaced_array(0, 7, 100)[27],
        linear_spaced_array(0.05 * 7, 0.95 * 7, 5), # notice scaling knots
        3 # note the different column number (here it should start at 0)
      ),
    make_matrix(100, 5, dimensions = 2, max = 7)[27 + 100 * (92 - 1), 4 + 5 * (2 - 1)] # column value starts at 1
  )
})
