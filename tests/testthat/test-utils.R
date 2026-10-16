test_that("logis(0) returns 0.5", {
  expect_equal(logis(0), 0.5)
})

test_that("logis() is non-negative with a range of negative and positive inputs", {
  expect_all_true(c(
    logis(c(-1e10, -1e5, 0, 1e5, 1e10)) >= 0
  ))
})

test_that("logis(1e10, max = 5.5) approaches specified asymptote", {
  expect_equal(logis(1e10, max = 5.5), 5.5)
})

test_that("changing asymptote with logis(-1e10, max = 5.5) does not change minimum", {
  expect_equal(logis(-1e10, max = 5.5), 0)
})

test_that("increasing monotonic behaviour with positive k", {
  expect_all_true(c(
    logis(-5, k = 1) < logis(-1, k = 1),
    logis(-1, k = 1) < logis(0, k = 1),
    logis(0, k = 1) < logis(1, k = 1),
    logis(1, k = 1) < logis(5, k = 1)
  ))
})

test_that("decreasing monotonic behaviour with negative k", {
  expect_all_true(c(
    logis(-5, k = -1) > logis(-1, k = -1),
    logis(-1, k = -1) > logis(0, k = -1),
    logis(0, k = -1) > logis(1, k = -1),
    logis(1, k = -1) > logis(5, k = -1)
  ))
})

test_that("confirm symmetry property", {
  expect_equal(1 - logis(0.1), logis(-0.1))
})

test_that("confirm symmetry property, when max is different than default", {
  expect_equal(4.6 - logis(0.1, max = 4.6), logis(-0.1, max = 4.6))
})

test_that("confirm symmetry property when k is different from default", {
  expect_equal(1 - logis(0.1, k = 0.4), logis(-0.1, k = 0.4))
})

test_that("confirm symmetry property is different when x_0 is different from default", {
  expect_equal(1 - logis(2 + 0.1, x_0 = 2), logis(2 - 0.1, x_0 = 2))
})
