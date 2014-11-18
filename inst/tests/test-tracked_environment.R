context('tracked environment')
library(testthatsomemore)

test_that('it can create a tracked_environment', {
  assert(tracked_environment(new.env()))
})

test_that('it can procure the underlying environment', {
  x <- new.env()
  expect_identical(environment(tracked_environment(x)), x)
  expect_identical(as.environment(tracked_environment(x)), x)
})

test_that("it doesn't allow recursively tracked environments", {
  expect_error(tracked_environment(tracked_environment(new.env())), "Recursion")
})

test_that('it can assign to the underlying environment', {
  x <- new.env()
  tx <- tracked_environment(x)
  assign('x', 1, tx)
  expect_identical(x$x, 1)
})

