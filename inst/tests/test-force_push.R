library(testthatsomemore)
context('force_push')

test_that("it can force push to the zeroth commit", {
  env <- tracked_environment()
  assert(force_push(env, 0))
})

test_that("it can force push back to the zeroth commit", {
  env <- tracked_environment()
  env$x <- 1; commit(env) <- 'first'
  force_push(env, 0)
  expect_equal(length(ls(env)), 0)
})

