context('primitives')

test_that('it does not break the ls function', {
  x <- 1
  expect_true('x' %in% ls())
})

