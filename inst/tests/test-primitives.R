context('primitives')

test_that('it does not break the ls function', {
  x <- 1
  expect_true('x' %in% ls())
  expect_true('x' %in% ls(envir = environment()))
})

test_that('it does not break the ls function with the inherits parameter', {
  x <- 1
  local({
    y <- 1
    expect_true('y' %in% ls())
    expect_false('x' %in% ls())
    expect_true('y' %in% ls(envir = environment()))
    expect_false('x' %in% ls(envir = environment()))
  })
})

test_that('it does not break the rm function', {
  x <- 1
  rm('x', envir = environment(), inherits = FALSE)
  expect_false('x' %in% ls())
})

