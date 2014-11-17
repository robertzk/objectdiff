context('s3 objects')
test_that('it can diff a trivial s3 object', {
  x <- list(1)
  class(x) <- 'boo'
  y <- x
  x[[1]] <- 2
  expect_diff(x, y)
})

