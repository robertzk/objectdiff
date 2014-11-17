context('s3 objects')

test_that('it can diff a trivial s3 object', {
  x <- list(1)
  class(x) <- 'boo'
  y <- x
  x[[1]] <- 2
  expect_diff(x, y)
})

test_that('it can diff an lm object', {
  lmo <- lm(Sepal.Width ~ Sepal.Length, iris)
  iris2 <- iris; iris2[1, 1] <- 7
  lmo2 <- lm(Sepal.Width ~ Sepal.Length, iris2)
  expect_diff(lmo, lmo2)
})

