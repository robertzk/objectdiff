context('data.frame')

test_that('it can patch a data.frame with a small patch', {
  iris2 <- iris
  iris2[1, 1] <- NA
  expect_diff(iris, iris2, small = 3000)
  # Should be even smaller
  # See: https://github.com/robertzk/objectdiff/issues/11
})

test_that('it can record dropping of rows', {
  iris2 <- iris
  iris2 <- iris2[-1, ]
  expect_diff(iris, iris2)
})


