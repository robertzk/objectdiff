context('data.frame')

test_that('it can patch a data.frame with a small patch', {
  iris2 <- iris
  iris2[1, 1] <- NA
  expect_diff(iris, iris2, small = 3000)
  # Should be even smaller
  # See: https://github.com/robertzk/objectdiff/issues/11
})

# https://github.com/robertzk/objectdiff/issues/35
test_that('it can record dropping of rows #35', {
  expect_diff(iris, iris[-1, ])
  expect_diff(iris, iris[-150, ])
  expect_diff(iris, iris[c(FALSE, TRUE), ])
  expect_diff(iris, iris[1:100, ])
})

# https://github.com/robertzk/objectdiff/issues/52
test_that('it ignores row.names during patching', {
  x <- iris; y <- x
  attr(y, "row.names") <- letters
  expect_identical(objectdiff(x, y)(x), x)
})

