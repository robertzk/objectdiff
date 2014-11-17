context('lists')

test_that('it can patch the trivial list to itself', {
  expect_diff(list(), list(), small = TRUE)
})

test_that('it can patch the trivial list to a non-trivial list', {
  expect_diff(list(), as.list(1:10), small = TRUE)
})

test_that('it can patch a non-trivial list to the trivial list', {
  expect_diff(as.list(1:10), list(), small = TRUE)
})

test_that('it can patch a huge list with only a tiny change', {
  x <- as.list(1:10000)
  y <- x
  y[[10000]] <- 1

  expect_diff(x, y, small = TRUE)
})

test_that('it can patch a huge list with a huge change', {
  x <- as.list(1:10000)
  y <- rev(x)

  expect_diff(x, y, small = FALSE)
})

test_that('it can patch a list name change', {
  x <- as.list(1:10)
  names(x) <- letters[1:10]
  y <- x
  names(y)[10] <- 'boo'
  expect_diff(x, y, small = 2000)
})

test_that('it can patch an attribute change', {
  x <- as.list(1:10)
  y <- x
  attr(y, 'blue') <- iris
  expect_diff(x, y)
})

