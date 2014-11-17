context('lists')

test_that('it can patch the trivial list to itself', {
  expect_diff(list(), list())
})

test_that('it can patch the trivial list to a non-trivial list', {
  expect_diff(list(), as.list(1:10))
})

test_that('it can patch a non-trivial list to the trivial list', {
  expect_diff(as.list(1:10), list())
})
test_that('it can patch a huge list with only a tiny change', {
  x <- as.list(1:10000)
  y <- x
  y[[10000]] <- 1

  expect_diff(x, y)
})
test_that('it does not take up too much space when patching a huge list with only a tiny change', {
  x <- as.list(1:10000)
  y <- x
  y[[10000]] <- 1

  expect_less_than(object.size(environment(objectdiff(x, y))), 1000)
})


