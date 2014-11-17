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
