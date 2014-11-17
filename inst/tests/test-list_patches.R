context('lists')

test_that('it can patch the trivial list to itself', {
  expect_diff(list(), list())
})
