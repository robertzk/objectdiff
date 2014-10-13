context('patches')

test_that('the identity patch has nothing in its environment', {
  expect_identical(environment(identity_patch(NULL)), emptyenv())
})

