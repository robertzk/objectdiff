context('patches')

test_that('the identity patch has nothing in its environment', {
  expect_identical(environment(identity_patch()), emptyenv())
})

test_that('the identity patch patches a simple example correctly', {
  for (obj in list(iris, 1:10, "X", function(x) y, as.environment(list(x = 1, y = 2))))
    expect_identical(identity_patch()(obj), obj)
})


