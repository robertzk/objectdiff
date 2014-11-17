context('environments') 

test_that('it can patch an empty environment', {
  x <- new.env(parent = emptyenv())
  expect_diff(x, x)
})

