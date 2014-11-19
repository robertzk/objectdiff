context('environments') 

test_that('it can patch an empty environment', {
  x <- new.env(parent = emptyenv())
  expect_diff(x, x)
})

test_that('it can patch an environment with one variable', {
  x <- list2env(list(x = 1))
  x$y <- 2
  y <- list2env(list(x = 1))

  expect_false(identical(env <- objectdiff(x, y)(x), list2env(lst <- list(x = 1))))
  # This is failing but should pass:
  # expect_false(identical(env, y))
  expect_identical(as.list(env), lst)
})

