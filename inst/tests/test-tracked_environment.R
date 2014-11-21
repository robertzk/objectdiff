context('tracked environment')
library(testthatsomemore)

test_that('it can create a tracked_environment', {
  assert(tracked_environment(new.env()))
})

test_that('it can procure the underlying environment', {
  x <- new.env()
  expect_identical(environment(tracked_environment(x)), x)
  expect_identical(as.environment(tracked_environment(x)), x)
})

test_that("it doesn't allow recursively tracked environments", {
  expect_error(tracked_environment(tracked_environment(new.env())), "Recursion")
})

test_that("the $ operator works", {
  x <- tracked_environment(list2env(list(x = 1)))
  expect_identical(x$x, 1)
})

test_that("the [[ operator works", {
  x <- tracked_environment(list2env(list(x = 1)))
  expect_identical(x[['x']], 1)
})

test_that('it can assign to the underlying environment', {
  x <- new.env()
  tx <- tracked_environment(x)
  assign('x', 1, tx)
  expect_identical(x$x, 1)
})

test_that('it does not break assignment to an environment', {
  x <- new.env()
  assign('x', 1, x)
  expect_identical(x$x, 1)
})

test_that('it can use ls on tracked_environments', {
  x <- tracked_environment(new.env())
  x$x <- 1
  x$y <- 2
  expect_equal(ls(x), c('x', 'y'))
})

test_that('it can use ls on environments', {
  x <- new.env()
  x$x <- 1
  x$y <- 2
  expect_equal(ls(x), c('x', 'y'))
})

test_that('it can use rm on tracked_environments', {
  x <- tracked_environment(new.env())
  x$x <- 1
  x$y <- 2
  rm('y', envir = x)
  expect_equal(ls(x), 'x')
})

test_that('it can still use rm on environments', {
  x <- new.env()
  x$x <- 1
  x$y <- 2
  rm('y', envir = x)
  expect_equal(ls(x), 'x')
})

test_that('the %$% infix operator works', {
  x <- tracked_environment(y <- new.env())
  expect_identical(x%$%env, y)
})

test_that('$<- assignment ghosts', {
  x <- tracked_environment(new.env())
  x$x <- 1
  expect_null((x%$%ghost)$x, NULL)
})

test_that('[[<- assignment ghosts', {
  x <- tracked_environment(new.env())
  x[['x']] <- 1
  expect_null((x%$%ghost)$x, NULL)
})

test_that('assign assignment ghosts', {
  x <- tracked_environment(new.env())
  assign('x', 1, envir = x)
  expect_null((x%$%ghost)$x, NULL)
})

test_that('the replay function works', {
  x <- tracked_environment()
  x$x <- 1; commit(x) <- 'blah'
  x$x <- 2; commit(x) <- 'blah'
  x$x <- 3; commit(x) <- 'blah'
  replay(x, 2)
  expect_equal(x$x, 2)
  replay(x, 1)
  expect_equal(x$x, 1)
})

