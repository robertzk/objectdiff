context('tracked environment')
library(testthatsomemore)

test_that('it can create a tracked_environment', {
  assert(tracked_environment(new.env()))
})

test_that('it can check that something is a tracked environment', {
  expect_true(is.tracked_environment(tracked_environment()))
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

test_that("the meta-environment %$% infix operator works", {
  x <- tracked_environment(list2env(list(x = 1)))
  expect_identical(as.list(x%$%env), list(x = 1))
})

test_that("the meta-environment %$%<- infix assignment operator works", {
  x <- tracked_environment(list2env(list(x = 1)))
  x%$%env <- list2env(list(x = 2))
  expect_identical(as.list(x%$%env), list(x = 2))
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

test_that('it errors when we try to assign environments directly', {
  x <- tracked_environment()
  expect_error(environment(x) <- new.env(), 'Cannot assign')
})

test_that('it does not break function environment assignment', {
  x <- function() { }
  y <- new.env()
  environment(x) <- y
  expect_identical(environment(x), y)
})

test_that('get works for getting a variable from a tracked_environment', {
  x <- tracked_environment()
  x$x <- 1
  expect_identical(get('x', envir = x), 1)
})

test_that("get works for getting a variable from a tracked_environment's meta-environment", {
  x <- tracked_environment(snapshot = 5)
  expect_identical(get('snapshot', envir = x, mode = 'meta'), 5)
})

test_that("get doesn't stop working for regular environments", {
  x <- new.env()
  x$x <- 1
  expect_identical(get('x', envir = x), 1)
})

test_that("mode = 'meta' doesn't work for regular environments", {
  x <- new.env()
  x$x <- 1
  expect_error(get('x', envir = x, mode = 'meta'))
})

test_that("it allows parent.env getting", {
  z <- new.env()
  y <- new.env(parent = z)
  x <- tracked_environment(y)
  expect_identical(parent.env(environment(x)), z)
})

test_that("it allows parent.env getting", {
  z <- new.env()
  y <- new.env(parent = z)
  x <- tracked_environment(y)
  w <- new.env()
  parent.env(x) <- w
  expect_identical(parent.env(environment(x)), w)
})

