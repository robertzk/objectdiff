context('commits')

test_that('it can fetch all commits in a tracked_environment', {
  env <- tracked_environment()
  env$x <- 1; commit(env) <- 'first'
  env$y <- 2; commit(env) <- 'second'
  env$z <- 3; commit(env) <- 'third'
  expect_equal(names(commits(env)), c('first', 'second', 'third'))
})

