context('force_push')

test_that("it can force push to the zeroth commit", {
  env <- tracked_environment()
  force_push(env, 0)
})

test_that("it can force push back to the zeroth commit", {
  env <- tracked_environment()
  env$x <- 1; commit(env) <- 'first'
  force_push(env, 0)
  expect_equal(length(ls(env)), 0)
})

test_that("it can force push to a previous commit, then force push to the future", {
  env <- tracked_environment()
  for (i in seq_along(letters)) {
    env[[letters[i]]] <- i
    commit(env) <- paste('commit', i)
  }
  for (i in c(15, 10, 5, 15)) {
    force_push(env, i)
    expect_identical(sort(ls(env)), letters[seq_len(i)])
  }
})

test_that("it can force push by name", {
  env <- tracked_environment()
  env$x <- 1; commit(env) <- 'first'
  env$y <- 1; commit(env) <- 'second'
  env$z <- 1; commit(env) <- 'third'
  force_push(env, 'first')
  expect_identical(as.list(as.environment(env)), list(x = 1))
})

test_that("it offers a warning when force pushing to a named commit matching multiple entries", {
  env <- tracked_environment()
  env$x <- 1; commit(env) <- 'first'
  env$y <- 1; commit(env) <- 'first'
  env$z <- 1; commit(env) <- 'third'
  expect_warning(force_push(env, 'first'), "Multiple commits")
})

test_that("it errors when an invalid commit is force pushed to", {
  env <- tracked_environment()
  expect_error(force_push(env, "blah"), "There is no commit")
})

test_that("it can overwrite commits after a force push backward", {
  env <- tracked_environment()
  env$x <- 1; commit(env) <- 'first'
  env$y <- 1; commit(env) <- 'second'
  env$z <- 1; commit(env) <- 'third'
  force_push(env, 1)
  env$y <- 2; commit(env) <- 'second'
  env$z <- 3; commit(env) <- 'third'
  expect_identical(as.list(as.environment(env)), list(x = 1, y = 2, z = 3))
  rollback(env) <- 1
  expect_identical(as.list(as.environment(env)), list(x = 1, y = 2))
})

test_that("replaying the current commit is a no-op", {
  with_mock(`objectdiff:::copy_env` = function(...) { stop("copy invoked") }, {
    env <- tracked_environment()
    env$x <- 1; commit(env) <- 'first'
    rollback(env) <- 0 # No error expected!
    env$x <- 2
    expect_error(rollback(env) <- 0, "copy invoked")
    env$x <- 2
    expect_error(force_push(env, 1), "copy invoked")
  })
})

