library(testthatsomemore)
context('force_push')

test_that("it can force push to the zeroth commit", {
  env <- tracked_environment()
  assert(force_push(env, 0))
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
