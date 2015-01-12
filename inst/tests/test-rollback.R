context('rollback')

test_that('it can roll back 0, and doing so is a trivial operation', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  e$y <- 2
  commit(e) <- 'Second message'
  rollback(e) <- 0
  expect_identical(as.list(as.environment(e)), list(x = 1, y = 2))
})

test_that('it can roll back a simple example', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  e$x <- 2
  commit(e) <- 'Second message'
  expect_identical(e$x, 2)
  rollback(e) <- 1
  expect_equal(e$x, 1) # The environment should have been rolled back.
})

test_that('it can roll back two commits', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  e$x <- 2
  commit(e) <- 'Second message'
  e$x <- 3
  commit(e) <- 'Third message'
  expect_identical(e$x, 3)
  rollback(e) <- 2
  expect_equal(e$x, 1) # The environment should have been rolled back.
})

test_that('it can roll back to the initial environment', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  expect_identical(e$x, 1)
  rollback(e) <- 1
  expect_equal(e$x, NULL) # The environment should have been rolled back.
})

test_that('it errors when rolling back more than the number of commits', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  expect_identical(e$x, 1)
  expect_error(rollback(e) <- 2, "because only 1 commits have been made")
})

test_that('it can roll back twice successfully', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  e$x <- 2
  commit(e) <- 'Second message'
  rollback(e) <- 1
  e$x <- 3
  commit(e) <- 'Third message'
  e$x <- 4
  commit(e) <- 'Fourth message'
  rollback(e) <- 1
  expect_identical(e$x, 3)
})

test_that('it can roll back changes to a dataframe', {
  e <- tracked_environment()
  e$x <- iris
  commit(e) <- 'First message'
  e$x[1, 1] <- 1
  commit(e) <- 'Second message'
  e$x[1, 1] <- 2
  commit(e) <- 'Third message'
  rollback(e) <- 1
  expect_equal(e$x[1, 1], 1)
})

test_that('it can revert from a snapshot', {
  e <- tracked_environment(snapshot = 2)
  e$x <- iris
  commit(e) <- 'First message'; e$x[1, 1] <- 1
  commit(e) <- 'Second message'; e$x[1, 1] <- 2
  commit(e) <- 'Third message'; e$x[1, 1] <- 3
  commit(e) <- 'Fourth message'; e$x[1, 1] <- 4
  commit(e) <- 'Fifth message'
  expect_equal(length(e%$%reference), 3) # 2 snapshots
  rollback(e) <- 1
  expect_equal(length(e%$%reference), 2) # 1 snapshot
})

# tracked_environments also support "silent rollbacks", wherein
# a previous commit is restored, but the commit history is not 
# truncated. This means we can "peek" backwards in time, then
# rollback to our current state. For example, if we run
#   env <- tracked_environment()
#   env$x <- 1; commit(env) <- 'first'
#   env$y <- 2; commit(env) <- 'second'
#   rollback(env, silent = TRUE) <- 1
#   # We rolled back one commit and now 'y' does not exist
#   stopifnot(is.null(env$y))
#   rollback(env) <- -1 # a "roll forward"
#   stopifnot(identical(env$y, 2))
describe("silent rollbacks", {
  test_that("it can entertain the silent = TRUE parameter", {
    env <- tracked_environment()
    env$x <- 1; commit(env) <- 'first'
    env$y <- 2; commit(env) <- 'second'
    rollback(env, silent = TRUE) <- 1
  })

  test_that("it can it can roll back, then forward!", {
    env <- tracked_environment()
    env$x <- 1; commit(env) <- 'first'
    env$y <- 2; commit(env) <- 'second'
    rollback(env, silent = TRUE) <- 1
    expect_null(env$y)
  })
})

