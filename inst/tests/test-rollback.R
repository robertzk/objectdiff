context('rollback')

test_that('it can roll back a simple example', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  e$x <- 2
  commit(e) <- 'Second message'
  stopifnot(identical(e$x, 2))
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
  stopifnot(identical(e$x, 3))
  rollback(e) <- 2
  expect_equal(e$x, 1) # The environment should have been rolled back.
})

test_that('it can roll back to the initial environment', {
  e <- tracked_environment()
  e$x <- 1
  commit(e) <- 'First message'
  stopifnot(identical(e$x, 1))
  rollback(e) <- 1
  expect_equal(e$x, NULL) # The environment should have been rolled back.
})

