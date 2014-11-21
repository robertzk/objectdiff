context('rollback')

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
  expect_error(rollback(e) <- 2)
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

