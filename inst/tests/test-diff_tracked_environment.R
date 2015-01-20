context('objectdiff/tracked_environment')

test_that('it can perform a diff on no changes', {
  x <- tracked_environment()
  patch <- objectdiff(x, x) 

  y <- tracked_environment(list2env(lst <- list(x = 1, y = 2)))
  y <- patch(y)
  expect_true(setequal(as.list(environment(y)), lst))
})

test_that('it can calculate removals', {
  x <- tracked_environment(list2env(list(x = 1)))
  rm('x', envir = x)
  patch <- objectdiff(x, x)

  y <- tracked_environment(list2env(list(x = 1)))
  patch(y)
  expect_is(y, 'tracked_environment')
  expect_null(y$x)
})

test_that('it can calculate additions', {
  x <- tracked_environment()
  x$x <- 1
  patch <- objectdiff(x, x)

  y <- tracked_environment()
  patch(y)
  expect_is(y, 'tracked_environment')
  expect_equal(y$x, 1)
})

# TODO: (RK) Add way more!


