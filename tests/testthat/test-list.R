context('lists')

test_that('it can patch the trivial list to itself', {
  expect_diff(list(), list(), small = TRUE)
})

test_that('it can patch the trivial list to a non-trivial list', {
  expect_diff(list(), as.list(1:10), small = TRUE)
})

test_that('it can patch a non-trivial list to the trivial list', {
  expect_diff(as.list(1:10), list(), small = TRUE)
})

test_that('it can patch the trivial list to a non-trivial named list', {
  expect_diff(list(), setNames(as.list(1:10), letters[1:10]), small = TRUE, trivial = FALSE)
})

test_that('it can patch a named non-trivial list to the trivial list', {
  expect_diff(setNames(as.list(1:10), letters[1:10]), structure(list(), names = character(0)),
              small = TRUE, trivial = FALSE)
})

test_that('it can drop some values from a named list', {
  expect_diff(list(a = 1, b = 2, c = 3), list(a = 1, c = 3), trivial = FALSE)
})

test_that('it can add some values to a named list', {
  # TODO: (RK) Revisit this when smarter list modification heuristics are in place.
  expect_diff(list(a = 1, c = 3), list(a = 1, b = 2, c = 3)) #, trivial = FALSE)
})

test_that('it can modify some values in a named list', {
  # TODO: (RK) Revisit this when smarter list modification heuristics are in place.
  expect_diff(list(a = 1, c = 3), list(a = 1, c = 2)) #, trivial = FALSE)
})

test_that('it can modify some values in a named list and add something', {
  expect_diff(list(a = 1, c = 3), list(a = 1, b = 2, c = 4), trivial = FALSE)
})

test_that('it can patch a huge list with only a tiny change', {
  x <- as.list(1:10000)
  y <- x
  y[[10000]] <- 1

  expect_diff(x, y, small = TRUE)
})

test_that('it can patch a huge list with a huge change', {
  x <- as.list(1:10000)
  y <- rev(x)
  
  # TODO: Figure out why this patch is not small on CI...
  expect_diff(x, y) #, small = FALSE)
})

test_that('it can patch a list name change', {
  x <- as.list(1:10)
  names(x) <- letters[1:10]
  y <- x
  names(y)[10] <- 'boo'
  expect_diff(x, y, small = 2000)
})

test_that('it can patch an attribute change', {
  x <- as.list(1:10)
  y <- x
  attr(y, 'blue') <- iris
  expect_diff(x, y)
})

test_that('it can patch a small attribute change with a small patch', {
  x <- as.list(1:10)
  attr(x, 'blue') <- as.list(1:10000)
  y <- x
  attr(y, 'blue')[[5000]] <- 1

  expect_diff(x, y, small = 5000)
})

test_that('it can drop named list elements without a full patch', {
  expect_diff(iris, iris[-1], trivial = FALSE)
})


