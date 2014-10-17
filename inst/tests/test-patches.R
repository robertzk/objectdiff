context('patches')

test_that('the identity patch has nothing in its environment', {
  expect_identical(environment(identity_patch()), emptyenv())
})

test_that('the identity patch patches a simple example correctly', {
  for (obj in list(iris, 1:10, "X", function(x) y, as.environment(list(x = 1, y = 2))))
    expect_identical(identity_patch()(obj), obj)
})

test_that('the trivial patch patches a simple example correctly', {
  for (obj in list(iris, 1:10, "X", function(x) y, as.environment(list(x = 1, y = 2))))
    expect_identical(trivial_patch("the real deal")(obj), "the real deal")
})

test_that('the atomic differences patch correctly does nothing on no change', {
  expect_identical(atomic_differences_patch(1:10, 1:10)(1:10), 1:10)
})

test_that('the atomic differences patch correctly changes one element only', {
  x <- seq_len(1000); y <- x; y[1] <- 5
  expect_identical(atomic_differences_patch(x, y)(x), y)
})

test_that('the atomic differences patch correctly changes one element only on small vector', {
  x <- seq_len(10); y <- x; y[1] <- 5
  expect_identical(atomic_differences_patch(x, y)(x), y)
})

test_that('the atomic differences patch patches an identical vector with diff attributes', {
  x <- seq_len(1000); y <- x; attr(y, 'blah') <- iris; class(y) <- c('foo', class(y))
  expect_identical(atomic_differences_patch(x, y)(x), y)
})

test_that('the atomic differences patch patches a similar vector with diff attributes', {
  x <- seq_len(1000); y <- x; y[1] <- 5; attr(y, 'blah') <- iris; class(y) <- c('foo', class(y))
  expect_identical(atomic_differences_patch(x, y)(x), y)
})

test_that('the atomic differences patch patches a similar vector with diff attributes without much memory use', {
  x <- seq_len(10000); y <- x; y[1] <- 5; attr(y, 'blah') <- 'arr'; class(y) <- c('foo', class(y))
  expect_less_than(as.integer(object.size(atomic_differences_patch(x, y))), 5000)
})

test_that('the atomic differences patch changes character vectors', {
  x <- sample(letters, 1000000, replace = TRUE)
  y <- x; y[1] <- 'z'; attr(y, 'blah') <- 'arr'; class(y) <- c('foo', class(y))
  expect_identical(atomic_differences_patch(x, y)(x), y)
})

test_that('the atomic differences patch changes character vectors with a small memory footprint', {
  x <- sample(letters, 1000000, replace = TRUE)
  y <- x; y[1] <- 'z'; attr(y, 'blah') <- 'arr'; class(y) <- c('foo', class(y))
  expect_less_than(as.integer(object.size(atomic_differences_patch(x, y))), 5000)
})

test_that('the atomic differneces patch changes logical vectors', {
  x <- as.logical(rep(c(0, 1), 500000))
  y <- x; y[1] <- TRUE; attr(y, 'blah') <- 'arr'; class(y) <- c('foo', class(y))
  expect_identical(atomic_differences_patch(x, y)(x), y)
})
