# A test helper for comparing patched to actual.
expect_diff <- function(x, y, small, trivial, identity) {
  testthat::expect_identical((patch <- objectdiff(x, y))(x), y)
  
  if (!missing(small)) {
    if (isTRUE(small)) small <- 1000

    if (identical(small, FALSE)) {
      testthat::expect_more_than(object.size(environment(patch)), 10000)
    } else {
      stopifnot(is.numeric(small))
      testthat::expect_less_than(object.size(environment(patch)), small)
    }
  }

  if (!missing(trivial)) {
    if (isTRUE(trivial)) {
      testthat::expect_true(is.trivial_patch(patch))
    } else if (identical(FALSE, trivial)) {
      testthat::expect_false(is.trivial_patch(patch))
    }
  }

  if (!missing(identity)) {
    if (isTRUE(identity)) {
      testthat::expect_true(is.identity_patch(patch))
    } else if (identical(FALSE, identity)) {
      testthat::expect_false(is.identity_patch(patch))
    }
  }
}
