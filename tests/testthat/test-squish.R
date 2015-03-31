test_that('it can squish a trivial example', {
  expect_identical(squish_patches(NULL, list()), identity_patch(),
    info = 'squishing no patches should just give the identity patch')
})

test_that('it can squish one patch into itself', {
  iris2 <- iris
  iris2[1, 1] <- NA
  patch <- objectdiff(iris, iris2)
  expect_equal(squish_patches(iris, list(patch)), patch,
               info = 'one patch should squish into itself')
})

test_that('it can squish several patches into one', {
  iris2 <- iris; iris2[1, 1] <- 1
  patch1 <- objectdiff(iris, iris2)
  iris3 <- iris2; iris3[1, 1] <- 2
  patch2 <- objectdiff(iris2, iris3)
  iris4 <- iris3; iris3[1, 1] <- 3
  patch3 <- objectdiff(iris3, iris4)
  patchouli <- squish_patches(iris, list(patch1, patch2, patch3))
  expect_identical(patchouli(iris), iris4)
})

