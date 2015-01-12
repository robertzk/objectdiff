context('force_push')

test_that('it can force push to the zeroth commit', {
  env <- tracked_environment()
  force_push(env, 0)
})

