context('s3 objects')

test_that('it can diff a trivial s3 object', {
  x <- list(1)
  class(x) <- 'boo'
  y <- x
  x[[1]] <- 2
  expect_diff(x, y)
})

test_that('it can diff an lm object', {
  lmo <- lm(Sepal.Width ~ Sepal.Length, iris)
  iris2 <- iris; iris2[1, 1] <- 7
  lmo2 <- lm(Sepal.Width ~ Sepal.Length, iris2)
  expect_diff(lmo, lmo2)
})

local({
  some_gbm_data <- function(N = 1000) {
    # Drawn from the documentation for ?gbm
    X1 <- runif(N)
    X2 <- 2*runif(N)
    X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
    X4 <- factor(sample(letters[1:6],N,replace=TRUE))
    X5 <- factor(sample(letters[1:3],N,replace=TRUE))
    X6 <- 3*runif(N)
    mu <- c(-1,0,1,2)[as.numeric(X3)]

    SNR <- 10 # signal-to-noise ratio
    Y <- X1**1.5 + 2 * (X2**.5) + mu
    sigma <- sqrt(var(Y)/SNR)
    Y <- Y + rnorm(N,0,sigma)

    # introduce some missing values
    X1[sample(1:N,size=500)] <- NA
    X4[sample(1:N,size=300)] <- NA

    data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
  }

  run_gbm <- function(data) {
    # Drawn from the documentation for ?gbm
    gbm::gbm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,
      data = data,                 # dataset
      var.monotone = rep(0, 6),    # -1: monotone decrease,
                                   # +1: monotone increase,
                                   #  0: no monotone restrictions
      distribution = "gaussian",   # see the help for other choices
      n.trees = 1000,              # number of trees
      shrinkage = 0.05,            # shrinkage or learning rate,
                                   # 0.001 to 0.1 usually work
      interaction.depth = 3,       # 1: additive model, 2: two-way interactions, etc.
      bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
      train.fraction = 0.5,        # fraction of data for training,
                                   # first train.fraction*N used for training
      n.minobsinnode = 10,         # minimum total weight needed in each node
      cv.folds = 3,                # do 3-fold cross-validation
      keep.data = TRUE,            # keep a copy of the dataset with the object
      verbose = FALSE,             # don't print out progress
      n.cores = 1                  # use only a single core (detecting #cores is
    )                              # error-prone, so avoided here)
  }

  test_that('it can diff a GBM object', {

    gbmo  <- run_gbm(some_gbm_data())
    gbmo2 <- run_gbm(some_gbm_data())

    expect_diff(gbmo, gbmo2)
  })

})

