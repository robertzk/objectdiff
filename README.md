Git-like object patching for R [![Build Status](https://travis-ci.org/robertzk/objectdiff.svg?branch=master)](https://travis-ci.org/robertzk/objectdiff)
==========

Intended to be used in conjunction with caching [stagerunners](http://github.com/robertzk/stagerunner),
the objectdiff package helps keep the recording of many operations on a data set concise.
If you are munging a data set, it would be ideal to record the progress so you can "rewind"
and interactively debug what happened in the past if you ever end up with a data set
that is either incorrect or malformed, without incurring too much memory overhead.

Objectdiff solves this problem by providing a mechanism of "patching" arbitrary R objects
to turn them into other ones. For example,

```R
iris2 <- iris[, -1]
iris2[[1]] <- as.character(iris2[[1]])
patch <- objectdiff(iris, iris2)
stopifnot(identical(iris2, patch(iris)))
```

In this example, `patch` is a function we can apply to the iris dataset to turn it into iris2.
The point here is that instead of storing *both* iris and iris2, which could 
get memory-prohibitive very quickly, we only need to store iris and its patch
(or sequence of patches).

If we applied ten different manipulations to a dataset, we could keep a history of our changes
by storing the successive patches in conjunction with the initial data set. In essence,
objectdiff aims to be a "Git for R data."

Tracked environments
------------

Hand in hand with patching R objects is the idea of tracking all changes to
an [environment](http://adv-r.had.co.nz/Environments.html). For this purpose,
objectdiff provides a `tracked_environment` object. Consider the 
following example.

```r
e <- tracked_environment(new.env())
e$x <- 1
commit(e) <- 'First commit' # Store changes -- NULL for no commit message
e$x <- 2
commit(e) <- 'Second commit'
e$x <- 3
commit(e) <- 'Third commit'
print(e$x)
# [1] 3
rollback(e) <- 2 # Roll back two commits
print(e$x)
# [1] 1 # Back to the first commit! This environment remembers changes.
```

Installation
------------

This package is not yet available from CRAN (as of Oct 12, 2014).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/objectdiff")
```


