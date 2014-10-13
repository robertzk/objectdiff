# What is the fastest way to tell if two objects are identical?

library(microbenchmark)

# Note: base::identical uses memcmp under the hood for atomic comparisons,
# which is *very* fast!
# https://github.com/SurajGupta/r-source/blob/46102b91b35a7befa0cf6cc6abd0da09b86f1621/src/main/identical.c#L160

x <- sample(seq_len(1000000)); x2 <- x
y <- runif(1000000, 0, 1); y2 <- y

print(microbenchmark(
  "base::identical on numerics" = identical(y, y2),
  "base::identical on integers" = identical(x, x2)
))

# Note the max below is artificial and a result of garbage collection.
#
# Unit: nanoseconds
#                         expr min    lq median   uq   max neval
#  base::identical on integers 804 905.5  958.0 1062 12934   100
#  base::identical on numerics 812 906.0  964.5 1058  2669   100
#

