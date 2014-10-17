# Sample with replace is 1000 times faster since the object doesn't
# have to be progressively culled!
library(microbenchmark)

x <- sample(seq_len(1000000))
microbenchmark(sample(x, 100), sample(x, 100, replace = TRUE))

#Unit: microseconds
#                           expr     min       lq   median       uq       max neval
#                 sample(x, 100) 447.352 762.5535 1159.427 2186.754 36640.108   100
# sample(x, 100, replace = TRUE)  17.139  19.7140   30.376   53.205   133.025   100

# It shouldn't be called sampling with or without replacement, but sampling
# with or without removal!

