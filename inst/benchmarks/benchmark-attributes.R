# How long does it take to compare attributes?
library(microbenchmark)

y <- sample(seq_len(1000000)); class(y) <- c('foo', class(y));
names(y) <- sample(letters, length(y), replace = TRUE)
z <- sample(seq_len(1000000)); class(z) <- c('foo', class(z));
names(z) <- names(y)

k <- names(y)
r <- names(z)

print(microbenchmark(
  "base::identical on attributes" = identical(attributes(y), attributes(z)),
  "base::identical on attributes with cheap mode" = identical(attributes(y), attributes(z),F,F,F,F),
  "base::identical on 1 name" = identical(.subset2(names(y), 1), .subset2(names(z), 1)),
  "1 million chars" = identical(k, r),
  "get attributes" = attributes(y)
))

w1 <- sample(seq_len(1000000)); z1 <- w1; z1[1000000] <- -1
w2 <- sample(letters, 1000000, replace = TRUE); z2 <- w2; z2[1000000] <- 'no'
print(microbenchmark(
  "1 million numerics" = identical(w1, z1),
  "1 million chars" = identical(w2, z2)
))
# Lesson: Comparing characters is *really* slow

# Unit: nanoseconds
#                                           expr     min        lq    median        uq      max neval
#                  base::identical on attributes 3232790 3468022.5 3744298.0 4768320.5  7028641   100
#  base::identical on attributes with cheap mode 3255716 3437544.5 3640882.5 4782078.0 13316575   100
#                      base::identical on 1 name    7229   11389.0   48779.5   57523.5    90371   100
#                                1 million chars 3221980 3542708.0 3888966.0 4686419.0  9568459   100
#                                 get attributes     342    1606.5    4148.0    5723.5    10847   100
# Unit: nanoseconds
#                expr     min        lq    median      uq     max neval
#  1 million numerics     828    1401.5    6138.5    7940   30903   100
#     1 million chars 3239020 3385422.5 3571055.5 4357100 6963236   100

