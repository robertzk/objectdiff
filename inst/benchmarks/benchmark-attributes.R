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


#
# What if two things are equal except for attributes and class?
# We need some ways to check for that.

unat <- function(y) { attributes(y) <- NULL; y }
microbenchmark(all.equal(x,y), identical(x, y), identical(head(x,-1), head(y,-1)),
               identical(unat(x), unat(y)))

# Unit: nanoseconds
#                                 expr      min         lq     median         uq       max neval
#                      all.equal(x, y)   100956   225546.0   263571.5   293590.5    662754   100
#                      identical(x, y)      901     3331.5     7947.5    10311.0     25113   100
#  identical(head(x, -1), head(y, -1)) 12927407 16399260.0 23043983.0 28436341.5 218761241   100
#          identical(unat(x), unat(y))  1835558  2348825.5  2511917.0  2936794.5  22979307   100

# For character comparison, it takes 4x longer
#          identical(unat(x), unat(y))  8352153  9224829.0 10499011.0 14747340.0 213376807   100
# 

# Maybe a good strategy then is to first sample 100 indices and see
# if they match before doing the full comparison.

smp <- sample(seq_len(1000000), 100)

microbenchmark(all.equal(unclass(x), unclass(y), check.attributes = FALSE),
               for(i in smp) identical(.subset2(x, i), .subset2(y, i)))

# Unit: microseconds
#                                                        expr       min        lq     median uq         max        neval
# all.equal(unclass(x), unclass(y), check.attributes = FALSE) 31587.659 37940.179 44013.4875 53668.0470 239773.415 100
#    for (i in smp) identical(.subset2(x, i), .subset2(y, i))   146.883   196.441   228.7455 300.8225   500.595    100
#                                   identical(x[smp], y[smp])     7.482    11.211    43.934    55.687   200.682    100





# It seems like temporarily removing their attributes may be necessary.

# It seems 
# Once we know that two things are identical
# we need to create a patch from one onto the other. It seems like the fastest way
# to "drop" any 
