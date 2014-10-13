library(microbenchmark)

# Usually, if two object are *almost* identical, this means their attributes
# or names differ. Here we investigate what the fastest way is to get
# the "bare bones" version of an object.

y <- sample(seq_len(1000000)); class(y) <- c('foo', class(y));
names(y) <- letters[1:10]

undress1 <- function(obj) {
  attributes(obj) <- NULL
  unname(obj)
}

undress2 <- function(obj) {
  #for (i in names(attributes(obj))) attr(obj, i) <- NULL
  class(obj) <- NULL
  obj
  #unname(obj)
}

copy <- function(obj) {
  new_obj <- numeric(length(obj))
  new_obj
}

print(microbenchmark(
  "Remove names and attributes directly" = undress1(y),
  "Remove names and attributes iteratively" = undress2(y),
  "Allocate memory for copy" = copy(y), # Even allocating memory is expensive,
    # we won't even bother trying to copy which takes 100x longer.
  "Call as.numeric" = as.numeric(y)
, times = 5))


# Unit: milliseconds
#                                     expr      min       lq    median        uq       max neval
#     Remove names and attributes directly 3.423476 3.997793  4.256173  4.416947  4.570352     5
#  Remove names and attributes iteratively 3.654168 3.778606  4.987407  5.493629  5.664423     5
#                 Allocate memory for copy 1.252063 1.355467  1.507160  1.960974 13.631622     5
#                          Call as.numeric 4.386874 5.420174 19.486084 19.891346 20.759525     5
# Not very good! We need to find another way... 

