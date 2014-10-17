library(microbenchmark)
# How long does it take to find an object's size?

num <- seq_len(1000000)
bigdf <- do.call(rbind, rep(list(iris), 50))
bigdf <- do.call(cbind, rep(list(bigdf), 50))
bigfn <- function(...) { x <- 1}
for (i in seq_len(1000)) body(bigfn)[[i + 2]] <- bquote(x[.(i)] <- .(i))
char <- sample(letters, 1000000, replace = TRUE)
lst <- as.list(num)
nested_list <- replicate(1000, lst[seq_len(1000)])
super_nested_list <- Reduce(function(x, .) list(x), seq_len(1000), list(NULL))

object.size <- function(...) UseMethod('object.size')
object.size.default <- function(...) utils::object.size(...)
object.size.environment <-
  function(x) sum(vapply(unlist(lapply(x, object.size)), as.integer, integer(1)))

print(microbenchmark(object.size(num), object.size(bigdf), object.size(bigfn),
  object.size(char), object.size(lst), object.size(nested_list),
  estimate_size(lst), object.size(super_nested_list)))
print(microbenchmark(object.size(environment()), times = 1))

# Unit: microseconds
#                            expr       min         lq     median         uq       max neval
#                object.size(num)    31.629    45.7370    90.3835   129.5110   595.814   100
#              object.size(bigdf)    50.019    85.9525   133.0395   171.3570   414.740   100
#              object.size(bigfn)   205.099   325.3645   392.2870   480.2070  4682.475   100
#               object.size(char) 10581.024 12511.9535 14627.0730 19882.3830 50620.039   100
#                object.size(lst) 15291.786 17430.9850 19299.4725 22209.1010 36402.859   100
#        object.size(nested_list) 15887.741 17542.6460 19304.4140 21717.4610 40449.597   100
#  object.size(super_nested_list)    49.855    78.8025   139.3085   175.9745   507.382   100
# Unit: milliseconds
#                        expr      min       lq   median       uq      max neval
#  object.size(environment()) 153.9532 153.9532 153.9532 153.9532 153.9532     1
