`$<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  class(env) <- NULL
  assign_call <- quote(`$<-`(env, name, value))
  assign_call[[3]] <- as.name(substitute(name))
  eval(assign_call)
  class(env) <- tmp
  env
}

`[[<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  class(env) <- NULL
  `[[<-`(env, name, value)
  class(env) <- tmp
  env
}




