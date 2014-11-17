#' An environment object that tracks changes made to its constituents.
#' 
#' A typical R environment is like a mathematical set. It is a bag of 
#' stuff and not much else. A tracked environment is like such an environment
#' that can also replay "changes" to its history.
#'
#' To create a tracked environment from any environment \code{e}, simply write
#' \code{tracked_environment(e)}.
#' 
tracked_environment <- function(env) {

}

`$<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  assign_call <- quote(`$<-`(env$env, name, value))
  assign_call[[3]] <- as.name(substitute(name))
  eval(assign_call)
  env
}

`[[<-.tracked_environment` <- function(env, name, value) {
  tmp <- class(env)
  `[[<-`(env$env, name, value)
  env
}






