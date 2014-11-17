#' An environment object that tracks changes made to its constituents.
#' 
#' A typical R environment is like a mathematical set. It is a bag of 
#' stuff and not much else. A tracked environment is like such an environment
#' that can also replay "changes" to its history.
#'
#' To create a tracked environment from any environment \code{e}, simply write
#' \code{tracked_environment(e)}.
#' 
#' @param env environment. When converted to a \code{tracked_environment},
#'   all changes will be remembered whenever a "commit" is registered
#'   on the environment. Commits can be named and labeled.
#' @examples
#' \dontrun{
#'   e <- tracked_environment()
#' }
tracked_environment <- function(env) {
  structure(list(env = env), class = 'tracked_environment')
}

as.environment.tracked_environment <- function(env) { env$env }
environment <- function(...) UseMethod('environment')
environment.tracked_environment <- as.environment.tracked_environment 
`environment.tracked_environment<-` <- function(tracked_env, env) {
  tracked_env$env <- env
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


