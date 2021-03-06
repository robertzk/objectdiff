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
#'   on the environment. Commits can be named and labeled. The default is
#'   \code{new.env(parent = emptyenv())} (a new environment with no parent).
#' @param snapshot integer. The interval at which to snapshot the environment.
#'   For example, if 100 commits have been made, and you would like to go
#'   back to commit 95, it would be very time-consuming to apply all
#'   the commits starting from the beginning. Instead a full copy
#'   of the environment will be made every \code{snapshot} commits.
#' @rdname tracked_environment
#' @export
#' @examples
#' e <- tracked_environment()
#' e$x <- 1
#' commit(e) <- 'First message'
#' e$x <- 2
#' commit(e) <- 'Second message'
#' stopifnot(identical(e$x, 2))
#' rollback(e) <- 1
#' stopifnot(identical(e$x, 1)) # The changes have been rolled back one step.
#'
#' classical_env <- list2env(list(x = 1, y = 2))
#' e <- tracked_environment(classical_env, snapshot = 5)
#' # Any changes to e will record full snapshots of the environment
#' # every 5 commits. This way, when the environment is rolled back to an
#' # earlier commit, it will not have to apply patches starting from the
#' # very beginning.
tracked_environment <- function(env = new.env(parent = emptyenv()), snapshot = 10) {
  force(env)
  stopifnot(is.environment(env))
  if (is.tracked_environment(env)) {
    stop("Recursion! Can't track an already-tracked environment.")
  }

  initial <- new.env(parent = emptyenv())
  if (length(env) > 0L) {
    copy_env(initial, env)
  }

  structure(class = c('tracked_environment', 'environment'),
            list2env(parent = emptyenv(),
    list(reference = list(initial),
         env = env,
         ghost = new.env(parent = emptyenv()),
         universe = ls(env, all = TRUE),
         commits = make_stack(),
         snapshot = snapshot)
  ))
}
#' @export
setClass('tracked_environment')

#' @export
ls <- function(name, ...) UseMethod('ls')
#' @method ls tracked_environment
#' @export
ls.tracked_environment <- function(name, ...) base::ls(environment(name), ...)
#' @export
ls.default <- function(...) {
  call <- match.call()
  call[[1]] <- base::ls
  eval.parent(call)
}

#' @export
rm <- function(..., envir = as.environment(-1)) {
  if (is.tracked_environment(envir)) {
    eval.parent(substitute(base::rm(..., envir = base::get("env", envir))))
  } else {
    eval.parent(substitute(base::rm(..., envir = envir)))
  }
}

#' @export
exists <- function(x, where = -1, envir = as.environment(-1), frame, mode = "any", inherits = TRUE) {
  base::exists(x, where,
               if (is.tracked_environment(envir)) environment(envir) else envir,
               frame, mode, inherits)
}

#' @export
as.environment <- function(...) UseMethod('as.environment')
#' @method as.environment tracked_environment
#' @export
as.environment.tracked_environment <- function(env) { env%$%env }
#' @method as.environment environment
#' @export
as.environment.environment <- function(...) ..1
#' @export
as.environment.default <-
 function(...) base::as.environment(...)

#' @export
environment <- function(fun) UseMethod('environment')
#' @method environment function
#' @export
environment.function <- function(fun) base::environment(fun)
#' @export
environment.default <- function(fun) {
  call <- match.call()
  call[[1]] <- base::environment
  eval.parent(call)
}
#' @method environment tracked_environment
#' @export
environment.tracked_environment <- function(fun) { as.environment(fun) }
#' @export
is.tracked_environment <- function(x) { is(x, 'tracked_environment') }

#' @export
`environment<-` <- function(env, value) UseMethod('environment<-')
#' @method environment<- function
#' @export
#' @export
`environment<-.function` <- function(env, value) base::`environment<-`(env, value)
#' @method environment<- character
#' @export
`environment<-.character` <- function(env, value) base::`environment<-`(env, value)
#' @export
`environment<-.default` <- function(env, value) base::`environment<-`(env, value)
#' @method environment<- tracked_environment
#' @export
`environment<-.tracked_environment` <- function(env, value) {
  stop("Cannot assign environment of a tracked_environment directly.")
}

#' @export
`parent.env` <- function(env) UseMethod("parent.env")
#' @export
`parent.env.default` <- function(env) base::`parent.env`(env)
#' @method parent.env tracked_environment
#' @export
`parent.env.tracked_environment` <- function(env) { base::`parent.env`(env%$%env) }
#' @export
`parent.env<-` <- function(env, value) UseMethod("parent.env<-")
#' @export
`parent.env<-.default` <- function(env, value) base::`parent.env<-`(env, value)
#' @method parent.env<- tracked_environment
#' @export
`parent.env<-.tracked_environment` <- function(env, value) {
  base::`parent.env<-`(env%$%env, value)
  env
}

#' @export
`commit<-` <- function(env, value) { UseMethod('commit<-') }
#' @export
`rollback<-` <- function(env, silent = FALSE, value) { UseMethod('rollback<-') }

#' Commit a change to a tracked environment.
#'
#' Committing a change is equivalent to storing a patch object (see
#' \code{\link{objectdiff}}). For the moment, any comment will produce
#' a single patch by "squishing" any changes made to the environment.
#' If you are iterating over an environment object many many times,
#' you should probably batch changes if it lives in a tracked environment.
#' Otherwise, for example, making 1,000,000 small modifications to
#' a data.frame before commiting could cause a long time to recompute
#' the "final patch".
#'
#' @seealso \code{\link{objectdiff}})
#' @param env tracked_environment.
#' @param value character. Commit message. May be \code{NULL}.
#' @rdname commit
#' @method commit<- tracked_environment
#' @export
#' @examples
#' x <- tracked_environment()
#' x$foo <- 1
#' commit(x) <- 'First message'
#' x$foo <- 2
`commit<-.tracked_environment` <- function(env, value) {
  (env%$%commits)$push(setNames(list(objectdiff(env, env)), value))

  if (`need_snapshot?`(env)) {
    `snapshot!`(env)
  }

  `reset_environment!`(env)
}

#' @rdname commit
#' @export
commit <- function(env, value = NULL) { commit(env) <- value }

#' A named list of all commits.
#'
#' @param env tracked_environment.
#' @export
#' @return a named list of all commits, with each name corresponding
#'   to the commit message, and each value a \code{patch} object
#'   (a function that you can apply to a tracked environment to
#'   perform that commit).
commits <- function(env) {
  Reduce(append, (env%$%commits)$peek_all())
}

#' Roll back commits to an earlier version of the tracked environment.
#'
#' @export
#' @rdname rollback
#' @param env tracked_environment.
#' @param silent logical. Whether or not to commit a silent rollback.
#'   If \code{TRUE}, the current chain of commits will not be pruned,
#'   so it will be possible to use \code{rollback} with a negative
#'   number to go back to a future commit. It is the user's responsibility
#'   to ensure that the commit stack does not become corrupt. 
#' @note Rolling back 0 commits clears the current staged changes.
#' @method rollback<- tracked_environment
#' @param value integer. Number of commits to roll back.
`rollback<-.tracked_environment` <- function(env, silent = FALSE, value) {
  stopifnot(is.numeric(value))
  head_commit <- (env%$%commits)$head()
  num_commits <- (env%$%commits)$count()

  replay_count <- head_commit - value
  if (replay_count < 0) {
    stop("Cannot rollback ", value, " commits ",
         "because only ", head_commit, " commits have been made (relative ",
         "to the current head).")
  } else if (replay_count > num_commits) {
    stop("Cannot rollforward ", -value, " commits ",
         "because only ", num_commits, " commits have been made in total.")
  }

  replay(env, replay_count, silent = silent)
}

#' @rdname rollback
#' @export
rollback <- function(env, value = 1, silent = FALSE) {
  rollback(env, silent = silent) <- value
}

#' Force push a tracked environment to a given commit.
#'
#' Forcing pushing means restoring a tracked environment to what it looked like
#' as of that commit. You can force push with either the commit index or
#' the name of the commit.
#'
#' @param env tracked_environment. 
#' @param commit integer or character. If character, the commit with this
#'   name will be attempted for the force push. If there are multiple commits
#'   with this same name, a warning will be issued.
#' @export
#' @examples
#' env <- tracked_environment()
#' env$x <- 1
#' commit(env) <- 'first commit'
#' env$y <- 2
#' commit(env) <- 'second commit'
#' force_push(env, 'first commit') # equivalent to force_push(env, 1)
#' stopifnot(identical(as.list(environment(env)), list(x = 1)))
force_push <- function(env, commit) {
  stopifnot(is.tracked_environment(env))
  if (length(commit) != 1) { stop(dQuote("commit"), " argument must be of length 1") }

  if (is.numeric(commit)) {
    stopifnot(commit >= 0)
    stopifnot(commit <= (env%$%commits)$count())
  } else if (is.character(commit)) {
    index <- which(vapply((env%$%commits)$peek_all(), names, character(1)) == commit)
    if (length(index) > 1) {
      warning(call. = FALSE, "Multiple commits match name ", sQuote(commit))
    } else if (length(index) == 0) {
      stop("There is no commit with name ", sQuote(commit))
    }

    commit <- index[1]
  } else {
    stop(dQuote("commit"), " argument must be of type numeric or character")
  }

  replay(env, commit, silent = TRUE) # force pushing is silent by default
}

#' @rdname tracked_environment
#' @param name character. When using the \code{\%$\%} infix operator,
#'    access a meta-datum from the \code{tracked_environment} (for example,
#'    "env", "reference", "ghost", "universe", "commits", or "snapshot").
#' @note
#' A tracked_environment is itself an environment that contains
#' \itemize{
#'   \item{\code{env}. }{The environment that is getting tracked.}
#'   \item{\code{reference}. }{When the first commit is published, a full
#'     copy of the original environment gets saved so that it can be
#'     replayed during rollbacks. Any additional snapshots (i.e., full
#'     copies of the environment) will be appended to this list at the
#'     \code{snapshot} interval.}
#'   \item{\code{ghost}. }{An environment that holds the "before" version
#'     of objects prior to committing a change. When a
#'     \code{tracked_environment} receives a commit, it will clear
#'     its ghost environment.}
#'   \item{\code{universe}. }{Essentially just running \code{base::ls} (i.e.,
#'     fetching the names of all objects in) the \code{env} before any
#'     changes occur. This is re-computed after a commit or rollback.}
#'   \item{\code{commits}. }{A list of commits (a curated list of \code{patch}es
#'     that represent the history of the \code{tracked_environment})}.
#'   \item{\code{snapshot}. }{The integer number of commits to wait before
#'     recording a full copy of the environment for rollbacks and for
#'     peeking back to past commits.}
#' }
#' 
#' From within the objectdiff package, it is possible to access these
#' explicitly using the \code{\%$\%} operator, for example, 
#' \code{some_tracked_env\%$\%commits}.
#'
#' @export
`%$%` <- function(env, name) {
  get(deparse(substitute(name)), envir = env, inherits = FALSE, mode = 'meta')
}

#' @export
`%$%<-` <- function(env, name, value) {
  stopifnot(is.tracked_environment(env))

  base::assign(deparse(substitute(name)), value, envir = env, inherits = FALSE)
  env
}

#' @export
get <- function(x, pos = -1, envir = as.environment(pos), mode = "any", inherits = TRUE) {
  "This function has been overwritten by the objectdiff package."
  "For the base R function, type base::get (or ?base::get to see documentation)."

  if (is.tracked_environment(envir)) {
    if (identical(mode, 'meta')) {
      base::get(x, pos, envir, "any", inherits)
    } else {
      base::get(x, pos, envir%$%env, mode, inherits)
    }
  } else {
    call <- sys.call()
    call[[1]] <- quote(base::get)
    eval.parent(call)
  }
}

#' @method [[ tracked_environment
#' @export
`[[.tracked_environment` <- function(env, name) {
  if (exists(name, envir = environment(env), inherits = FALSE))
    base::get(name, envir = environment(env))
  else NULL
}

#' @method $ tracked_environment
#' @export
`$.tracked_environment` <- `[[.tracked_environment`

#' @method $<- tracked_environment
#' @export
`$<-.tracked_environment` <- function(env, name, value) {
  assign_call <- quote(`[[<-`(env, name, value))
  assign_call[[3]] <- substitute(name)
  eval(assign_call)
  env
}

#' @method [[<- tracked_environment
#' @export
`[[<-.tracked_environment` <- function(env, name, value) {
  # Record the before-value in the ghost environment.
  # TODO: (RK) What about environments...? Those won't work correctly.
  e <- env%$%env; g <- env%$%ghost
  if (!exists(name, envir = g, inherits = FALSE)) {
    g[[name]] <-
      if (exists(name, envir = e, inherits = FALSE)) e[[name]]
      else NULL
  }

  `[[<-`(e, name, value)
  env
}

#' @export
assign <- function(x, value, envir, ...) {
  if (!missing(envir)) {
    if (is.tracked_environment(envir)) {
      envir[[x]] <- value
    } else base::assign(x, value, envir, ...)
  } else base::assign(x, value, ...)
}

replay <- function(env, count, silent = FALSE) {
  stopifnot(is.tracked_environment(env))

  # If the current commit head is the same as the commit we are replaying to
  # and no unstaged changes exist on the environment, there is no work to do.
  if (count == (env%$%commits)$head() && length(env%$%ghost) == 0L) {
    return(env)
  }

  snapshot <- env%$%snapshot
  reference_index <-
    if (snapshot > count) 1
    else 1 + floor(count / ((length(env%$%reference) - 1) * snapshot))

  rm(list = ls(env, all = TRUE), envir = env)
  copy_env(env%$%env, (env%$%reference)[[reference_index]])

  seq2    <- function(x, y) { if (y >= x) seq(x, y) else integer(0) }
  commits <- (env%$%commits)$peek_all()[
    seq2(1 + (reference_index - 1) * snapshot, count)]

  for (commit in commits) { commit[[1]](env) }

  `reset_environment!`(env) # Re-calculate the current universe.

  # In silent replays, we do not modify the commit chain or the reference chain
  if (identical(silent, FALSE)) {
    for (i in seq_len((env%$%commits)$count() - count)) {
      (env%$%commits)$pop() # Remove these commits from history
    }

    env%$%reference <- (env%$%reference)[seq_len(reference_index)]
  } else {
    # But we do have to update the head accordingly...
    (env%$%commits)$set_head(count)
  }

  env
}

`need_snapshot?` <- function(env) {
  (env%$%commits)$count() > length(env%$%reference) * (env%$%snapshot)
}

`snapshot!` <- function(env) {
  stopifnot(is.tracked_environment(env))
  reference <- new.env(parent = emptyenv())
  copy_env(reference, env%$%env)

  env%$%reference <- c(env%$%reference, reference)
}

`reset_environment!` <- function(env) {
  env%$%universe <- ls(env%$%env, all = TRUE)
  clear_environment(env%$%ghost)
  env
}

