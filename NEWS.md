# Version 0.2.0

* Introduced a `tracked_environment` object. This is like an `environment`,
  but it supports [commits](R/tracked_environment.R#L56) in-memory,
  allowing you to [rollback](R/tracked_environment.R#L77) your changes to
  a previous state (or peek back in history by re-running a chain of patches).
  For more details, see the [vignette on `tracked_environment`s](vignettes/tracked_environment.Rmd)

# Version 0.1.1

* Rudimentary object diffing of lists. Several features are still to be
  desired, like diffing minor insertions, deletions, or name changes
  without incurring a prohibitive memory overhead, but the in general
  two arbitrary lists can be diffed. 

* Rudimentary diffing of atomic vectors. The same heuristic considerations
  apply as to lists: the features are adequate but more optimization is possible.

