# Version 0.2.3.9001

 * Added an optimization to `replay` that avoids re-building an environment
   when it is already at the desired commit.

# Version 0.2.3.9000

 * Started development version of 0.2.3. Fixed attribute patching when
   two lists are passed to `objectdiff`.

# Version 0.2.2

 * Better list and data.frame patches. In particular, additions and deletions from
   a list (or data.frame) will not make a full copy of the object.

 * [Data.frame row removal is fixed.](https://github.com/robertzk/objectdiff/issues/35)

# Version 0.2.1

* A new exported function `force_push`. It allows a `tracked_environment` to revert
  to its state as of any given commit, specified either by numerical index or by name.

* The `rollback` and `rollback<-` functions support a `silent` parameter that
  controls whether or not to prune the commit history after rolling back.
  This can be useful when one wishes to revert to an earlier state, while 
  retaining the ability to "roll forward" back to the future unless a
  change is made.

* The `commits` function for fetching a named list of all commits of a
  `tracked_environment`.

# Version 0.2.0

* Introduced a `tracked_environment` object. This is like an `environment`,
  but it supports [commits](R/tracked_environment.R#L78) in-memory,
  allowing you to [rollback](R/tracked_environment.R#L107) your changes to
  a previous state (or peek back in history by re-running a chain of patches).
  For more details, see the [vignette on `tracked_environment`s](vignettes/tracked_environment.Rmd)

# Version 0.1.1

* Rudimentary object diffing of lists. Several features are still to be
  desired, like diffing minor insertions, deletions, or name changes
  without incurring a prohibitive memory overhead, but the in general
  two arbitrary lists can be diffed. 

* Rudimentary diffing of atomic vectors. The same heuristic considerations
  apply as to lists: the features are adequate but more optimization is possible.

