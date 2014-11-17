# Version 0.1.1

* Rudimentary object diffing of lists. Several features are still to be
  desired, like diffing minor insertions, deletions, or name changes
  without incurring a prohibitive memory overhead, but the in general
  two arbitrary lists can be diffed. 

* Rudimentary diffing of atomic vectors. The same heuristic considerations
  apply as to lists: the features are adequate but more optimization is possible.

