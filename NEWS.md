# climeseries 0.150.0

* Consolidated duplicated utility code into the `keystone` dependency
  (Stage 2a/2b refactor); no user-facing behavior change.
* Reorganized `R/` source files: split `gridded.R` and `helper.R` into
  focused modules.
* `fit_segmented_model` retained as a local implementation rather than
  delegating to `keystone::fit_segmented_model`; see inline note for rationale.
* Declared previously-implicit package dependencies: `dplyr`, `tibble`,
  `tidyr`, `purrr`, `tidyselect`, `tictoc`, `uncompress`, `abind`, `rio`,
  `rlist`, `fs`, `stringi`, `rex`, `naniar`, `colorspace`, `DescTools`, `boot`.
