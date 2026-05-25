HARNESS BUILD (Task B) — build the oracle, nothing else this round.

1. Vendor the wiki-plot script verbatim into inst/harness/run_harness.R, with ONLY
   these two edits:
     - .reload_all(...)            -> devtools::load_all(getwd())
     - plot/PNG save directory      -> inst/harness/golden/

2. Wrap each plot block in tryCatch() so one failure doesn't halt the run. Collect
   and report which blocks rendered vs. skipped, with the error message for skips.

3. Determinism (required, or the visual diff is invalid):
   a. Data frozen — force download = FALSE on the data-fetch call(s)
      (e.g. get_climate_data(download = FALSE)); run against the existing
      climeseries_data_dir snapshot with no network access.
   b. Clock pinned — IMMEDIATELY after load_all(), override in the climeseries
      namespace:
        assignInNamespace("current_year",  <YYYY>, "climeseries")
        assignInNamespace("current_month", <MM>,   "climeseries")
      and set current_year_lagged / current_month_lagged consistent with the
      Jan-rollover logic at constants.R:44-47. Record the pinned date in a header
      comment so goldens are reproducible across wall-clock time.

4. Render goldens to inst/harness/golden/. Report: rendered-vs-skipped list + the
   pinned date used.

OUT OF SCOPE this round: no reformatting, no keystone swaps, no file splits, no
DESCRIPTION edits (tictoc/uncompress/abind go in the separate Stage-1 housekeeping
commit), no profiling. Oracle first.
