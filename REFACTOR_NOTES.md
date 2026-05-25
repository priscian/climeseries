# climeseries Refactor Notes

Generated 2026-05-24 from a full read of all `R/` files.
**No code was changed to produce this document.**

---

## 1. File Inventory

Line counts: constants.R 474 | utils.R 818 | window-default.R 133 | zzz.R 5 |
graphics.R 55 | models.R 191 | ushcn.R 186 | series.R 2472 |
helper.R 2712 | gridded.R 1203 | plot-series.R 1242 | **total 9491**

---

### R/constants.R (474 lines)

| Symbol | Exported | Purpose |
|--------|----------|---------|
| `` `%_%` `` | YES | Infix string-concat (paste shorthand) |
| `MOS`, `MONTHS` | YES | Month-abbreviation/name vectors |
| `current_month`, `current_year` | YES | Package-load-time date constants |
| `current_month_lagged`, `current_year_lagged` | no | December-lag helpers for URL templates |
| `dataDir`, `filenameBase`, `defaultBaseline` | no | Internal defaults |
| `*Base` variables (~20) | no | URL prefix strings for each data source |
| `make_reanalysis_urls()` | no | Builds ~240 PSL/WRIT reanalysis URLs at load time |
| `reanalysis_urls` | no | Result of `make_reanalysis_urls()` (package global) |
| `data_urls` | YES | Master named list of all ~150+ climate data URLs |
| `omitUrlNames` | no | Names omitted from default download |
| `common_columns` | YES | `c("year","met_year","yr_part","month")` |

Key deps: `mgsub::mgsub`, `purrr::flatten`, `stringr::str_detect`. No `keystone::` calls.

**Flags:**
- 3× stale comment `"N.B. Change this back in Feb 2024!!"` at lines 59, 60, 106 — now past.
- TODO comments at lines 401, 438 (Cape Grim CH4/N2O, OHC additions).
- `make_reanalysis_urls()` executes at package-load time; cheap but side-effectful.
- Commented-out alternate URLs are necessary history; do not delete.

---

### R/utils.R (818 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `seqle()` | YES | Run-length encoding for arithmetic sequences |
| `na_unwrap()` generic + S3 methods | YES | Flag non-leading/trailing NAs |
| `shift()` generic + S3 methods | YES | Cyclic/non-cyclic vector shift |
| `nearest_orig()` | **no** | data.table binary-search nearest (seemingly replaced) |
| `is_zero()` | YES | Fuzzy-zero test |
| `nearest()` | YES | Closest values in a vector |
| `nearest_below()`, `nearest_above()` | YES | One-line nearest-bound helpers |
| `moving_average()` / `MA` alias | YES | Convolution moving average |
| `interpNA()` | YES | Interpolate NAs in matrix/timeSeries |
| `split_at()` | YES | Split vector at index |
| `get_dots()` | YES | Introspect `...` arguments (NSE) |
| `get_all_args()` | YES | Collect all named + `...` args of caller (NSE) |
| `merge_fun_factory()` | YES | Factory producing merge functions with preset args |
| `nop()` | YES | No-op; returns `invisible(NULL)` |
| `capwords()` | YES | Capitalize words |
| `` `%nin%` `` | YES | "not in" operator |
| `parse_one()` | YES | Extract named regex captures |
| `integratex()` | YES | Numerical area-under-curve (linear or spline) |
| `backtick()` | YES | Wrap strings in backticks |
| `dataframe()` | YES | `data.frame()` with `check.names=FALSE` defaults |
| `char_sort()` | YES | Reorder `x` elements to match sort order of `s` |
| `only_selected_series()` / `oss` | YES | Subset to `common_columns` + named series |
| `view_only_selected_series()` / `vss` | YES | `View()` wrapper around `oss()` |
| `are_same()` | YES | Reference identity via `.Internal(inspect())` |
| `all_equal()`, `is_equal()` | YES | Fuzzy equality helpers |
| `is_invalid()` | YES | NULL / empty / all-NA check |
| `make_current_timestamp()` | YES | Formatted date string |
| `add_months()` | YES | Add N months to a year-month pair |
| `eval_js()` | YES | Polymorphic eval: function / expression / string |
| `get_index_from_element()` | YES | Flat index → 2-D array index |
| `unzip_Z()` | YES | Decompress Unix `.Z` files via `uncompress` package |
| `optimize_span()` | **no** | Auto-select LOESS span via AICc/GCV |
| `LOESS()` | YES | Drop-in for `stats::loess()` with optional auto-span |
| `poly_eval()` | YES | Dispatch: function → call it; expression → eval it; else return it |

No `keystone::` calls.

**Flags:**
- `nearest_orig()` (line 156) — internal, unreferenced anywhere; appears superseded by `nearest()`. Delete candidate.
- `interpNA()` line 40 uses `stats:::na.omit.default(x)` — triple-colon private API access; fragile across R versions.
- `optimize_span()` is only called by `LOESS()`; could be nested.
- `unzip_Z()` depends on the `uncompress` package (not in DESCRIPTION).

---

### R/window-default.R (133 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `window_default()` | **no** | Adapted copy of `stats:::window.default()` |
| `FillBlankDates()` | **no** | Fill date gaps in padded time series (helper for above) |

No `keystone::` calls.

**Flags:**
- File header says "Code modified from `stats:::window.default()`." Only two lines differ from base R: the two calls to `make_time_series_from_anomalies()`. Silent divergence risk if base R changes its internal.
- `FillBlankDates()` is tightly coupled to `window_default()`; consider nesting it.
- File name uses a hyphen, unlike all other R/ files (which use dots or nothing).

---

### R/zzz.R (5 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `.onLoad()` | no | Package load hook (body is nearly empty) |

**Flags:**
- Effectively empty. The commented-out `setGeneric` line dates from a timeSeries conflict that may be resolved. Either delete the file or fold it into a comment in another file.

---

### R/graphics.R (55 lines) ← **Pilot file for Task C**

| Function | Exported | Purpose |
|----------|----------|---------|
| `vary_brightness()` | YES | Build palette by interpolating brightness of one color |
| `change_luminance()` | YES | Shift RGB channels by a fixed offset |
| `vline()` | YES | Draw vertical reference lines with year labels |

No `keystone::` calls.

**Flags:**
- Standalone comments inside `vary_brightness()` use single `#`; user style requires `##`.
- `change_luminance()` is a one-liner; the roxygen doc is absent.
- Smallest meaningful file in the package — ideal pilot for the style-reformat loop.

---

### R/models.R (191 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `get_models_data()` | YES | Load or build CMIP3/CMIP5 model ensemble data frames |

Key deps: `plyr` (unqualified `melt`, `arrange` via `import(plyr)`), `reshape2`, `stringr`, `data.table`.
No `keystone::` calls.

**Flags:**
- Uses `import(plyr)` wildcard, so `melt`, `arrange`, etc. are available unqualified; conflicts with `dplyr::arrange` when both are loaded — a known plyr/dplyr tension.
- `AlignCmip3Data()` is a well-scoped nested helper inside `get_models_data()` — good pattern.
- Roxygen `@examples` block references series names from a prior data schema (pre-v4 GISTEMP naming).
- `convert_fun = kelvin_to_celsius` default parameter references a local function by name; works but is unusual.

---

### R/ushcn.R (186 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `get_ushcn_data()` | **no** | Download & process USHCN v2.5 raw/final station data |

Key deps: `tictoc` (not in DESCRIPTION!), `data.table`, `plyr`, `reshape2`, `dplyr`.
No `keystone::` calls.

**Flags:**
- **HARDCODED PATH line 11:** `dataDir <- "C:/Users/james/Downloads/climate/data/USHCN"` — overrides `getOption("climeseries_data_dir")` that was just read on line 10.
- `tictoc::tic()` / `toc()` called but `tictoc` is absent from DESCRIPTION Imports and Suggests — clean-install failure.
- `capitalize()` (line 183) unqualified — probably `Hmisc::capitalize` or `stringr`-based but not traceable without the search path.
- Function body ends without returning anything useful (comparison series are created but never returned or plotted explicitly); reads like an abandoned analysis script.
- Not exported; arguably should live in `inst/scripts/` rather than `R/`.

---

### R/series.R (2472 lines) — *Largest file*

| Function | Exported | Purpose |
|----------|----------|---------|
| `ReadAndMungeInstrumentalData()` | **no** | Giant switch (~1970 lines) parsing every data source |
| `DownloadInstrumentalData()` | **no** | Loop over URLs, merge results, save RData/CSV |
| `make_met_year()` | YES | Add meteorological-year column |
| `make_yr_part()` | YES | Add `yr_part` (decimal year midpoint) column |
| `LoadInstrumentalData()` | **no** | Load most-recent saved RData from data dir |
| `get_climate_data()` | YES | Main entry point: download or load climate data |
| `get_climate_series_names()` | YES | Non-`common_columns` column names from a climeseries df |
| `recenter_anomalies()` | YES | Re-baseline temperature anomalies to new reference period |
| `recenter_anomalies_test()` | YES | Experimental alternative recenter (possibly dead) |
| `make_time_series_from_anomalies()` | YES | Convert data frame → `mts` zoo/ts object |
| `window_ts()` | YES | Window time series with climeseries-aware handling |

`keystone::` calls: `psapply()` × 2 (lines 2352, 2375), `is_invalid()` × 2 (lines 2362, 2382).

**Flags:**
- **GIANT FUNCTION:** `ReadAndMungeInstrumentalData()` spans roughly lines 1–1974. It is a single switch statement with one arm per data source (~80+ arms). This is intentional by design (all parsing logic in one place) but makes the file unmaintainable at this size. A registry pattern (named list of parser functions) would be the natural refactor target.
- **Dead code block** in `DownloadInstrumentalData()` lines 2004–2010: `if (FALSE) { load("C:/common/data/climate/climeseries/climate-series_raw_20241117.RData"...` — hardcoded path in dead code; harmless but should be removed or converted to a comment.
- `keystone::is_invalid()` vs local `is_invalid()` in the same file — redundant, likely identical behavior.
- `keystone::psapply()` vs `sapply()` — presumably adds parallelism; this dependency and its effect should be documented.
- `recenter_anomalies_test()` is exported but the "test" suffix strongly suggests experimental status; unclear if it is used anywhere.
- Roxygen `@return` / `@examples` in `get_climate_data()` references old series column names (GISTEMP, HadCRUT4, etc. without version suffixes) — stale docs.
- `AIRS Zonal` handler in the switch does a multi-arm join; hard to follow at this scale.

---

### R/helper.R (2712 lines) — *Second-largest, mixed concerns*

| Function | Exported | Purpose |
|----------|----------|---------|
| `correlate_co2_temperature()` | YES | CO₂ vs temperature scatter plot + correlation |
| `plot_horse_race()` | YES | ggplot YTD cumulative temperature race chart |
| `get_yearly_gistemp()` | YES | Fetch archived GISS yearly data (Wayback Machine) |
| `get_old_monthly_gistemp()` | YES | Fetch archived GISS monthly data |
| `get_satellite_slr()` | YES | Scrape satellite SLR from Colorado SLR wizard |
| `get_tidegauge_slr()` | YES | Fetch PSMSL tide-gauge SLR data |
| `remove_periodic_cycle()` | YES | Remove seasonal cycle (LOESS + Fourier) |
| `create_aggregate_variable()` | YES | Average multiple series into one aggregate |
| `create_aggregate_co2_variable()` | YES | Splice Law Dome + instrumental CO₂ |
| `add_default_aggregate_variables()` | YES | Add MEI/SAOD/TSI/CO₂ aggregate columns |
| `remove_exogenous_influences()` | YES | Foster-Rahmstorf style detrend for ENSO/volcanic/solar |
| `easy_exogenous_plot()` | YES | Convenience wrapper: exogenous removal + plot |
| `fahr_to_kelvin()`, `kelvin_to_celsius()`, `fahr_to_celsius()`, `celsius_to_fahr()` | YES | Temperature unit conversions |
| `convert_hdf4_to_h5()` | YES | Shell-invoked HDF4→HDF5 file conversion |
| `create_airs_monthly_data()` | YES | Process HDF5 AIRS files → monthly lat-weighted series |
| `create_combined_airs_series()` | YES | Average ascending + descending AIRS nodes |
| `interpolate_baseline()` | YES | Linearly extend a series backward for baselining |
| `interpolate_baselines()` | YES (has `@export`) | Apply `interpolate_baseline()` to multiple series |
| `create_cmip5_taz_data()` | YES | Read TAZ NetCDF files into named list |
| `get_rss_msu_weights()` | YES | Read RSS MSU weighting function files |
| `create_cmip5_atmosphere_temps()` | YES | Compute MSU-channel temps from CMIP5 TAZ data |
| `create_osiris_daily_saod_data_orig()` | YES | OSIRIS SAOD processor — **original/deprecated version** |
| `create_osiris_daily_saod_data()` | YES | OSIRIS SAOD processor — parallelized replacement |
| `create_osiris_saod_data()` | YES | Aggregate daily OSIRIS SAOD → monthly means |
| `make_yearly_data()` | YES | Aggregate monthly → annual means |
| `show_warmest_years()` | YES | Print top-N warmest years per series |
| `get_yearly_difference()` | YES | Compute and print trend/difference over a date range |
| `make_vv_cranberry_plot()` | YES | LOESS + residuals plot (Variable-Variability style) |
| `show_single_value()` | YES | Print hottest/coldest year with ranking |
| `create_cmip5_tas_tos_data()` | YES | Rebuild CMIP5 TAS+TOS blended land/ocean data |
| `create_loess_variables()` | YES | Add LOESS fit columns to a data frame |
| `add_loess_variables()` | YES | Merge LOESS columns back into original data |
| `fit_segmented_model()` | YES | Breakpoint detection + segmented linear regression |
| `nearest_year_month_from_numeric()` | YES | Decimal year → nearest year/month pair |
| `create_timeseries_from_gridded()` | YES | **STUB — body says "To be continued!"** |
| `create_zonal_data()` | YES | Download gridded NetCDF, compute weighted-mean series |
| `read_cru_hemi()` | **no** | Read CRU hemispherical text file |
| `correct_monthly_autocorrelation()` | YES | Autocorrelation-corrected trend SE (Foster-Rahmstorf / Santer) |
| `simulate_temp_series()` | YES | Generate synthetic monthly temperature series |

`keystone::` calls: `shift()` at lines 2576, 2580 (inside `correct_monthly_autocorrelation()`).

**Flags:**
- `create_osiris_daily_saod_data_orig()` — exported but superseded by `create_osiris_daily_saod_data()`. Should be unexported (`@keywords internal`) or removed after confirming it's not called externally.
- `create_timeseries_from_gridded()` — exported stub. Body is one comment: "To be continued!" Consider removing the export or adding a `stop()` with a clear message.
- `interpolate_baselines()` has `#' @export` but is **not in NAMESPACE** — NAMESPACE appears stale; run `devtools::document()` to reconcile.
- `keystone::shift()` in `correct_monthly_autocorrelation()` vs local `shift()` — functionally identical signatures; the `keystone::` prefix implies a specific behavior is needed (possibly different `roll` default?); should be documented.
- `read_cru_hemi()` is internal but lacks `@keywords internal`; not clearly connected to any exported function that calls it.
- **God-file problem:** helper.R mixes temperature conversions, AIRS HDF5 processing, CMIP TAZ processing, OSIRIS SAOD processing, aggregate variable creation, Foster-Rahmstorf analysis, LOESS helpers, segmented models, and more. All concerns are separately maintainable.

---

### R/gridded.R (1203 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `make_planetary_grid()` | YES | lat×long weighted grid matrix (class `PlanetaryGrid`) |
| `find_planetary_grid_square()` | YES | Find row/col indices for a lat/long coordinate |
| `get_series_from_ghcn_gridded()` | YES | Download/process GHCN-M v2/v3/v4 into data frames |
| `make_coverage_filter()` | YES | Filter gridded data by coverage threshold |
| `make_ghcn_temperature_series()` | YES | Build area-weighted mean T series from GHCN-M grid |
| `metadata_select()` | YES | Select metadata rows by criteria |
| `get_station_counts()` | YES | Station counts per grid cell over time |
| `get_random_stations()` | YES | Sample random stations from gridded data |
| `grid_info()` | YES | Summary statistics of planetary grid cell contents |
| `plot_stations_map()` | YES | Plot station locations on a map |

`keystone::` calls: `psapply()` (line 388), `na_unwrap()` (line 439).
`tictoc::` calls: 24+ (lines 187, 343, 354, 461, 526, etc.).

**Flags:**
- **`tictoc` not in DESCRIPTION** — used 24+ times in `make_ghcn_temperature_series()` and `get_series_from_ghcn_gridded()`. Will fail `R CMD check` and clean installs. Must add to Imports or Suggests.
- `keystone::na_unwrap()` (line 439) is identical to the locally exported `na_unwrap()` — which one to use should be consistent.
- The file name is misleading: `make_planetary_grid()` and `find_planetary_grid_square()` are general-purpose grid infrastructure used by `helper.R` and `ushcn.R` too; the GHCN-M functions are data-source specific. See proposed split below.

---

### R/plot-series.R (1242 lines)

| Function | Exported | Purpose |
|----------|----------|---------|
| `plot_climate_data()` | YES | Main multi-series climate plot with MA, baseline, CI, trends, segmented, LOESS |
| `make_standardized_plot_filename()` | **no** | Build canonical output PNG filename from plot parameters |
| `plot_sequential_trend()` | YES | Running trend plot (one trend per start year) |
| `plot_models_and_climate_data()` | YES | Overlay CMIP ensemble envelope on observed series |

No `keystone::` calls.

**Flags:**
- `plot_climate_data()` function signature spans nearly 200 characters (~50 parameters). Intentional (dots-passing convention), but hard to scan.
- `make_standardized_plot_filename()` (line 625) lacks `@keywords internal` in its (absent) roxygen block.
- `plot_models_and_climate_data()` relies on `attr(cmip, "scenario")` and similar model attributes set by `get_models_data()` — coupling is invisible to callers.
- `sign_callback` default uses `rlang::expr(text(..., labels = "@priscian", ...))` — personal watermark baked into default; fine but document it.

---

## 2. Cross-File Issues

### Dead / superseded code

| Location | Issue |
|----------|-------|
| `utils.R:156` `nearest_orig()` | Internal, no callers found; superseded by `nearest()` |
| `helper.R` `create_osiris_daily_saod_data_orig()` | Exported; clearly superseded by the non-`_orig` version |
| `helper.R` `create_timeseries_from_gridded()` | Exported stub: body = one comment "To be continued!" |
| `helper.R` `recenter_anomalies_test()`? | "test" in name; verify whether any script calls it |
| `series.R:2004–2010` `if (FALSE) { load("C:/common/...") }` | Dead code with hardcoded path |

### Hardcoded paths

| Location | Path |
|----------|------|
| `ushcn.R:11` | `"C:/Users/james/Downloads/climate/data/USHCN"` |
| `series.R:2006` | `"C:/common/data/climate/climeseries/climate-series_raw_20241117.RData"` (dead block) |

### Missing DESCRIPTION entries

| Package | Used in | Notes |
|---------|---------|-------|
| `tictoc` | `gridded.R` (24+ calls), `ushcn.R` (2 calls) | Not in Imports or Suggests |
| `uncompress` | `utils.R:unzip_Z()` | Not in Imports or Suggests |
| `abind` | `helper.R:create_airs_monthly_data()` | Not in Imports or Suggests |
| `esd` | `helper.R:create_cmip5_atmosphere_temps()` | `data(etopo5, package="esd")` |

### keystone:: call sites

| File | Line | Call | Local equivalent |
|------|------|------|-----------------|
| `series.R` | 2352, 2375 | `keystone::psapply()` | `sapply()` (different: parallel) |
| `series.R` | 2362, 2382 | `keystone::is_invalid()` | local `is_invalid()` |
| `gridded.R` | 388 | `keystone::psapply()` | `sapply()` (different: parallel) |
| `gridded.R` | 439 | `keystone::na_unwrap()` | local `na_unwrap()` |
| `helper.R` | 2576, 2580 | `keystone::shift()` | local `shift()` |

Note: `keystone::psapply()` genuinely differs from `sapply()` (parallel execution); the qualifier is load-bearing. The `is_invalid`/`na_unwrap`/`shift` calls shadow local functions — verify behavior is identical before replacing.

### NAMESPACE mismatch

`interpolate_baselines()` in `helper.R` has `#' @export` but is **absent from NAMESPACE**. Either `devtools::document()` has not been re-run, or the export was intentionally removed from NAMESPACE without removing the roxygen tag. Reconcile.

### Private API access

`utils.R:40`: `stats:::na.omit.default(x)` — triple-colon access to a base-R internal. Will generate a NOTE in `R CMD check` and could break silently if R changes the internal.

### .gitignore gaps

`.Rprofile` is not in `.gitignore`. The user's `.Rprofile` sets `climeseries_data_dir` and other options; if accidentally committed it would expose local paths. **Add `.Rprofile` to `.gitignore`.**

### Stale / inconsistent comments

- 3× `"N.B. Change this back in Feb 2024!!"` in `constants.R` (lines 59, 60, 106) — past date.
- `utils.R:128–135`: usage examples left as top-level comment block — fine but inconsistent.
- Some files use `## usage:` blocks after functions; others use inline `# usage` or nothing.

---

## 3. Style Inconsistencies

- **Comment hashes:** Most standalone comments use `#` but user style specifies `##`. This is pervasive throughout all files; `graphics.R` is the exception pilot.
- **Assignment operator:** Mix of `<-` (top-level) and `=` (inside function bodies, e.g., `loessArgs = list(...)`). User style is spaces around `=` for assignments.
- **`return(x)` vs `return (x)`:** Most functions use `return (x)` (with space); occasional `return(x)`.
- **Spacing in function signatures:** Some have no spaces around `=` in defaults (`end=NULL`); others do (`na_rm = FALSE`).
- **Single-line vs braced bodies:** Inconsistent for simple S3 methods (some on one line, some not).
- **Quoting:** Mix of single and double quotes; no consistent rule.

---

## 4. Giant Functions

| Function | File | Lines (approx.) | Notes |
|----------|------|-----------------|-------|
| `ReadAndMungeInstrumentalData()` | series.R | ~1970 | One switch arm per data source |
| `make_yearly_data()` | helper.R | ~40 | Acceptable |
| `plot_climate_data()` | plot-series.R | ~600 | Large but structured; ~50 params |
| `make_ghcn_temperature_series()` | gridded.R | ~500 | Highly nested; tictoc-instrumented |
| `remove_exogenous_influences()` | helper.R | ~110 | Acceptable |

`ReadAndMungeInstrumentalData()` is the primary refactoring target: a dispatch table (named list of parser closures) would replace the 80+ arm switch and allow adding new data sources without touching existing parsers.

---

## 5. Proposed File Organization

*Propose only; do not apply until approved.*

| Current file | Proposed file(s) | Rationale |
|---|---|---|
| `constants.R` | `constants.R` | No change |
| `utils.R` | `utils.R` | No change; `nearest_orig()` deletion is a separate PR |
| `graphics.R` | `graphics.R` | Keep; could absorb `zzz.R` |
| `zzz.R` | *(delete or absorb)* | Near-empty; `.onLoad()` body is a commented-out stub |
| `window-default.R` | `window-default.R` | Keep with stronger header comment |
| `series.R` | `series.R` | Structure is intentional; `ReadAndMungeInstrumentalData` refactor is a future milestone |
| `helper.R` | `analysis.R` (stats/exogenous/aggregate) + `airs.R` + `cmip.R` + `osiris.R` + `slr.R` | Split by domain; keeps each file under ~400 lines |
| `gridded.R` | `grid.R` (planetary grid infrastructure) + `ghcn.R` (GHCN-M download/processing) | `make_planetary_grid` and `find_planetary_grid_square` are used by multiple other files and belong in shared infrastructure |
| `models.R` | `models.R` | No change |
| `plot-series.R` | `plot-series.R` | No change |
| `ushcn.R` | `inst/scripts/ushcn-analysis.R` | Not exported; reads like a personal analysis script; hardcoded path needs fixing either way |

**gridded.R rename note:** The current name implies only gridded-data processing. The planetary grid classes (`PlanetaryGrid`, `make_planetary_grid`, `find_planetary_grid_square`) are a general coordinate-system utility; isolating them makes them easier to find and test independently of the GHCN-specific processing.

---

## 6. TODO / FIXME Summary

| File | Line | Note |
|------|------|------|
| constants.R | 59, 60, 106 | Stale "Change this back in Feb 2024" comments |
| constants.R | 401 | TODO: Cape Grim CH4 / N2O |
| constants.R | 438 | TODO: More OHC, snow/ice |
| helper.R | 2335 | TODO: comment block inside `create_zonal_data()` metadata list |
| series.R | 2004 | `if (FALSE) { ... }` dead block with hardcoded path |
| ushcn.R | 11 | Hardcoded `C:/Users/james/...` path |
| utils.R | 40 | `stats:::na.omit.default()` triple-colon access |
| gridded.R | (DESCRIPTION) | `tictoc` not declared |
| helper.R | (NAMESPACE) | `interpolate_baselines` has `@export` but not in NAMESPACE |
