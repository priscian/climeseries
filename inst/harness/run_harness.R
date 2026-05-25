## inst/harness/run_harness.R
## Oracle harness — renders golden PNGs to inst/harness/golden/ for visual regression testing.
## Pinned date: 2026-05-24 (current_year=2026, current_month=5; no Jan rollover → lagged==current).
## Vendored from D:/Users/priscian/my_documents/code/2016/R/climate/make-climeseries-wiki-plots.R
## with only the changes specified in inst/harness/SPEC.md.

## ── Preflight ─────────────────────────────────────────────────────────────────
if (is.null(getOption("climeseries_data_dir")))
  stop("climeseries_data_dir option is not set — source your .Rprofile first.")

## ── Load package ──────────────────────────────────────────────────────────────
devtools::load_all(getwd())

## ── Pin clock (May 2026; no January rollover → lagged == current) ─────────────
assignInNamespace("current_year",          2026L, "climeseries")
assignInNamespace("current_month",            5L, "climeseries")
assignInNamespace("current_year_lagged",   2026L, "climeseries")
assignInNamespace("current_month_lagged",     5L, "climeseries")

## ── Golden output directory ───────────────────────────────────────────────────
save_png_dir <- file.path(getwd(), "inst", "harness", "golden")
dir.create(save_png_dir, recursive = TRUE, showWarnings = FALSE)

## ── Result tracking ───────────────────────────────────────────────────────────
.rendered <- character(0)
.skipped  <- list()

.ok <- function(label) {
  cat(sprintf("  [OK] %s\n", label))
  .rendered <<- c(.rendered, label)
}
.fail <- function(label, e) {
  msg <- conditionMessage(e)
  cat(sprintf("  [SKIP] %s\n    %s\n", label, msg))
  .skipped[[label]] <<- msg
}

# ```r
########################################
## Plot several global instrumental temperature series.
########################################
cat("\n── Blocks 01-03: Instrumental temperature series ──\n")
tryCatch({
  airs_series <- "AIRS v7 Global"; baseline <- 1981:2010
  new_airs <- interpolate_baseline(airs_series, baseline = baseline)
  inst0 <- get_climate_data(download = FALSE, baseline = FALSE)
  inst0 <- create_aggregate_variable(inst0, c("20th C. Reanalysis V3 Surface Air Global",
    "NCEP/DOE R2 Surface Air Global"),
    "20th C. Reanalysis V3–NCEP/DOE R2 Surface Air Global", type = "head")
  inst0[[airs_series]] <- new_airs[[airs_series]]
  series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT5 Global",
    "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5",
    "UAH TLT 6.0 Global", "STAR v5.0 TLT Global Mean", "JRA-3Q Surface Air Global",
    "ERA5 2m Global", "NCEP/NCAR R1 Surface Air Global",
    "20th C. Reanalysis V3–NCEP/DOE R2 Surface Air Global",
    "RATPAC-A Surface GLOBE", airs_series)
  inst <- inst0 %>%
    dplyr::select(all_of(c(get_climate_series_names(inst0, invert = FALSE), series))) %>%
    recenter_anomalies(baseline = baseline, keep = series, skip = "AIRS v7 Global")
  ## N.B. Don't rebaseline here!

  tryCatch({
    plot_climate_data(inst, series = series, 1880, yearly = TRUE, lwd = 2, ylim = c(-1.0, 1.0),
      save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "monthly-temp-series_1880.1-recent_yearly_baseline1981-2010.png", sep = "/")))
    .ok("01: inst_1880_yearly")
  }, error = function(e) .fail("01: inst_1880_yearly", e))

  ## Not in README: Plot these series from 1970-current as 12-month moving averages.
  tryCatch({
    plot_climate_data(inst, series = series, 1970, ma = 12, yearly = FALSE, lwd = 1, conf_int = FALSE,
      trend = TRUE, trend_legend_inset = c(0.1, 0.0), interpolate = TRUE, save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "monthly-temp-series_1970.1-recent_ma12_baseline1981-2010.png", sep = "/")))
    .ok("02: inst_1970_ma12")
  }, error = function(e) .fail("02: inst_1970_ma12", e))

  ## Not in README: Plot these series offset from each other & w/ piecewise regressions along them.
  tryCatch({
    inst1 <- inst
    offset <- 0.0; for (s in series) { inst1[[s]] <- inst1[[s]] + offset; offset <- offset - 0.4 }
    plot_climate_data(inst1, series = series, 1880, end = NULL, yearly = TRUE, lwd = 1,
      segmented = TRUE, plot.segmented... = list(lwd = 1), mark_segments = "points",
      ylim = NULL, alpha = 1.0, points.segmented... = list(lwd = 2), save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "monthly-temp-series_1880.1-recent_baseline1981-2010_seg_offset.png", sep = "/")))
    .ok("03: inst_seg_offset")
  }, error = function(e) .fail("03: inst_seg_offset", e))

}, error = function(e) {
  msg <- conditionMessage(e)
  cat(sprintf("  [SETUP FAIL blocks 01-03]: %s\n", msg))
  .skipped[["01: inst_1880_yearly"]]  <<- paste("setup:", msg)
  .skipped[["02: inst_1970_ma12"]]    <<- paste("setup:", msg)
  .skipped[["03: inst_seg_offset"]]   <<- paste("setup:", msg)
})
# ```

# ```r
########################################
## Plot global instrumental temperature series with 95% confidence intervals.
########################################
cat("\n── Block 04: BEST + HadCRUT5 with 95% CI ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = TRUE)
  series <- c("BEST Global (Air Ice Temp.)", "HadCRUT5 Global")
  plot_climate_data(inst, series = series, 1850, yearly = TRUE, lwd = 2, conf_int = TRUE,
    col = c("red", "blue"), alpha = 0.4, ci_alpha = 0.1, save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "cw14.ci-hadcrut4.ci_1850.1-recent_yearly_baseline1981-2010.png", sep = "/")))
  .ok("04: best_hadcrut5_ci")
}, error = function(e) .fail("04: best_hadcrut5_ci", e))
# ```

# ```r
########################################
## Plot all CMIP5 scenario realizations, no instrumental temperature series.
########################################
cat("\n── Block 05: CMIP5 all scenario realizations ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = TRUE)
  cmip5 <- get_models_data(ensemble = "cmip5")
  plot_models_and_climate_data(inst, cmip5, series = NULL, scenario = NULL,
    start = 1950, end = 2100.99,
    ma = 12, baseline = 1981:2010, center_fun = "mean", smooth_envelope = TRUE,
    col_m_mean = "red", ylim = c(-1, 5), save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "cmip5-realizations_1950.1-2100.1_ma12_baseline1981-2010.png", sep = "/")))
  .ok("05: cmip5_realizations")
}, error = function(e) .fail("05: cmip5_realizations", e))
# ```

# ```r
########################################
## CMIP5 RCP 8.5 TAS + TOS scenario realizations compared to the primary land+SST series.
## Cf. Fig. 4(a) of Cowtan et al. 2015, dx.doi.org/10.1002/2015GL064888
########################################
cat("\n── Block 06: CMIP5 TAS+TOS RCP 8.5 ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = TRUE)
  cmip5 <- get_models_data(ensemble = "cmip5", subdir = "tas + tos")
  series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT5 Global",
    "BEST Global (Air Ice Temp.)", "JMA Global")
  plot_models_and_climate_data(inst, cmip5, series = series, scenario = NULL, start = 1950,
    end = 2050.99, yearly = TRUE, ma = 12, baseline = 1986:2005, scenario_text =
    "Scenario TAS + TOS Realizations", center_fun = "mean", smooth_envelope = FALSE,
    envelope_type = "range", envelope_coverage = 0.90, envelope_text = "range",
    ylim = c(-0.75, 2.75), conf_int_i = FALSE, col_m_mean = grDevices::gray(0.8),
    alpha_envelope = 0.1, save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "cmip5-tas+tos-rcp85-realizations.range+land-sst_1880.1-2020.1_yearly_baseline1970-2000.png", sep = "/")))
  inst <- NULL; cmip5 <- NULL; gc()
  .ok("06: cmip5_tas_tos_rcp85")
}, error = function(e) {
  inst <- NULL; cmip5 <- NULL; gc()
  .fail("06: cmip5_tas_tos_rcp85", e)
})
# ```

# ```r
########################################
## Remove influence of exogenous factors characterizing ENSO, volcanic activity, and solar.
## Cf. Foster & Rahmstorf 2011, dx.doi.org/10.1088/1748-9326/6/4/044022
## Update 2024: https://tamino.wordpress.com/2024/02/16/adjusted-global-temperature-data/
########################################
cat("\n── Blocks 07a-07g: Exogenous influences removal ──\n")
tryCatch({
  airs_series <- "AIRS v7 Global"; baseline <- 1981:2010
  inst0 <- get_climate_data(download = FALSE, baseline = FALSE)
  new_airs <- interpolate_baseline(airs_series, inst0, baseline = baseline)
  inst0 <- create_aggregate_variable(inst0, c("20th C. Reanalysis V3 Surface Air Global",
    "NCEP/DOE R2 Surface Air Global"),
    "20th C. Reanalysis V3–NCEP/DOE R2 Surface Air Global", type = "head")
  inst0[[airs_series]] <- new_airs[[airs_series]]
  series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT5 Global",
    "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5",
    "UAH TLT 6.0 Global", "STAR v5.0 TLT Global Mean", "JRA-3Q Surface Air Global",
    "ERA5 2m Global", "NCEP/NCAR R1 Surface Air Global",
    "20th C. Reanalysis V3–NCEP/DOE R2 Surface Air Global",
    "RATPAC-A Surface GLOBE", airs_series)
  inst <- inst0 %>%
    recenter_anomalies(baseline = baseline, keep = series, skip = airs_series)
  start <- 1950; end <- NULL
  g0 <- remove_exogenous_influences(inst, series = series, start = start, end = end, max_lag = 12)
  airs_series_adj <- paste(airs_series, "(adj.)")
  new_airs_adj <- interpolate_baseline(airs_series_adj, g0, baseline = baseline)
  series_adj <- paste(series, "(adj.)")
  g <- g0 %>%
    recenter_anomalies(baseline = baseline, keep = series_adj, skip = airs_series_adj)
  g[[airs_series_adj]] <- new_airs_adj[[airs_series_adj]]
  main <- "Average Temperature Adjusted for ENSO, Volcanic, & Solar Influences"
  ## N.B. Don't rebaseline here!

  tryCatch({
    plot_climate_data(g, series_adj, yearly = TRUE, main = main, type = "o", pch = 19,
      baseline = FALSE, save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "major-monthly-inst-series-adj_1950.1-recent_yearly_baseline1981-2010.png", sep = "/")))
    .ok("07a: exo_adj_series_1950")
  }, error = function(e) .fail("07a: exo_adj_series_1950", e))

  ## Plot several forcing variables
  tryCatch({
    inst <- get_climate_data(download = FALSE, baseline = FALSE) %>%
      add_default_aggregate_variables()
    forcings <- c("MEI Aggregate Global", "SAOD Aggregate Global", "TSI Aggregate Global",
      "CO2 Aggregate Global (interp.)")
    forcings_scaled <- paste(forcings, "scaled")
    forcings_start_year <- 1880
    plyr::l_ply(forcings, function(a) { inst[[paste(a, "scaled")]] <<-
      inst[[a]] %>% `is.na<-`(inst$year < forcings_start_year) %>% `-`(min(., na.rm = TRUE)) %>%
        `/`(max(., na.rm = TRUE)) }) # Optimize white space
    plot_climate_data(inst, series = forcings_scaled[1:4], start = forcings_start_year,
      ylab = "Normalized Ordinate", main = "Climate Forcings", ma = 12, yearly = FALSE, lwd = 2,
      col = c("darkblue", "darkgrey", "darkorange", "darkred"), save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "exogenous-forcings+co2_normalized_1880.1-recent.png", sep = "/")))
    .ok("07b: exo_forcings_normalized")
  }, error = function(e) .fail("07b: exo_forcings_normalized", e))

  ## Not in README: Plot the unadjusted series
  tryCatch({
    plot_climate_data(g, series, 1950, yearly = TRUE, main = NULL, type = "o", pch = 19, baseline = FALSE,
      save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "major-monthly-inst-series_1950.1-recent_yearly_baseline1981-2010.png", sep = "/")))
    .ok("07c: exo_unadj_series_1950")
  }, error = function(e) .fail("07c: exo_unadj_series_1950", e))

  ## Not in README: Plot the adjusted series w/ trend values.
  tryCatch({
    plot_climate_data(g, series_adj, 1970, yearly = TRUE, main = main, type = "o", pch = 19, baseline = FALSE,
      trend = TRUE, trend_legend_inset = c(0.1, 0.001), trend_format = "1.2f",
      save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "major-monthly-inst-series-adj_1970.1-recent_yearly_baseline1981-2010_trend.png", sep = "/")))
    .ok("07d: exo_adj_series_1970_trend")
  }, error = function(e) .fail("07d: exo_adj_series_1970_trend", e))

  ## Not in README: Plot the unadjusted series w/ trend values.
  tryCatch({
    plot_climate_data(g, series, 1970, yearly = TRUE, main = NULL, type = "o", pch = 19, baseline = FALSE,
      trend = TRUE, trend_legend_inset = c(0.1, 0.001), trend_format = "1.2f",
      save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "major-monthly-inst-series_1970.1-recent_yearly_baseline1981-2010_trend.png", sep = "/")))
    .ok("07e: exo_unadj_series_1970_trend")
  }, error = function(e) .fail("07e: exo_unadj_series_1970_trend", e))

  ## Not in README: Changepoint analysis on unadjusted & adjusted data.
  tryCatch({
    g1 <- g
    offset <- 0.0; for (s in series) { g1[[s]] <- g1[[s]] + offset; offset <- offset - 0.25 }
    plot_climate_data(g1, series = series, 1970, end = NULL, yearly = TRUE, lwd = 1, segmented = TRUE,
      plot.segmented... = list(lwd = 1), mark_segments = "points", ylim = c(-3.5, 0.75), alpha = 1.0,
      points.segmented... = list(lwd = 2), save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "major-monthly-inst-series_1970.1-recent_yearly_baseline1981-2010_seg_offset.png", sep = "/")))
    offset <- 0.0; for (s in series_adj) { g1[[s]] <- g1[[s]] + offset; offset <- offset - 0.25 }
    plot_climate_data(g1, series = series_adj, 1970, end = NULL, yearly = TRUE, lwd = 1, segmented = TRUE,
      plot.segmented... = list(lwd = 1), mark_segments = "points", ylim = c(-3.5, 0.75), alpha = 1.0,
      points.segmented... = list(lwd = 2), save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "major-monthly-inst-series-adj_1970.1-recent_yearly_baseline1981-2010_seg_offset.png", sep = "/")))
    .ok("07f: exo_seg_offset_pair")
  }, error = function(e) .fail("07f: exo_seg_offset_pair", e))

  ## Not in README: Create a file of the exogenous variables.
  ## N.B. Wrapped in tryCatch — writes to hardcoded path outside repo (expected skip).
  tryCatch({
    inst2 <- get_climate_data(download = FALSE, baseline = FALSE)
    g2 <- add_default_aggregate_variables(inst2)
    g2 <- g2[, c(intersect(common_columns, names(g2)), setdiff(names(g2), names(inst2)))]
    rio::export(g2, "D:/Users/priscian/my_documents/code/2016/R/climeseries/inst/extdata/latest/global-exogenous-variables+co2_latest.csv")
    .ok("07g: rio_export_exo_vars")
  }, error = function(e) .fail("07g: rio_export_exo_vars (hardcoded path — expected skip)", e))

  inst <- NULL; g <- NULL; gc()

}, error = function(e) {
  msg <- conditionMessage(e)
  cat(sprintf("  [SETUP FAIL blocks 07a-07g]: %s\n", msg))
  for (lbl in c("07a: exo_adj_series_1950", "07b: exo_forcings_normalized",
                 "07c: exo_unadj_series_1950", "07d: exo_adj_series_1970_trend",
                 "07e: exo_unadj_series_1970_trend", "07f: exo_seg_offset_pair",
                 "07g: rio_export_exo_vars (hardcoded path — expected skip)")) {
    .skipped[[lbl]] <<- paste("setup:", msg)
  }
})
# ```

# ```r
########################################
## Estimate optimal number and location of significant changepoints in piecewise regression of
##   climate series.
## Cf. Figure 1 of Cahill et al. 2015, dx.doi.org/10.1088/1748-9326/10/8/084002
########################################
cat("\n── Block 08: Changepoint analysis ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = TRUE)
  series <- c("HadCRUT5 Global", "NCEI Global", "GISTEMP v4 Global", "JMA Global")
  plot_climate_data(inst, series, yearly = TRUE, col = c("red", "purple", "blue", "green"), lwd = 1,
    segmented = TRUE, save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "hadcrut4+ncei+gistemp+cw14_1850.1-recent_yearly_baseline1981-2010_seg.png", sep = "/")))
  gc()
  .ok("08: changepoint_4series")
}, error = function(e) .fail("08: changepoint_4series", e))
# ```

# ```r
########################################
## Has past sea-level rise accelerated?
## V. Church & White 2011, dx.doi.org/10.1007/s10712-011-9119-1.
########################################
cat("\n── Block 09: CSIRO reconstructed GMSL segmented ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = FALSE)
  series <- c("CSIRO Reconstructed Global Mean Sea Level")
  g <- remove_periodic_cycle(inst, series, fit_unc = FALSE)
  series_adj <- series %_% " (anomalies)"
  ylab <- "Global Mean Sea Level (mm)"
  main <- "Reconstructed GMSL"
  plot_climate_data(g, series_adj, yearly = TRUE, ylab = ylab, main = main, col = "blue", conf_int = TRUE,
    segmented = TRUE, mark_segments = "lines", vline... = list(text... = list(y = 125)),
    segmented... = list(yearly = FALSE, breakpoints... = list(h = 36, breaks = NULL)),
    plot.segmented... = list(col = "red"), save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "csiro-reconstructed-gmsl-anomalies_1880.1-recent_yearly_seg.png", sep = "/")))
  .ok("09: csiro_gmsl_seg")
}, error = function(e) .fail("09: csiro_gmsl_seg", e))
# ```

# ```r
########################################
## Has recent sea-level rise accelerated?
## V. https://tamino.wordpress.com/2017/10/24/what-is-sea-level-up-to-lately
########################################
cat("\n── Block 10: NOAA GMSL ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = FALSE)
  series <- c("NOAA Global Mean Sea Level")
  g <- remove_periodic_cycle(subset(inst, inst$year >= 1993), series)
  series_adj <- series %_% " (anomalies)"
  ylab <- "Global Mean Sea Level (mm)"
  main <- "GMSL from TOPEX/Poseidon, Jason-1, & Jason-2 Satellite Altimetry"
  plot_climate_data(g, series_adj, ylab = ylab, main = main, col = "blue", segmented = TRUE,
    mark_segments = "lines", segmented... = list(yearly = FALSE,
    breakpoints... = list(h = 120, breaks = NULL)), plot.segmented... = list(col = "red"),
    save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "noaa-gmsl-anomalies_1993.1-recent_ma0_seg.png", sep = "/")))
  print(gc())
  .ok("10: noaa_gmsl_seg")
}, error = function(e) .fail("10: noaa_gmsl_seg", e))
# ```

# ```r
########################################
## Has sea-level rise accelerated?
## V. Church & White 2011, dx.doi.org/10.1007/s10712-011-9119-1.
## V. https://tamino.wordpress.com/2017/10/24/what-is-sea-level-up-to-lately
########################################
cat("\n── Block 11: CSIRO + AVISO composite GMSL ──\n")
tryCatch({
  inst <- get_climate_data(download = FALSE, baseline = FALSE)
  slr_series <- c("CSIRO Reconstructed Global Mean Sea Level", "AVISO Global Mean Sea Level")
  slr <- purrr::reduce(
    list(
      inst,
      remove_periodic_cycle(inst, slr_series[1], center = FALSE, keep_series = FALSE,
        suffix = " (non-seasonal)"),
      remove_periodic_cycle(inst, slr_series[2], center = FALSE, keep_series = FALSE,
        suffix = " (non-seasonal)")
    ), dplyr::full_join) %>%
    dplyr::mutate(yr_part = year + (2 * month - 1)/24, .after = "month") %>%
    dplyr::arrange(year, month)
  slr_baseline <- 1993:2013
  slr <- create_aggregate_variable(slr, c("CSIRO Reconstructed Global Mean Sea Level (non-seasonal)",
    "AVISO Global Mean Sea Level (non-seasonal)"), "Global Mean Sea Level Aggregate",
    type = "head", baseline = slr_baseline)

  sm <- fit_segmented_model(oss(slr, "Global Mean Sea Level Aggregate"),
    "Global Mean Sea Level Aggregate", yearly = TRUE, breakpoints... = list(h = 36, breaks = NULL))

  slr_cols <- c("#1F78B4", "#33A02C")
  slr_ylab <- sprintf("Global Mean Sea Level (mm) w.r.t %s–%s", min(slr_baseline), max(slr_baseline))
  slr_main <- "Composite GMSL (Reconstruction + Satellite Altimetry)"
  slr_end_callback <- expression({
    plot(sm$piecewise[["Global Mean Sea Level Aggregate"]]$sm, col = scales::alpha("red", 0.4),
      add = TRUE, rug = FALSE)
    psi <- sprintf(sm$piecewise[["Global Mean Sea Level Aggregate"]]$sm$psi[, 2], fmt = "%1.1f")
    vline(psi)
    ptbl <- segmented::slope(sm$piecewise[["Global Mean Sea Level Aggregate"]]$sm)$year %>%
      apply(2, sprintf, fmt = "%1.2f")
    colnames(ptbl)[1] <- "Rate (mm/y)"
    yr <- sm$piecewise[["Global Mean Sea Level Aggregate"]]$sm$model$year
    rownames(ptbl) <- c(min(yr), sort(rep(psi %>% as.numeric %>% round, 2)), max(yr)) %>%
      keystone::chunk(2) %>% sapply(paste, collapse = "–")
    ptbl %>% plotrix::addtable2plot(x = 1940, y = -200, table = ., cex = 0.8, bg = "lightgray",
      display.rownames = TRUE)
  })
  plot_climate_data(slr, series = paste(slr_series, "(non-seasonal)"), yearly = TRUE,
    baseline = slr_baseline, conf_int = TRUE, col = slr_cols, lwd = 2, main = slr_main,
    ylab = slr_ylab, ylim = NULL, end_callback = slr_end_callback, save_png = TRUE,
    png... = list(filename = paste(save_png_dir, "csiro-reconstructed-gmsl-aviso_1880.1-recent_yearly_seg.png", sep = "/")))
  .ok("11: csiro_aviso_composite_gmsl")
}, error = function(e) .fail("11: csiro_aviso_composite_gmsl", e))
# ```

# ```r
########################################
## Calculate global average land temperature anomalies from GHCN-m station data, w/ 95% CI.
## SOP:
## * For each station/month, calc 1951-1980 baseline
## * Compute temp anomalies by subtracting station/month baseline from absolute temps
## * Split region into lat-long grid
## * For each grid cell, get monthly avg of all anomalies
## * Lat-weight each cell & calc monthly region avg
########################################
cat("\n── Blocks 12a-12c: GHCN-m gridded land temperature ──\n")
tryCatch({
  ## If 'download' = TRUE, import & prep data from https://www.ncei.noaa.gov/pub/data/ghcn/v4/
  download <- FALSE
  ghcn_v4_avg_f <- new.env()
  get_series_from_ghcn_gridded(ver = 4, temp = "avg", quality = "f", load_env = ghcn_v4_avg_f,
    download = download)

  ghcn_v4_avg_u <- new.env()
  get_series_from_ghcn_gridded(ver = 4, temp = "avg", quality = "u", load_env = ghcn_v4_avg_u,
    download = download)

  ## Select only stations (e.g.) which are longer-term over the coverage period
  coverage_years <- NULL # Default: keep all stations
  meets_filter_criteria_u <- make_coverage_filter(ghcn_v4_avg_u$ghcn, coverage_years,
    min_nonmissing_months = 12, min_nonmissing_years_prop = 0.9)
  meets_filter_criteria_u %>% table
  meets_filter_criteria_f <- make_coverage_filter(ghcn_v4_avg_f$ghcn, coverage_years,
    min_nonmissing_months = 12, min_nonmissing_years_prop = 0.9)
  meets_filter_criteria_f %>% table

  ## In the case of adj. v unadj., however, the temporal coverage is usu. different;
  ##   so for comparison, let's use the unadj. stations for both series.
  gf <-
    ghcn_v4_avg_f$ghcn %>% dplyr::select(any_of(c(get_climate_series_names(., invert = FALSE),
    meets_filter_criteria_u[meets_filter_criteria_u] %>% names)))

  lat_range <- c(90, -90); long_range <- c(-180, 180) # Default global coverage
  round_to_nearest <- NULL #1.0

  ## Supply paths to XLSX files to store portable versions of data & results:
  spreadsheet_path_f <- spreadsheet_path_u <- NULL

  grid_size <- c(5, 5)
  use_lat_zonal_weights <- TRUE

  ## Raw data
  uadj_ghcn_v4_avg_u <-
    make_ghcn_temperature_series(ghcn_v4_avg_u$ghcn, ghcn_v4_avg_u$station_metadata,
      other_filters = meets_filter_criteria_u, grid_size = grid_size, lat_range = lat_range,
      long_range = long_range, make_planetary_grid... = list(use_lat_weights = TRUE),
      use_lat_zonal_weights = use_lat_zonal_weights, uncertainty = TRUE, boot_seed = NULL,
      round_to_nearest = round_to_nearest, spreadsheet_path = spreadsheet_path_u)

  ## Homogenized/adjusted data
  adj_ghcn_v4_avg_f <-
    make_ghcn_temperature_series(gf, ghcn_v4_avg_f$station_metadata, grid_size = grid_size,
      lat_range = lat_range, long_range = long_range,
      make_planetary_grid... = list(use_lat_weights = TRUE),
      use_lat_zonal_weights = use_lat_zonal_weights, uncertainty = TRUE, boot_seed = NULL,
      round_to_nearest = round_to_nearest, spreadsheet_path = spreadsheet_path_f)

  ## Collect both data sets & equivalent official series for comparison
  inst <- get_climate_data(download = FALSE, baseline = FALSE) %>%
    { purrr::reduce(list(., adj_ghcn_v4_avg_f, uadj_ghcn_v4_avg_u), dplyr::full_join,
      by = c("month", "year")) } %>%
    dplyr::mutate(yr_part = year + (month - 0.5)/12, met_year = NA)

  series <- c(names(adj_ghcn_v4_avg_f)[3], names(uadj_ghcn_v4_avg_u)[3],
    "GISTEMP v4 Global Land")

  extra_trends <- sapply(series[1:2],
    function(a) { list(range = c(1970, current_year - 0.01), lwd = 2) }, simplify = FALSE)

  ## Plot both data sets & equivalent official series for comparison
  tryCatch({
    r <-
      plot_climate_data(inst, series = series[1:3], 1850, yearly = TRUE, baseline = 1981:2010,
        conf_int = TRUE, conf_int_series = NULL, ci_alpha = c(0.2, 0.2, 0.2), lwd = c(2, 2),
        ylim = NULL, alpha = c(1, 1, 1), trend = TRUE, trend... = list(keep_default_trends = FALSE,
        rate_expression =
          sprintf("expression(Delta ~ \"= %%+1.2f ± %%1.2f %s/dec. %s\")", "°C", "1970–" %_%
            (current_year - 1))),
        extra_trends = extra_trends, trend_legend_inset = c(0.2, 0.01),
        make_standardized_plot_filename... = list(suffix = "1970_adj-v-unadj-all"),
        save_png = TRUE,
        png... = list(filename = paste(save_png_dir, "ghcnv4-global-avg_1850.1-recent_raw-&-adj_5x5-cells_trend1970.png", sep = "/")))
    .ok("12a: ghcn_avg_temp_series")
  }, error = function(e) .fail("12a: ghcn_avg_temp_series", e))

  ## Station counts
  tryCatch({
    station_counts <- get_station_counts(x = uadj_ghcn_v4_avg_u, env = ghcn_v4_avg_u,
      region_name = "v4 Global Complete", start_year = 1850, end_year = current_year - 0.01,
      save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "ghcnv4-station-counts_1850.1-recent_global.png", sep = "/")))
    ## Max station count for a single month:
    station_counts$station_counts_series$`station count` %>% max(na.rm = TRUE) %>% print
    ## Total no. of stations used in average temp series:
    station_counts$station_series %>% get_climate_series_names %>% length %>% print
    .ok("12b: ghcn_station_counts")
  }, error = function(e) .fail("12b: ghcn_station_counts", e))

  ## Station distribution
  tryCatch({
    plot_stations_map(attr(uadj_ghcn_v4_avg_u, "filtered_metadata"), region_name = "global",
      title_text = sprintf("GHCN-m v4 global station distribution"), save_png = TRUE,
      png... = list(filename = paste(save_png_dir, "ghcnv4-station-distribution_1850.1-recent_global.png", sep = "/")))
    .ok("12c: ghcn_station_map")
  }, error = function(e) .fail("12c: ghcn_station_map", e))

}, error = function(e) {
  msg <- conditionMessage(e)
  cat(sprintf("  [SETUP FAIL blocks 12a-12c]: %s\n", msg))
  for (lbl in c("12a: ghcn_avg_temp_series", "12b: ghcn_station_counts", "12c: ghcn_station_map")) {
    .skipped[[lbl]] <<- paste("setup:", msg)
  }
})
# ```

########################################
## Final report
########################################
cat("\n\n========================================\n")
cat("HARNESS REPORT -- Pinned date: 2026-05-24\n")
cat("========================================\n\n")

cat(sprintf("Rendered (%d):\n", length(.rendered)))
for (.lbl in .rendered) cat(sprintf("  [OK] %s\n", .lbl))

cat(sprintf("\nSkipped (%d):\n", length(.skipped)))
for (.lbl in names(.skipped)) cat(sprintf("  [SKIP] %s\n    %s\n", .lbl, .skipped[[.lbl]]))

cat(sprintf("\nGolden PNGs written to: %s\n", save_png_dir))
