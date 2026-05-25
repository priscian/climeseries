#' @export
create_aggregate_variable <- function(
  x,
  var_names,
  aggregate_name = "aggregate_var",
  get_climate_series_names... = list(),
  method = "fmm",
  interpolate = TRUE,
  baseline = NULL,
  add = TRUE,
  ...
){
  get_climate_series_namesArgs <- list(
    x = x,
    invert = FALSE
  )
  get_climate_series_namesArgs <-
    utils::modifyList(get_climate_series_namesArgs,
      get_climate_series_names...,
 keep.null = TRUE
) %>%
      `$<-`(name = "invert", value = FALSE)

  colNamesAll <- do.call(get_climate_series_names, get_climate_series_namesArgs)

  d <- x %>% dplyr::select(c(colNamesAll, var_names))

  ## Put variables on common baseline before combining them
  if (!is.null(baseline))
    d <- recenter_anomalies(d, baseline = baseline)

  ## Remove non-monthly-series columns
  colNamesSeries <- do.call(
get_climate_series_names,
    get_climate_series_namesArgs %>% `$<-`(name = "invert", value = TRUE)
)
  d %<>% dplyr::select(any_of(colNamesSeries))

  if (interpolate)
    d %<>% interpNA(method = method, unwrap = TRUE)

  r <- apply(d, 1, function(a) {
 r <- NA; if (!all(is.na(a))) r <- mean(a, na.rm = TRUE); r
 })
  if (interpolate)
    r %<>% {
 drop(interpNA(., method = "linear", unwrap = TRUE, ...))
 }

  if (!add) return (r)

  x %<>% dplyr::mutate(!!aggregate_name := r)

  x
}

## usage:
# e <- get_climate_data(download=TRUE, baseline=FALSE)
# e <- create_aggregate_variable(e, c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global"), "SAOD Aggregate Global", type="head")


#' @export
create_aggregate_co2_variable <- function(x, co2_var_name, merge...=list(), ...){
  lawPath <- system.file("extdata/co2/law2006.txt", package="climeseries")
  l <- read.table(lawPath, header=TRUE, skip=182, nrow=2004)
  law <- data.frame(year=l$YearAD, month=6, `CO2 Law Dome`=l$CO2spl, check.rows=FALSE, check.names=FALSE, fix.empty.names=FALSE, stringsAsFactors=FALSE)
  law <- base::merge(expand.grid(month=1:12, year=law$year), law, by=c("year", "month"), all=TRUE)
  yearlyInstrumentalCo2 <- as.data.frame(make_yearly_data(x[, c(common_columns, co2_var_name)]))
  instrumentalStartYear <- head(yearlyInstrumentalCo2[na_unwrap(yearlyInstrumentalCo2[[co2_var_name]]), ]$year, 1)

  mergeArgs = list(
    x = x,
    y = law[law$year < instrumentalStartYear, ],
    by = intersect(common_columns, names(law)),
    all.x = TRUE
  )
  mergeArgs <- utils::modifyList(mergeArgs, merge..., keep.null = TRUE)

  ## Unlike the other aggregate variables, which use overlap means, the CO2 aggregate series has a distinct cutpoint between paleo and instrumental.
  x <- do.call("merge", mergeArgs)

  r <- create_aggregate_variable(x, c("CO2 Law Dome", co2_var_name), ...)
  ## Replace truncated Law Dome series with the full one.
  r$`CO2 Law Dome` <- NULL

  mergeArgs$x <- r
  mergeArgs$y <- law
  r <- do.call("merge", mergeArgs)

  r
}
## usage:
# e <- get_climate_data(download=FALSE, baseline=FALSE)
# e <- create_aggregate_co2_variable(e, "CO2 Mauna Loa", aggregate_name="CO2 Aggregate Global", type="head") # With interpolation.
## Create a yearly aggregate CO2 variable without any monthly interpolation.
# e <- get_climate_data(download=FALSE, baseline=FALSE)
# e <- create_aggregate_co2_variable(e, "CO2 Mauna Loa", aggregate_name="CO2 Aggregate Global", merge...=list(all=TRUE), interpolate=FALSE)
# g <- make_yearly_data(e[, c(climeseries::common_columns, "CO2 Aggregate Global")])


#' @export
add_default_aggregate_variables <- function(x, co2_instrumental_variable = "CO2 Mauna Loa", use_adjusted_tsi = TRUE, ...) # Use 'interpolate = FALSE' as needed
{
  x <- create_aggregate_variable(x, c("Extended Multivariate ENSO Index", "Multivariate ENSO Index"), "MEI Aggregate Global", type = "head", ...)
  x <- create_aggregate_variable(x, c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global"), "SAOD Aggregate Global", type = "head", ...)

  ## TSI
  if (use_adjusted_tsi) {
    ## "PMOD TSI VIRGO A+B (new)" is shaped very much like "TSI Reconstructed" but shifted downwards a bit;
    ## so, shift it up and fill in the monthly details missing from "TSI Reconstructed".
    flit <- make_yearly_data(x[, c(common_columns, "PMOD TSI VIRGO A+B (new)", "TSI Reconstructed")])
    tsiDifference <- flit[[2]] - flit[[3]]
    x$`PMOD TSI VIRGO A+B (new adj.)` <- x$`PMOD TSI VIRGO A+B (new)` - mean(tsiDifference, na.rm = TRUE)

    x <- create_aggregate_variable(x, c("TSI Reconstructed", "PMOD TSI VIRGO A+B (new adj.)"), "TSI Aggregate Global", type = "head", ...)
  } else { # Otherwise, for less monthly detail and less interpolation, just use "Reconstructed" and SORCE.
    x <- create_aggregate_variable(x, c("TSI Reconstructed", "PMOD TSI VIRGO A+B (new)"), "TSI Aggregate Global", type = "head", ...)
  }

  aggregateName <- "CO2 Aggregate Global"
  x <- create_aggregate_co2_variable(x, co2_instrumental_variable, aggregate_name = aggregateName %_% " (interp.)", type = "head", ...)
  #x[["log " %_% aggregateName %_% " (interp.)"]] <- 5.35 * log(x[[aggregateName %_% " (interp.)"]] / 280) # A test.
  x[["log " %_% aggregateName %_% " (interp.)"]] <- log(x[[aggregateName %_% " (interp.)"]])
  x$`CO2 Law Dome` <- NULL
  x <- create_aggregate_co2_variable(x, co2_instrumental_variable, aggregate_name = aggregateName, interpolate = FALSE)
  x[["log " %_% aggregateName]] <- log(x[[aggregateName]])

  x
}

## usage:
# e <- get_climate_data(download=FALSE, baseline=FALSE)
# e <- add_default_aggregate_variables(e)
# plot_climate_data(e, c("Extended Multivariate ENSO Index", "Multivariate ENSO Index", "MEI Aggregate Global"), 1940, lwd = 2)
# plot_climate_data(e, c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global", "SAOD Aggregate Global"), 1985, lwd = 2)
# plot_climate_data(e, c("TSI Reconstructed", "PMOD TSI (new VIRGO)", "TSI Aggregate Global"), 1985, lwd = 2)


#' @export
interpolate_baseline <- function(
  series, # A single column in 'x'
  x, # A 'climeseries' data set
  baseline = NULL
){
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)

  if (is.null(series) || length(series) > 1) {
    return (interpolate_baselines(series, x, baseline))
  }

  series <- series[1]
  xu <- x[na_unwrap(x[[series]]), c(common_columns, series)]

  if (!is.null(baseline)) {
    if (min(baseline) < min(xu$year)) {
      xx <- x[, c(common_columns, series)] %>%
        dplyr::filter(year >= min(baseline))

      is_na <- is.na(xx[[series]])
      m <- stats::lm(substitute(s ~ yr_part, list(s = as.name(series))), data = x)
      ## Calculate linear prediction back to start of baseline (don't go back further than about 1970).
      xx[, series][is_na] <- stats::predict(m, dataframe(yr_part = xx$yr_part))[is_na]
      xxx <- recenter_anomalies(xx, baseline)
      is.na(xxx[, series]) <- is_na

      r <- merge(x[, common_columns], xxx[, c("year", "month", series)], all.x = TRUE)
    } else {
      r <- recenter_anomalies(x[, c(common_columns, series)], baseline)
    }
  } else {
    r <- x[, c(common_columns, series)]
  }

  r
}


interpolate_baselines <- function(
  series = NULL, # A vector of column names in 'x', NULL for all columns
  x, # A 'climeseries' data set
  baseline = NULL # Single baseline, but multiples might be allowed later
){
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)

  if (is.null(series))
    series <- get_climate_series_names(x)

  plyr::l_ply(series, function(s) {
 x[, s] <<- interpolate_baseline(s, x, baseline)[, s]
 })

  x
}


## First download CMIP5 TAZ, where e.g. "/mnt/v/data/climate/CMIP5-taz" is your directory:
## wget --user-agent=Mozilla --no-check-certificate -r -np -nd -l 1 -A nc,NC -H -P /mnt/v/data/climate/CMIP5-taz -erobots=off https://climexp.knmi.nl/CMIP5/monthly/taz/
#' @export
create_loess_variables <- function(inst, series, loess... = list(), unwrap = TRUE, keep_interpolated = FALSE, ...){
  yearVar <- ifelse(is.null(inst$month), "year", "yr_part")

  baselineAttribute <- attr(inst, "baseline")

  d <- inst[, c(names(inst)[names(inst) %in% common_columns], series)]
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series]))

  for (i in series) {
    d[[i %_% " (interpolated)"]] <- drop(interpNA(d[, i], "fmm"))

    loessArgs = list(
      formula = eval(substitute(s ~ yr_part, list(s = as.name(i %_% " (interpolated)"), yr_part = as.name(yearVar)))),
      data = d,
      span = NULL # Perform best fit to data (was 'span = 0.2')
    )
    loessArgs <- utils::modifyList(loessArgs, loess..., keep.null = TRUE)

    #l <- do.call("loess", loessArgs) # Removes NAs, so attend to it.
    l <- do.call(LOESS, loessArgs) # Removes NAs, so attend to it.
    lContext <- d[[i %_% " (interpolated)"]]
    lContext[!is.na(lContext)] <- l$fit
    d[[i %_% " (LOESS fit)"]] <- lContext

    if (!keep_interpolated)
      d[[i %_% " (interpolated)"]] <- NULL
  }

  attr(d, "baseline") <- baselineAttribute
  d
}


#' @export
add_loess_variables <- function(inst, series, ...){
  d <- create_loess_variables(inst, series, ...)
  baselineAttribute <- attr(inst, "baseline")
  r <- base::merge(inst, d[, setdiff(names(d), series)], by = names(d)[names(d) %in% common_columns], all.x = TRUE)

  attr(r, "baseline") <- baselineAttribute
  r
}

## usage:
# series <- c("GISTEMP Zonal 64N-90N", "GISTEMP Zonal 44N-64N", "GISTEMP Zonal 24N-44N", "GISTEMP Zonal EQU-24N", "GISTEMP Zonal 24S-EQU", "GISTEMP Zonal 44S-24S", "GISTEMP Zonal 64S-44S", "GISTEMP Zonal 90S-64S")
# d <- get_climate_data(download=FALSE, baseline=TRUE)
# g <- add_loess_variables(d, series, loess...=list(span=0.4))
# plot_climate_data(g, series %_% " (LOESS fit)")


