#' @export
correlate_co2_temperature <- function(series, start_year=1880, end_year=current_year - 1, data, ylab, main_base="Temperature", text_x=380, text_y=-0.4, baseline=FALSE, download=FALSE)
{
  if (missing(data))
    e <- get_climate_data(download=download, baseline=baseline)
  else
    e <- data

  dm1 <- data.matrix(e[e$year %in% ifelse(start_year < 1958, 1958, start_year):end_year, c(series, "CO2 Mauna Loa")])

  colnames(dm1) <- c("temp", "co2")
  row.names(dm1) <- apply(e[e$year %in% ifelse(start_year < 1958, 1958, start_year):end_year, c("year", "month")], 1, paste, collapse=".")

  lawPath <- system.file("extdata/co2/law2006.txt", package="climeseries")
  law <- read.table(lawPath, header=TRUE, skip=182, nrow=2004)

  dm <- dm1
  if (start_year < 1958) {
    l <- law[law$YearAD %in% start_year:1957, c("CO2spl")]
    flit <- e[e$year %in% start_year:1957, c("year", series)]
    flitDm <- tapply(flit[, 2], flit[, 1], mean)
    dm0 <- data.matrix(data.frame(temp=flitDm, co2=l))

    dm <- rbind(dm0, dm1)
  }

  r <- cor(dm[, 1], dm[, 2], use="pairwise.complete.obs")
  #r^2

  plot(as.numeric(row.names(dm)), dm[, 1])
  plot(as.numeric(row.names(dm)), dm[, 2])

  xlab <- eval(substitute(expression(paste("Atmospheric CO", phantom()[2], " (PPM)", sep=""))))
  if (missing(ylab))
    ylab <- eval(substitute(expression(paste(series, " Temp. Anomaly (", phantom(l) * degree, "C)", sep="")), list(series=as.symbol(series))))
  main <- eval(substitute(expression(paste(main_base, " vs. CO", phantom()[2], " (", startYear, "\u2013", endYear, ")", sep="")), list(endYear=as.symbol(end_year), startYear=as.symbol(start_year), main_base=as.symbol(main_base))))

  plot(dm[, 2], dm[, 1], ylab=ylab, xlab=xlab, main=main)
  m <- lm(dm[, 1] ~ dm[, 2])
  #summary(m)
  abline(coef(m)[1], coef(m)[2], col="red", lwd=2)

  r2Text <- eval(substitute(expression(paste("R", phantom()^2, " = ", v, sep="")), list(v=sprintf(r^2, fmt="%1.2f"))))
  text(text_x, text_y, r2Text)

  list(series=series, data=dm, model=m)
}

## usage:
# [File: "HadCRUT4-vs-CO2_1850-2015.png"]
# rv <- correlate_co2_temperature("HadCRUT4 Global", 1850)
# [File: "HadCRUT4-vs-CO2_1970-2015.png"]
# rv <- correlate_co2_temperature("HadCRUT4 Global", 1970)
# [File: "GISTEMP-vs-CO2_1880-2015.png"]
# rv <- correlate_co2_temperature("GISTEMP Global", 1880)
# [File: "RATPAC-A 850-300 mb-vs-CO2_1958-2015.png"]
# rv <- correlate_co2_temperature("RATPAC-A 850-300 mb Global", 1958)
# [File: "RSS TLT 3.3-vs-CO2_1979-2015.png"]
# rv <- correlate_co2_temperature("RSS TLT 3.3 -70.0/82.5", 1979)


#' @export
#' @import data.table
plot_horse_race <- function(series, top_n_years=NULL, baseline=TRUE, size=1)
{
  inst <- get_climate_data(download=FALSE, baseline=baseline)

  d <- inst[, c("year", "month", series)]
  d <- subset(d, na_unwrap(d[, series]))
  d1 <- tbl_dt(dcast(d, year ~ month, value.var=series))
  d2 <- data.table::copy(d1)
  ## Calculate cumulative average by row.
  d2[, names(d2[, !1, with=FALSE]) := as.list((function(x) { cumsum(as.matrix(x)[1, ]) / seq_along(x) })(.SD)), .SDcols=names(d2[, !1, with=FALSE]), by=1:nrow(d2)]
  ## Melt data set for plotting.
  d3 <- dplyr::arrange(data.table::melt(d2, id.vars=c("year"), variable.name="month", value.name="YTD mean temp."), year, month)
  d4 <- data.table::copy(d2)
  d4[, `latest YTD mean temp.` := as.list((function(x) { y <- as.matrix(x)[1, ]; tail(y[!is.na(y)], 1) })(.SD)), .SDcols=names(d4[, !1, with=FALSE]), by=1:nrow(d4)]
  d4 <- dplyr::arrange(d4[, .(year, `latest YTD mean temp.`)], dplyr::desc(`latest YTD mean temp.`))

  ## Top n warmest years:
  if (!is.null(top_n_years)) {
    if (top_n_years < 0)
      d4 <- tail(d4, -top_n_years)
    else
      d4 <- head(d4, top_n_years)

    d3 <- d3[year %in% d4$year, ]
  }

  baselineText <- ""
  if (is.logical(baseline)) {
    if (baseline) baseline <- defaultBaseline
    else baseline <- NULL
  }
  if (!is.null(baseline))
    baselineText <- " w.r.t. " %_% min(baseline) %_% "\u2013" %_% max(baseline)

  subtitle <- paste(series, " ", min(d$year), "\u2013", max(d$year), sep="")
  ylab <- eval(substitute(expression(paste("Temperature Anomaly (", phantom(l) * degree, "C)", b, sep="")), list(b=baselineText)))
  g <- ggplot(d3, aes(x=month, y=`YTD mean temp.`, group=factor(year), color=factor(year))) +
    geom_line(size=size) +
    scale_colour_discrete(guide="none") +
    scale_x_discrete(expand=c(0, 1)) +
    directlabels::geom_dl(aes(label=year), method = list(directlabels::dl.trans(x=x + 0.2), "last.points", cex = 0.8)) +
    # coord_cartesian(ylim(c(-4, 4)) + # No clipping.
    labs(list(title="Year-to-Date Temperature Anomalies", subtitle=subtitle, y=ylab))

  print(g)

  return(d4)
}

## usage:
# ytd <- plot_horse_race("GISTEMP Global")
# ytd <- plot_horse_race("UAH TLT 6.0 Global", 10)
# ytd <- plot_horse_race("NCEI US Avg. Temp.", 10) # Use -10 for bottom 10 years.
# print(as.data.frame(ytd), digits=3, row.names=FALSE, right=FALSE)


#' @export
get_yearly_gistemp <- function(series="GISTEMP Met. Stations Oct. 2005", uri="https://web.archive.org/web/20051029130103/http://data.giss.nasa.gov/gistemp/graphs/Fig_A.txt", skip=0L)
{
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  x <- NULL

  gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

  tryCatch({
    r <- httr::content(httr::GET(uri), "text", encoding="ISO-8859-1")
    tab <- gsub("^(?!\\s*\\d{4}\\s+).*$", "", strsplit(r, '\n')[[1L]], perl=TRUE)
    x <- read.table(text=tab, header=FALSE, as.is=TRUE, na.strings=c("*", "**", "***", "****"), skip=skip, check.names=FALSE)
  }, error=Error, warning=Error)

  d <- cbind(data.frame(year=x$V1, month=6, check.names=FALSE, stringsAsFactors=FALSE), temp=x$V2)
  d <- base::merge(expand.grid(month=1:12, year=d$year), d, by=c("year", "month"), all=TRUE)
  d$yr_part <- d$year + (2 * d$month - 1)/24

  names(d)[names(d) == "temp"] <- series

  return (d)
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=FALSE)
# allSeries <- list(
#   inst,
#   get_yearly_gistemp("GISTEMP Global Met. Stations Oct. 2005", "https://web.archive.org/web/20051029130103/http://data.giss.nasa.gov/gistemp/graphs/Fig_A.txt"),
#   get_yearly_gistemp("GISTEMP Global Met. Stations Sep. 2015", "https://web.archive.org/web/20150918040726/http://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A.txt"),
#   get_yearly_gistemp("GISTEMP Global Met. Stations Apr. 2016", "https://web.archive.org/web/20160419081141/http://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A.txt"),
## N.B. The following won't work any more since changes to NASA's Web site:
#   get_yearly_gistemp("GISTEMP Global Met. Stations Current", "http://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A.txt")
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries::common_columns), lapply(allSeries, names))))), allSeries)
# d <- recenter_anomalies(d, 1951:1980) # Should be the same baseline, but make sure.
# series <- sapply(allSeries[-1], get_climate_series_names)
# plot_climate_data(d, series=series, start=1994, ma=NULL, lwd=2, conf_int=FALSE, show_trend=TRUE)
## N.B 'show_trend=TRUE' requires that the column 'met_year' be in the data frame 'x'. Fix this!


#' @export
get_old_monthly_gistemp <- function(series="GISTEMP Global Nov. 2015", uri="http://web.archive.org/web/20151218065405/http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt", skip=0L)
{
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  x <- NULL

  skip <- skip # Skip over notes at start of data.
  gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

  #tryCatch({
    r <- httr::content(httr::GET(uri), "text", encoding="ISO-8859-1")
    r <- gsub("*****", " ****", r, fixed=TRUE)
    r <- gsub("****", " ***", r, fixed=TRUE)
    r <- gsub("^\\D+.*$", "", strsplit(r, '\n')[[1L]], perl=TRUE)
    x <- read.table(text=r, header=FALSE, as.is=TRUE, na.strings=c("*", "**", "***", "****"), skip=skip, check.names=FALSE)
  #}, error=Error, warning=Error)

  flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
  for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
  flit <- dplyr::arrange(flit, V1, month)

  d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
  d$temp <- d$temp / 100

  names(d)[names(d) == "temp"] <- series

  return (d)
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=FALSE)
# climeseriesDataPath <- "C:/common/data/climate/climeseries"
# env <- new.env()
# load(paste(climeseriesDataPath, "climate-series_raw_20160711.RData", sep="/"), envir=env) # Get GISTEMP Global data from climeseries archive.
# env$d$`GISTEMP Global May 2016` <- env$d$GISTEMP
# allSeries <- list(
#   inst,
#   get_old_monthly_gistemp("GISTEMP Global Nov. 2005", "http://web.archive.org/web/20051227031241/http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt"),
#   env$d[, c(climeseries::common_columns, "GISTEMP Global May 2016")],
#   get_old_monthly_gistemp()
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries::common_columns), lapply(allSeries, names))))), allSeries)
# d <- recenter_anomalies(d, 1951:1980) # Should be the same baseline, but make sure.
# series <- c("GISTEMP Global Nov. 2005", "GISTEMP Global Nov. 2015", "GISTEMP Global May 2016", "GISTEMP Global")
# plot_climate_data(d, series=series, ma=12, lwd=2, conf_int=FALSE, show_trend=TRUE)
#
## Even older e.g.:
# d1 <- get_old_monthly_gistemp("GISTEMP Global Land Dec. 1998", "http://web.archive.org/web/19990220235952/http://www.giss.nasa.gov/data/gistemp/GLB.Ts.txt")
# d2 <- get_old_monthly_gistemp("GISTEMP Global Dec. 2001", "https://web.archive.org/web/20020122065805/http://www.giss.nasa.gov/data/update/gistemp/GLB.Ts+dSST.txt")


satelliteSlrBaseUrl <- "http://sealevel.colorado.edu/cgi-bin/table.cgi?q=content%2Finteractive-sea-level-time-series-wizard&dlat=@@LAT@@&dlon=@@LON@@&fit=s&smooth=n&days=0"

#' @export
get_satellite_slr <- function(lat, lon) # +lat N of the equator, -lon W of the prime meridian.
{
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  skip <- 1L

  x <- NULL

  uri <- sub("@@LAT@@", lat, sub("@@LON@@", lon, satelliteSlrBaseUrl))

  tryCatch({
    ## Scrape Web page for data.
    webPage <- httr::content(httr::GET(uri), "text", encoding="ISO-8859-1")
    webPage <- readLines(tc <- textConnection(webPage)); close(tc)
    pageTree <- htmlTreeParse(webPage, useInternalNodes=TRUE)
    ## The data table is in a PRE node (the only one, hopefully).
    pre <- XML::xpathSApply(pageTree, "//*/pre", xmlValue)
    ## Prepare the table text.
    tab <- strsplit(pre, '\n')[[1L]]
    tab <- tab[tab != ""]
    x <- read.table(text=tab, header=FALSE, skip=skip, fill=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
  }, error=Error, warning=Error)

  x$V2 <- as.numeric(x$V2) * 10 # Convert to mm.

  names(x) <- c("Year", "Sea Level (mm)")

  x
}

## usage:
# d <- get_satellite_slr(lat=26, lon=-80) # Off the coast of Miami, FL.


tidegaugeSlrBaseUrl <- "http://www.psmsl.org/data/obtaining/rlr.monthly.data/@@STATION_ID@@.rlrdata"
## List of IDs: http://www.psmsl.org/data/obtaining/

#' @export
get_tidegauge_slr <- function(station_id)
{
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  skip <- 1L

  x <- NULL

  uri <- sub("@@STATION_ID@@", station_id, tidegaugeSlrBaseUrl)

  tryCatch({
    x <- read.csv2(uri, header=FALSE, fill=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
  }, error=Error, warning=Error)

  x <- x[, 1:2]
  x$V2 <- as.numeric(x$V2)
  is.na(x$V2) <- x$V2 == -99999

  names(x) <- c("Year", "Sea Level (mm)")

  x
}

## usage:
# d <- get_tidegauge_slr(station_id=1858) # E.g. Virginia Key, E of Miami, FL.


## Based on the technique described at https://tamino.wordpress.com/2012/01/08/trend-and-cycle-together/.
#' @export
remove_periodic_cycle <- function(inst, series, center=TRUE, period=1, num_harmonics=4, loess...=list(), unwrap=TRUE, keep_series=TRUE, keep_interpolated=FALSE, keep_loess=FALSE, is_unc=FALSE, unc_suffix="_uncertainty", fit_unc=FALSE, ...)
{
  uncertaintyDf <- NULL

  if (!is_unc && !is_invalid(inst[[series %_% unc_suffix]])) {
    ## Get all arguments of this function to pass on for recursion.
    recursiveArgs <- get_all_args()
    recursiveArgs$center <- FALSE
    recursiveArgs$is_unc <- TRUE

    uncertaintyDf <- do.call(remove_periodic_cycle, recursiveArgs)
  }

  d <- inst[, c(common_columns, series %_% ifelse(!is_unc, "", unc_suffix))]
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series %_% ifelse(!is_unc, "", unc_suffix)]))
  if (is_unc && !fit_unc) {
    d[[series %_% " (anomalies)" %_% unc_suffix]] <- d[[series %_% unc_suffix]]

    return (d)
  }

  d[[series %_% " (interpolated)" %_% ifelse(!is_unc, "", unc_suffix)]] <- drop(interpNA(d[, series %_% ifelse(!is_unc, "", unc_suffix)], "fmm"))

  if (is.null(period)) { # Estimate period from data.
    spectralDensity <- spectrum(y)
    period <- 1 / spectralDensity$freq[spectralDensity$spec == max(spectralDensity$spec)]
  }

  ## Get residuals from LOESS fit.
  loessArgs = list(
    formula = eval(substitute(s ~ yr_part, list(s=as.name(series %_% " (interpolated)" %_% ifelse(!is_unc, "", unc_suffix))))),
    data = d,
    span = 0.2
  )
  loessArgs <- modifyList(loessArgs, loess...)

  l <- do.call("loess", loessArgs)
  if (keep_loess)
    d[[series %_% " (LOESS fit)" %_% ifelse(!is_unc, "", unc_suffix)]] <- l$fit
  r <- l$resid

  ## Construct model formula for given no. of harmonics.
  fBase <- "r ~ "; f <- NULL
  for (i in seq(num_harmonics))
    f <- c(f, paste0(c("sin", "cos"), paste0("(", 2 * i, " * pi / period * yr_part)")))
  f <- as.formula(paste0(fBase, paste0(f, collapse=" + ")))

  rfit <- lm(f, data=d, ...)
  uncycled <- d[[series %_% ifelse(!is_unc, "", unc_suffix)]] - rfit$fit

  if (is.logical(center))
    d[[series %_% " (anomalies)" %_% ifelse(!is_unc, "", unc_suffix)]] <- scale(uncycled, center=center, scale=FALSE)
  else
    d[[series %_% " (anomalies)" %_% ifelse(!is_unc, "", unc_suffix)]] <- uncycled - mean(uncycled[d$year %in% center], na.rm=TRUE)

  if (!keep_series)
    d[[series %_% ifelse(!is_unc, "", unc_suffix)]] <- NULL

  if (!keep_interpolated)
    d[[series %_% " (interpolated)" %_% ifelse(!is_unc, "", unc_suffix)]] <- NULL

  if (!is.null(uncertaintyDf))
    d <- merge(d, uncertaintyDf[c("yr_part", get_climate_series_names(uncertaintyDf, conf_int=TRUE))], all.x=TRUE, by="yr_part", sort=TRUE)

  d
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=FALSE)
# series <- "CO2 Mauna Loa"
# d <- remove_periodic_cycle(inst, series, center=2000:2012)
# plot(d$yr_part, d[, series %_% " (anomalies)"], type="o", pch=20, xlim=c(2000, 2012), ylim=c(-15, 15))
# ## Temperature series.
# inst <- get_climate_data(download=FALSE, baseline=TRUE)
# series <- "GISTEMP Global"
# ## To calculate Fourier series only on a specific time range, subset the data first, e.g. 'subset(inst, inst$year %in% 1970:2016)':
# d <- remove_periodic_cycle(subset(inst, inst$year %in% 1970:2016), series, center=climeseries:::defaultBaseline)
# plot(d$yr_part, d[, series %_% " (anomalies)"], type="o", pch=20)
# ## Monthly anomalies are almost the same because trend and cycle are very different in size.
# lines(d$yr_part, d[, series], type="o", pch=20, col="red")


#' @export
create_aggregate_variable <- function(x, var_names, aggregate_name="aggregate_var", interpolate=TRUE, add=TRUE, ...)
{
  d <- x[, var_names]
  if (interpolate)
    d <- interpNA(d, method="fmm", unwrap=TRUE)

  r <- apply(d, 1, function(a) { r <- NA; if (!all(is.na(a))) r <- mean(a, na.rm=TRUE); r })
  if (interpolate)
    r <- drop(interpNA(r, method="linear", unwrap=TRUE, ...))

  if (!add) return (r)

  x[[aggregate_name]] <- r

  x
}

## usage:
# e <- get_climate_data(download=TRUE, baseline=FALSE)
# e <- create_aggregate_variable(e, c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global"), "SAOD Aggregate Global", type="head")


#' @export
create_aggregate_co2_variable <- function(x, co2_var_name, merge...=list(), ...)
{
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
  mergeArgs <- modifyList(mergeArgs, merge...)

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
add_default_aggregate_variables <- function(x, co2_instrumental_variable = "CO2 Mauna Loa", use_adjusted_tsi = TRUE)
{
  x <- create_aggregate_variable(x, c("Extended Multivariate ENSO Index", "Multivariate ENSO Index"), "MEI Aggregate Global", type = "head")
  x <- create_aggregate_variable(x, c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global"), "SAOD Aggregate Global", type = "head")

  ## TSI
  if (use_adjusted_tsi) {
    ## "PMOD TSI (new VIRGO)" is shaped very much like SORCE but shifted downwards a bit;
    ## so, shift it up and fill in the monthly details missing from "TSI Reconstructed".
    flit <- make_yearly_data(x[, c(common_columns, "PMOD TSI (new VIRGO)", "SORCE TSI")])
    tsiDifference <- flit$`PMOD TSI (new VIRGO)` - flit$`SORCE TSI`
    x$`PMOD TSI (new VIRGO adj.)` <- x$`PMOD TSI (new VIRGO)` - mean(tsiDifference, na.rm = TRUE)
    #x <- create_aggregate_variable(x, c("TSI Reconstructed", "PMOD TSI (new VIRGO)", "SORCE TSI"), "TSI Aggregate Global", type = "head")
    x <- create_aggregate_variable(x, c("TSI Reconstructed", "PMOD TSI (new VIRGO adj.)", "SORCE TSI"), "TSI Aggregate Global", type = "head")
  }
  else { # Otherwise, for less monthly detail and less interpolation, just use "Reconstructed" and SORCE.
    x <- create_aggregate_variable(x, c("TSI Reconstructed", "SORCE TSI"), "TSI Aggregate Global", type = "head")
  }

  aggregateName <- "CO2 Aggregate Global"
  x <- create_aggregate_co2_variable(x, co2_instrumental_variable, aggregate_name = aggregateName %_% " (interp.)", type = "head")
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


## Create temperature series with the influence of some exogenous factors removed.
## Based on Foster & Rahmstorf 2011, dx.doi.org/10.1088/1748-9326/6/4/044022.
#' @export
remove_exogenous_influences <- function(x, series,
  start = NULL, end = NULL,
  lags = list(`MEI Aggregate Global` = NULL, `SAOD Aggregate Global` = NULL, `TSI Aggregate Global` = NULL),
  aggregate_vars_fun = add_default_aggregate_variables,
  period = 1, num_harmonics = 4,
  max_lag = 12, bs_df = NULL, bs_degree = 3,
  suffix = " (adj.)")
{
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)

  x <- aggregate_vars_fun(x)

  if (length(lags) == 0)
    return (x)

  lagsDf <- NULL

  for (i in series) {
    startYrPart <- min(x$yr_part[na_unwrap(x[[i]])], na.rm = TRUE)
    endYrPart <- max(x$yr_part[na_unwrap(x[[i]])], na.rm = TRUE)
    if (!is.null(start)) startYrPart <- max(start, startYrPart)
    if (!is.null(end)) endYrPart <- min(end, endYrPart)

    yr_part_offset <- trunc(mean(x$yr_part[x$yr_part >= startYrPart & x$yr_part <= endYrPart]))

    ## This guess is crude, but should work okay for the instrumental temperature series.
    rangeYrPart <- endYrPart - startYrPart
    if (!is.null(bs_df))
      bsDf <- bs_df
    else {
      bsDf <- 6
      if (rangeYrPart > 50)
        bsDf <- 8
    }

    ## Construct model formula for given no. of harmonics.
    flitSeries <- x[[i]]
    x[[i]] <- interpNA(x[[i]], type = "tail")
    fBase <- backtick(i) %_% "~"; form <- NULL
    for (j in seq(num_harmonics))
      form <- c(form, paste0(c("sin", "cos"), paste0("(", 2 * j, " * pi / period * yr_part)")))
    form <- c(paste0("splines::bs(yr_part - yr_part_offset, df = ", bsDf, ", degree=", bs_degree, ")"), backtick(names(lags)), form)
    form <- as.formula(paste0(fBase, paste0(form, collapse = " + ")))

    y <- x[, c(i, "yr_part", names(lags))]
    x[[i]] <- flitSeries
    l <- expand.grid(sapply(lags, function(a) { r <- seq(0, max_lag); if (!is.null(a)) r <- a; r }, simplify = FALSE))
    aic <- apply(l, 1,
      function(a) {
        lr <- as.list(unlist(a))
        z <- shift(y, lr, roll = FALSE)
        z <- subset(z, z$yr_part >= startYrPart & z$yr_part <= endYrPart)

        ## Test the lag combinations to find the model with the lowest AIC.
        AIC(lm(form, z))
      }
    )

    lagMinAic <- as.list(unlist(l[which.min(aic)[1], , drop = FALSE]))
    z <- shift(y, lagMinAic, roll = FALSE)
    z <- subset(z, z$yr_part >= startYrPart & z$yr_part <= endYrPart)
    yr_part <- z$yr_part
    ## Interpolate exogenous variables back in time a little for long lags.
    for (j in names(lagMinAic))
      z[[j]] <- drop(interpNA(z[[j]], type = "tail"))
    m <- lm(form, z)
    mf <- model.frame(m)

    ## Check the fit:
    # plot(yr_part, mf[[1]], type="l"); lines(yr_part, m$fitted, type = "l", col = "red"); plot(m$residuals)

    partialCoefsRe <- "bs\\(yr_part"
    partialCoefs <- coef(m)[grep(partialCoefsRe, names(coef(m)))]
    partialValuesNames <- grep(partialCoefsRe, names(mf), value = TRUE)
    partialValuesList <- sapply(partialValuesNames, function(a) data.matrix(mf[[a]]), simplify = FALSE)
    partialValues <- Reduce(cbind, partialValuesList)
    partial <- (partialValues %*% partialCoefs)[, , drop = TRUE] + coef(m)["(Intercept)"]
    adj <- m$residuals + partial
    adj <- adj - mean(adj)

    flit <- dataframe(yr_part = yr_part)
    flit[[i %_% suffix]] <- adj

    lagsDf <- rbind(lagsDf, dataframe(lagMinAic))

    #browser()
    x <- merge(x, flit, by = "yr_part", all.x = TRUE)
  }

  rownames(lagsDf) <- series
  cat("Lag values (mos.) of exogenous variables for each series:", fill = TRUE)
  print(lagsDf, row.names = TRUE)
  cat(fill = TRUE)

  x
}

## usage:
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "RSS TLT 3.3 -70.0/82.5", "UAH TLT 5.6 Global")
# start <- 1979; end <- 2011
# g <- remove_exogenous_influences(series = series, start = start, end = end, max_lag = 12)
# series_all <- as.vector(rbind(series, paste(series, "(adj.)")))
# h <- make_yearly_data(g[, c(climeseries::common_columns, series_all)])
# h <- h[year >= start & year < end]
# ylab <- expression(paste("Temperature Anomaly (", phantom(l) * degree, "C)", sep=""))
# main <- "Adjusted for ENSO, Volcanic, and Solar Influences"
# if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
#   dev.new(width = 12.5, height = 7.3) # New default device of 1200 × 700 px at 96 DPI.
# for (i in series) {
#   year_range <- paste0(min(h$year), "\u2013", max(h$year))
#   plot(h$year, h[[i]], lwd = 2, pch = 19, type = "o", main = paste(i, year_range), xlab = "year", ylab = ylab)
#   plot(h$year, h[[i %_% " (adj.)"]], lwd = 2, pch = 19, type = "o", main = paste(i, year_range, main), xlab = "year", ylab = ylab)
# }


#' @export
easy_exogenous_plot <- function(series, start=NULL, end=NULL, bs_df=NULL, tamino_style=FALSE, ...)
{
  g <- remove_exogenous_influences(series=series, start=start, end=end, bs_df=bs_df)
  series_all <- as.vector(rbind(series, paste(series, "(adj.)")))
  if (!tamino_style)
    plot_climate_data(g, paste(series, "(adj.)"), start, end, ...)
  else {
    h <- make_yearly_data(g[, c(common_columns, series_all)])
    if (!is.null(start)) h <- h[year >= start]
    if (!is.null(end)) h <- h[year < end]
    ylab <- expression(paste("Temperature Anomaly (", phantom(l) * degree, "C)", sep=""))
    main <- "Adjusted for ENSO, Volcanic, and Solar Influences"
    if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
      dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.
    for (i in series) {
      keepRows <- na_unwrap(h[[i %_% " (adj.)"]])
      year_range <- paste0(min(h$year[keepRows]), "\u2013", max(h$year[keepRows]))
      plot(h$year[keepRows], h[[i]][keepRows], lwd=2, pch=19, type="o", main=paste(i, year_range), xlab="year", ylab=ylab)
      plot(h$year[keepRows], h[[i %_% " (adj.)"]][keepRows], lwd=2, pch=19, type="o", main=paste(i, year_range, main), xlab="year", ylab=ylab)
    }

    return (h) # Return the data set for reuse.
  }

  return (g) # Return the data set for reuse.
}

## usage:
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global", "BEST Global (Water Ice Temp.)", "JMA Global", "RSS TLT 3.3 -70.0/82.5", "UAH TLT 6.0 Global")
## N.B. Each of these will likely take a minute or two to run.
# g <- easy_exogenous_plot(series[c(1:3, 7:8)], 1979, 2011, ma=12, lwd=2, show_trend=TRUE, baseline=TRUE)
# g <- easy_exogenous_plot(series, ma=12, lwd=2, baseline=TRUE)
## Similar to Foster & Rahmstorf 2011:
# h <- easy_exogenous_plot(series[c(1:3, 7:8)], 1979, 2011, tamino=TRUE)
# ## Make and save some plots:
# setwd(".") # Set to desired storage location.
# png(filename="tamino-style_%03d.png", width=12.5, height=7.3, units="in", res=600)
# h <- easy_exogenous_plot(series, tamino=TRUE)
# dev.off()
## Similar to plots here: https://tamino.wordpress.com/2017/01/18/global-temperature-the-big-3/.
# setwd(".") # Set to desired storage location.
# png(filename="tamino-big-3/tamino-big-3_%03d.png", width=12.5, height=7.3, units="in", res=600)
# h <- easy_exogenous_plot(series, 1950, tamino=TRUE)
# dev.off()


## Convert Fahrenheit temperatures to Kelvin.
#' @export
fahr_to_kelvin <- function(temp)
{
  ((temp - 32) * (5/9)) + 273.15
}

## Convert Kelvin temperatures to Celsius.
#' @export
kelvin_to_celsius <- function(temp)
{
  temp - 273.15
}

## Convert Fahrenheit temperatures to Celsius.
#' @export
fahr_to_celsius <- function(temp)
{
  kelvin_to_celsius(fahr_to_kelvin(temp))
}

## Convert Celsius temperatures to Fahrenheit.
#' @export
celsius_to_fahr <- function(temp)
{
  temp * (9/5) + 32
}


## Aided by the description at http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/map/download.html.
#' @export
#' @import plyr
make_planetary_grid <- function(lat_range=c(90, -90), long_range=c(0, 0), grid_size=c(5, 5), clockwise=FALSE, reverse_long=FALSE, container=list(structure(list(c(NA)), weight=1.0)), digits=3)
{
  ## N.B. 90° N = +90° lat; 90° S = -90° lat; 180° W = -180° long; 180° E = +180° long.

  allSame <- function(x, tol=.Machine$double.eps ^ 0.5) abs(max(x) - min(x)) < tol

  if (length(grid_size) == 1L)
    grid_size <- rep(grid_size[1], 2L)
  latSize <- grid_size[1]; longSize <- grid_size[2]

  getShortArcMidpointValues <- function(r, g)
  {
    r <- sort(r)
    signr <- sign(r)
    if (allSame(signr)) {
      if (allSame(r)) mr <- r
      else
        mr <- r + c(1, -1) * (g / 2)
    }
    else {
      if (any(signr == 0)) signr[signr == 0] <- -sum(signr)
      mr <- r - signr * (g / 2)
    }

    mv <- seq(mr[1], mr[2], by=g)

    mv
  }

  latValues <- getShortArcMidpointValues(lat_range, latSize)
  if (diff(lat_range) < 0)
    latValues <- sort(latValues, decreasing=TRUE)

  getLongMidpointValues <- function(r, g, clockwise)
  {
    if ((diff(r) > 0 && !clockwise) || (diff(r) <= 0 && clockwise)) {
      mv <- getShortArcMidpointValues(r, g)
      if (diff(r) < 0)
        mv <- sort(mv, decreasing=TRUE)
    }
    else {
      mv <- c(
        sort(getShortArcMidpointValues(c(r[1], ((2 * !clockwise) - 1) * 180), g), decreasing=clockwise),
        sort(getShortArcMidpointValues(c((2 * clockwise - 1) * 180, r[2]), g), decreasing=clockwise)
      )
    }

    ## Reversing the order of long. values might sometimes be necessary for complete arcs, i.e. same start and end values.
    if (reverse_long) mv <- rev(mv)

    mv
  }

  longValues <- getLongMidpointValues(long_range, longSize, clockwise)

  g <- matrix(container, length(latValues), length(longValues), dimnames=list(round(latValues, digits), round(longValues, digits)))

  ## Add latitude-weight attributes to row elements.
  w <- cos(matrix(rep(latValues, ncol(g)), ncol=ncol(g), byrow=FALSE) * (pi / 180)) # Latitude weights.
  plyr::m_ply(expand.grid(r_=seq(nrow(g)), c_=seq(ncol(g))), function(r_, c_) attr(g[[r_, c_]], "weight") <<- w[r_, c_])

  attr(g, "grid_size") <- grid_size; names(attr(g, "grid_size")) <- c("lat", "long")
  class(g) <- "PlanetaryGrid"

  g
}

## usage:
# g <- make_planetary_grid() # Default complete globe after JMA, 90N–90S, 0W–0E.


#' @export
find_planetary_grid_square <- function(p, lat, long)
{
  if (!inherits(p, "PlanetaryGrid"))
    stop("'p' must be a \"PlanetaryGrid\" object.")

  gridLatValues <- as.numeric(rownames(p)); gridLongValues <- as.numeric(colnames(p))
  gridRow <- which.min(abs(lat - gridLatValues))
  gridCol <- which.min(abs(long - gridLongValues))

  gridSize <- attr(p, "grid_size")
  if (abs(gridLatValues[gridRow] - lat) > gridSize[1] / 2) gridRow <- NA
  if (abs(gridLongValues[gridCol] - long) > gridSize[2] / 2) gridCol <- NA

  c(row=gridRow, col=gridCol)
}

## usage:
# g <- make_planetary_grid()
# find_planetary_grid_square(g, 60.15, 110.82)


#' @export
create_osiris_daily_saod_data <- function(data_path=".", rdata_path=".", daily_filename="OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm.RData", planetary_grid=NULL, extract=FALSE)
{
  if (extract) {
    datasetPathBase <- "/HDFEOS/SWATHS/OSIRIS\\Odin Aerosol MART/"
    datasetPaths <- datasetPathBase %_% c("Data Fields/AerosolExtinction", "Geolocation Fields/" %_% c("Altitude", "Latitude", "Longitude"))
    names(datasetPaths) <- c("extinction", "alt", "lat", "long")
    dirNames <- list.dirs(data_path, recursive=FALSE)
    x <- list()
    cat(fill=TRUE)
    for (i in dirNames) {
      ## Make sure the following regex 'pattern' doesn't change or lead to mixed file versions.
      fileNames <- list.files(i, pattern="^OSIRIS-Odin_L2-Aerosol-Limb-MART", full.names=TRUE)
      x[[basename(i)]] <- list()
      for (j in fileNames) {
        re <- ".*?_(\\d{4})m(\\d{4})\\..*$"
        dateMatches <- stringr::str_match(j, re)
        yyyymmdd <- paste(dateMatches[, 2:3], collapse="")

        cat("    Processing file", basename(j), fill=TRUE); flush.console()
        #flit <- tempfile()
        flit <- j

        x[[basename(i)]][[basename(j)]] <- list()
        for (k in names(datasetPaths))
          x[[basename(i)]][[basename(j)]][[k]] <- h5read(flit, datasetPaths[k])
      }
    }

    save(x, file=paste(rdata_path, "OSIRIS-Odin_L2-Aerosol-Limb-MART_v5-07.RData", sep="/"))
  }
  else
    load(paste(rdata_path, "OSIRIS-Odin_L2-Aerosol-Limb-MART_v5-07.RData", sep="/"))

  ### Process the extinction data to calculate monthly SAOD.

  saodDaily <- NULL

  cat(fill=TRUE)
  for (i in names(x)) {
    re <- "(\\d{4})(\\d{2})"
    yearMatches <- stringr::str_match(i, re)
    yearValue <- as.numeric(yearMatches[, 2L])
    monthValue <- as.numeric(yearMatches[, 3L])
    saodDailyTemplate <- data.frame(year=yearValue, month=monthValue, day=NA, saod=NA, check.names=FALSE, stringsAsFactors=FALSE)

    for (j in seq_along(x[[i]])) {
      dayValue <- as.numeric(str_match(names(x[[i]])[j], ".*?_\\d{4}m\\d{2}(\\d{2})\\..*$")[, 2])
      cat("    Processing object", paste(i, tools::file_path_sans_ext(names(x[[i]])[j]), sep="/"), fill=TRUE); flush.console()
      extinction <- x[[i]][[j]]$extinction
      ## Missing values are given as -9999.
      is.na(extinction) <- extinction == -9999
      alt <- x[[i]][[j]]$alt
      extinction <- data.frame(extinction, check.names=FALSE, stringsAsFactors=FALSE)
      rownames(extinction) <- alt
      lat <- x[[i]][[j]]$lat
      long <- x[[i]][[j]]$long
      coords <- mapply(function(x, y) c(lat=x, long=y), lat, long, SIMPLIFY=FALSE)

      ## Get stratospheric subset of extinction values from 15–35 km. (After Sato et al. 1993 and Rieger et al. 2015; but v. Ridley et al. 2014 for including some aerosol effects below 15 km.)
      keepRows <- alt >= 15 & alt <= 35
      e <- subset(extinction, keepRows)
      for (k in seq_along(coords)) {
        attr(e[[k]], "alt") <- alt[keepRows]
        attr(e[[k]], "coords") <- coords[[k]]
      }

      gridSaod <- sapply(e,
        function(y)
        {
          r <- NA

          ## Calculate vertical column integral of aerosol extinction.
          if (!all(is.na(y))) {
            r <- integratex(attr(y, "alt"), y)$value
            ## Boucher - Atmospheric Aerosols--Properties and Climate Impacts (2015), p. 44 (Eq. 3.31):
            ## τ = τ_r × (λ / λ_r)^-α; λ = 550 nm, λ_r = 750 nm, τ_r is OSIRIS value, α = 2.3 (v. Rieger et al. 2015)
            ##   = τ_r × 2.04, where τ_r is aerosol extinction integrated from 15–35 km
            r <- r * 2.04
          }

          attr(r, "coords") <- attr(y, "coords")

          r
        }, simplify = FALSE
      )

      ## Create global grid of 5° × 5° squares and bin each SAOD value in the correct square.
      if (is.null(planetary_grid))
        g <- make_planetary_grid()
      else
        g <- planetary_grid
      dev_null <- sapply(gridSaod,
        function(y)
        {
          coords <- attr(y, "coords")
          lat <- coords["lat"]; long <- coords["long"]
          rc <- find_planetary_grid_square(g, lat, long)
          if (any(is.na(rc))) return ()
          sq <- g[[rc["row"], rc["col"]]][[1]]
          if (all(is.na(sq)))
            g[[rc["row"], rc["col"]]][[1]] <<- as.vector(y)
          else
            g[[rc["row"], rc["col"]]][[1]] <<- c(sq, as.vector(y))
        }
      )

      ## From the global grid, create a data frame of mean values for every bin and their corresponding latitude weights.
      d <- sapply(g,
        function(y)
        {
          r <- c(value=NA, weight=attr(y, "weight"))
          if (all(is.na(y[[1]]))) return (r)
          r["value"] <- mean(y[[1]], na.rm=TRUE)

          r
        }, simplify = FALSE
      )

      d <- data.matrix(Reduce(rbind, d))
      saodToday <- stats::weighted.mean(d[, "value"], d[, "weight"], na.rm=TRUE)
      is.na(saodToday) <- is.nan(saodToday)
      saodTodayDf <- saodDailyTemplate
      saodTodayDf$day <- dayValue
      saodTodayDf$saod <- saodToday

      saodDaily <- rbind(saodDaily, saodTodayDf, make.row.names=FALSE, stringsAsFactors=FALSE)
    }
  }

  saod_daily <- saodDaily
  save(saod_daily, file=paste(rdata_path, daily_filename, sep="/"))
}


#' @export
create_osiris_saod_data <- function(path=NULL, filename="OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm.RData", series_name="OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global", create_daily=FALSE, ...)
{
  if (is.null(path)) {
    if (!is.null(getOption("climeseries_saod_data_dir")))
      path <- getOption("climeseries_saod_data_dir")
    else
      path <- "."
  }
  if (create_daily)
    create_osiris_daily_saod_data(rdata_path=path, daily_filename=filename, ...)

  load(paste(path, filename, sep="/"), envir=environment())

  r <- plyr::arrange(Reduce(rbind,
    by(saod_daily, list(saod_daily$year, saod_daily$month),
      function(x) data.frame(year=x$year[1], month=x$month[1], flit=mean(x$saod, na.rm=TRUE), check.names=FALSE, stringsAsFactors=FALSE),
      simplify=FALSE)), year, month)
  r$yr_part <- r$year + (2 * r$month - 1)/24

  names(r)[names(r) %in% "flit"] <- series_name

  r
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=FALSE)
# allSeries <- list(
#   inst,
#   create_osiris_saod_data()
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries::common_columns), lapply(allSeries, names))))), allSeries)
# series <- c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global")
# plot_climate_data(d, series, start=1985, ylab="SAOD", main="Global Mean Stratospheric Aerosol Optical Depth")
## Save only OSIRIS data as CSV file.
# keepRows <- na_unwrap(d$`OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global`)
# write.csv(d[keepRows, c("year", "month", "yr_part", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global")], "./OSIRIS-SAOD_2001.11-2016.7.csv", row.names=FALSE)


#' @export
make_yearly_data <- function(x, na_rm=TRUE, unwrap=TRUE, baseline=FALSE)
{
  if (missing(x))
    x <- get_climate_data(download=FALSE, baseline=baseline)

  r <- tbl_dt(x)[, lapply(.SD, function(a) { r <- NA_real_; if (!all(is.na(a))) r <- mean(a, na.rm=na_rm); r }), .SDcols=-common_columns[common_columns %nin% "year"], by=year]
  if (unwrap)
    r <- r[na_unwrap(r), ]

  r
}

## usage:
## Reproduce a plot here: https://tamino.wordpress.com/2017/01/01/tony-hellers-snow-job/.
# g <- make_yearly_data(na_rm=FALSE) # Allow NA values for 'mean()'; possibly better for very seasonally sensitive series.
# series <- "Rutgers NH Snow Cover"
# h <- eval(substitute(g[na_unwrap(SERIES)][year >= min(year) & !is.na(SERIES)], list(SERIES=as.name(series))))
# plot(h$year, h[[series]]/1e6, lwd=2, pch=19, type="o")


#' @export
get_yearly_difference <- function(series, start, end=current_year - 1, data, digits = 3, unit = "\u00b0C", loess = FALSE, ...)
{
  if (missing(data))
    data <- get_climate_data(download = FALSE, baseline = FALSE)

  if (loess)
    data <- add_loess_variables(data, series, ...)

  g <- make_yearly_data(data)
  h <- g[year %in% c(start, end), series %_% ifelse(loess, " (LOESS fit)", ""), with = FALSE]

  ## N.B. Use e.g. stringi::stri_escape_unicode("°") to get Unicode value(s) easily.
  cat("Difference in ", unit ," from ", start, "\u2013", end, sep = "", fill = TRUE)
  print(t(h[2] - h[1]), digits = digits, row.names = FALSE)
  cat(fill = TRUE)
  cat("Decadal rate in ", unit ,"/dec. from ", start, "\u2013", end, sep = "", fill = TRUE)
  print(10 * t(h[2] - h[1]) / (end - start), digits = digits, row.names = FALSE)

  attr(h, "range") <- c(start=start, end=end)

  return (h)
}

## usage:
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "BEST Global (Air Ice Temp.)")
# ytd <- get_yearly_difference(series, 1880)
# ytd <- get_yearly_difference(series, 1880, loess=TRUE)
# ytd <- get_yearly_difference(series, 1880, loess=TRUE, loess...=list(span=0.4))
# ytd <- get_yearly_difference(series, 1970)


## Make "cranberry plots" à la http://variable-variability.blogspot.com/2017/01/cherry-picking-short-term-trends.html.
#' @export
make_vv_cranberry_plot <- function(x, series, start, end, ylab, span=0.2)
{
  if (missing(x)) g <- make_yearly_data()
  else g <- make_yearly_data(x)

  if (!missing(start)) g <- g[year >= start]
  if (!missing(end)) g <- g[year <= end]

  if (missing(ylab))
    ylab <- expression(paste("Temperature Anomaly (", phantom(l) * degree, "C)", sep=""))

  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=9, height=8) # New default device.

  for (i in series) {
    h <- g[, c("year", i), with=FALSE]
    h <- h[na_unwrap(h[[i]])]

    layout(matrix(c(1, 2)))

    ## Plot the time series.
    plot(h$year, h[[i]],
      lwd = 2, type = "l", col = "gray",
      main = series, xlab = "year", ylab = ylab,
      panel.first=(function(){ grid(); abline(h=0.0) })(),
      panel.last=points(g$year, g[[i]], pch=19, col="red"))
    l <- loess(h[[i]] ~ h$year, span=span)
    lines(l$x, l$fitted, lwd=3, col="blue", type="l")

    ## Plot the LOESS residuals.
    plot(l$x, l$residuals, lwd=2,
      type="l", col="gray",
      main = "Standard Deviation = " %_% sprintf(sd(l$residuals, na.rm=TRUE), fmt="%.3f"), xlab = "year", ylab = ylab,
      panel.last=points(l$x, l$residuals, pch=19, col="red"))
  }

  return (nop())
}

## usage:
# series <- c("BEST Global (Water Ice Temp.)", "UAH TLT 6.0 Global")
# g <- remove_exogenous_influences(series=series)
# make_vv_cranberry_plot(g, series[1], start=1880, span=0.3)
# make_vv_cranberry_plot(g, series[1] %_% " (adj.)", start=1880, span=0.3)
# make_vv_cranberry_plot(g, series[2], span=0.9)
# make_vv_cranberry_plot(g, series[2] %_% " (adj.)", span=0.9)


## Basically a "show hottest year" function, but slightly configurable.
#' @export
show_single_value <- function(series, baseline=TRUE, data, fun=which.max, ..., value_name="temp anom. (\u00b0C)", digits=3)
{
  if (missing(data))
    data <- get_climate_data(download=FALSE, baseline=baseline)

  baseline <- attr(data, "baseline")
  g <- make_yearly_data(data)[, c("year", series), with=FALSE]

  single <- t(sapply(series,
    function(a)
    {
      m <- fun(g[[a]], ...)
      r <- data.frame(year=g$year[m], check.names=FALSE)
      r[[value_name]] <- g[[a]][m]

      r
    }))

  print(single, digits=digits)
  if (!is.null(baseline))
    cat("\nBaseline: ", min(baseline), "\u2014", max(baseline), fill=TRUE, sep="")

  attr(single, "baseline") <- baseline
  single
}

## usage:
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global", "BEST Global (Water Ice Temp.)", "JMA Global", "RSS TLT 3.3 -70.0/82.5", "UAH TLT 6.0 Global")
# hottest <- show_single_value(series)


## Crudely based on Cowtan et al. 2015, dx.doi.org/10.1002/2015GL064888.
#' @export
create_cmip5_tas_tos_data <- function(baseline=defaultBaseline, save_to_package=FALSE)
{
  data_dir <- system.file("extdata", package="climeseries")
  ensemble <- "cmip5"
  subdir <- "tas + tos"
  path <- paste(data_dir, ensemble, subdir, sep="/")

  ff <- list.files(path, "^.*?\\.dat$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
  modelSummary <- sapply(ff,
    function(a) {
      modelVariable <- str_match(a, "\\S(tas|tas land|tos)\\S")[2L]
      pathway <- str_match(a, "rcp(\\d{2})")[2L]
      modelType <- str_match(a, "\\S(all models|all members|one member per model)\\S")[2L]
      l <- readLines(a)
      re <- "^#.*?from\\s+(.*?),?\\s+(RCP|experiment|model).*$"
      modelLine <- grep(re, l, value=TRUE)
      model <- str_match(modelLine, re)[2L]

      r <- data.frame(model=model, type=tolower(modelType), variable=tolower(modelVariable), RCP=as.numeric(pathway)/10, path=a,
        check.rows=FALSE, check.names=FALSE, fix.empty.names=FALSE, stringsAsFactors=FALSE)

      r
    }, simplify = FALSE
  )
  modelSummary <- Reduce(rbind, modelSummary)
  modelSummary <- subset(modelSummary, modelSummary$variable %in% c("tas land", "tos")) # Not necessary here, but for genericness.
  #xtabs(~ model + variable + type + RCP, modelSummary)

  l <- dlply(modelSummary, ~ model + type + RCP,
    function(a)
    {
      if (is.null(a) || length(unique(a$variable)) < 2)
        return (NULL)

      ## Earth is approx. 72% water, 28% land.
      weightMap <- c(`tas land`=0.28, tos=0.72)
      w <- weightMap[a$variable]
      w <- w / table(a$variable)[names(w)]

      ## Now read in the files to be averaged together. Returns a list of the separate time series.
      weightedValues <- mapply(a$model, w, a$path, seq(nrow(a)),
        FUN = function (model, weight, path, n)
        {
          tab <- read.table(path)
          flit <- melt(tab[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
          for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
          flit <- arrange(flit, V1, month)
          x <- data.frame(year=flit$V1, met_year=NA, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

          modelDesignation <- "m" %_% sprintf("%04d", n)
          x[[modelDesignation]] <- x$temp

          if (!is.null(baseline)) {
            flit <- subset(x, x$year %in% baseline)
            bma <- tapply(flit$temp, flit$month, mean, na.rm=TRUE)
            x$base <- NA
            l_ply(names(bma), function(s) { v <- bma[s]; if (is.nan(v)) v <- 0.0; x$base[x$month == s] <<- v })

            ## Center anomalies on average baseline-period temperatures.
            x[[modelDesignation]] <- round(x$temp - x$base, 3L)
          }
          x <- x[, c(common_columns, modelDesignation)]
          ## Weight the series.
          x[[modelDesignation]] <- x[[modelDesignation]] * weight
          attr(x, "weight") <- weight
          attr(x, "model") <- model

          x
        }, SIMPLIFY = FALSE
      )

      m <- Reduce(merge_fun_factory(all=TRUE, by=common_columns), weightedValues)
      modelDesignation <- paste0(unique(a$model), "_", unique(a$RCP))
      m[[modelDesignation]] <- rowSums(m[, colnames(m) %nin% common_columns])

      m[, c(names(m)[names(m) %in% common_columns], modelDesignation)]
    }
  )

  keepElements <- !sapply(l, is.null)
  modelDetails <- subset(attr(l,"split_labels"), keepElements)
  m <- l[keepElements]

  m <- Reduce(merge_fun_factory(all=TRUE, by=common_columns), m)
  m <- recenter_anomalies(m, baseline=baseline) # Is this necessary?

  ## Make similar to previously made model objects.
  colNames <- colnames(m)
  colnames(m)[colNames %nin% common_columns] <- "m" %_% sprintf("%04d", seq(sum(colNames %nin% common_columns)))

  attr(m, "ensemble") <- "CMIP5"

  attr(m, "model_type") <- subdir

  attr(m, "model") <- modelDetails$model

  scenario <- "RCP " %_% sprintf(modelDetails$RCP, fmt="%.1f")
  names(scenario) <- colnames(m)[colNames %nin% common_columns]
  attr(m, "scenario") <- factor(scenario)

  cmip5 <- m
  if (save_to_package) {
    save(cmip5, file=paste(path, "cmip5.RData", sep="/"))
    save(cmip5, file=paste(path, "cmip5_raw.RData", sep="/")) # Not really "raw," but oh well.
  }
  ## To create the package data set:
  # m <- create_cmip5_tas_tos_data(save_to_package=TRUE)

  cmip5
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=TRUE)
# cmip5 <- get_models_data(ensemble="cmip5", subdir="tas + tos")
# series <- c("HadCRUT4 Global")
## Like Fig. 4(b) of Cowtan et al. 2015:
# plot_models_and_climate_data(inst, cmip5, series=series, scenario="RCP 8.5", start=1880, end=2020, ma=12, ma_i=12, baseline=1961:1990, center_fun="mean", smooth_envelope=FALSE, envelope_type="range", envelope_text="range", ylim=c(-1.0, 1.5), conf_int_i=FALSE, col_i_fun="topo.colors", col_i_fun...=list())


#' @export
create_loess_variables <- function(inst, series, loess...=list(), unwrap=TRUE, keep_interpolated=FALSE, ...)
{
  baselineAttribute <- attr(inst, "baseline")

  d <- inst[, c(common_columns, series)]
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series]))

  for (i in series) {
    d[[i %_% " (interpolated)"]] <- drop(interpNA(d[, i], "fmm"))

    loessArgs = list(
      formula = eval(substitute(s ~ yr_part, list(s=as.name(i %_% " (interpolated)")))),
      data = d,
      span = 0.2
    )
    loessArgs <- modifyList(loessArgs, loess...)

    l <- do.call("loess", loessArgs) # Removes NAs, so attend to it.
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
add_loess_variables <- function(inst, series, ...)
{
  d <- create_loess_variables(inst, series, ...)
  baselineAttribute <- attr(inst, "baseline")
  r <- base::merge(inst, d[, setdiff(names(d), series)], by=common_columns, all.x=TRUE)

  attr(r, "baseline") <- baselineAttribute
  r
}

## usage:
# series <- c("GISTEMP Zonal 64N-90N", "GISTEMP Zonal 44N-64N", "GISTEMP Zonal 24N-44N", "GISTEMP Zonal EQU-24N", "GISTEMP Zonal 24S-EQU", "GISTEMP Zonal 44S-24S", "GISTEMP Zonal 64S-44S", "GISTEMP Zonal 90S-64S")
# d <- get_climate_data(download=FALSE, baseline=TRUE)
# g <- add_loess_variables(d, series, loess...=list(span=0.4))
# plot_climate_data(g, series %_% " (LOESS fit)")


## Fit segmented linear models to selected climate data.
#' @export
fit_segmented_model <- function(x, series, col=suppressWarnings(brewer.pal(length(series),"Paired")), start=NULL, end=NULL, yearly=TRUE, breakpoints...=list(), segmented...=list(), seg.control...=list(seed=100), ...)
{
  r <- list(data=x, series=series)
  r$range <- list(start=start, end=end)
  r$col <- col
  length(r$col) <- length(r$series); names(r$col) <- r$series

  if (!yearly) {
    g <- r$data
  }
  else {
    g <- as.data.frame(make_yearly_data(r$data))
    if (!is.null(start)) start <- trunc(start)
    if (!is.null(end)) end <- trunc(end)
  }

  yearVar <- ifelse(yearly, "year", "yr_part")

  r$piecewise <- list()
  for (i in r$series) {
    r$piecewise[[i]] <- list()
    r$piecewise[[i]]$col <- r$piecewise$col[i]

    h <- oss(g, i)[na_unwrap(g[[i]]), , drop=FALSE]
    h <- h[h[[yearVar]] >= ifelse(!is.null(start), start, -Inf) & h[[yearVar]] <= ifelse(!is.null(end), end, Inf), ]

    breakpointsArgs <- list(
      formula = eval(substitute(Y ~ X, list(X=as.name(yearVar), Y=as.name(i)))),
      data = h,
      breaks = NULL
    )
    breakpointsArgs <- modifyList(breakpointsArgs, breakpoints...)
    r$piecewise[[i]]$bp <- do.call("breakpoints", breakpointsArgs)

    r$piecewise[[i]]$breaks <- r$piecewise[[i]]$bp$X[, yearVar][r$piecewise[[i]]$bp$breakpoint]

    seg.controlArgs <- list(
      stop.if.error = TRUE,
      K = length(r$piecewise[[i]]$breaks),
      n.boot = 250,
      random = FALSE,
      h = 0.3
    )
    seg.controlArgs <- modifyList(seg.controlArgs, seg.control...)
    segControl <- do.call("seg.control", seg.controlArgs)

    r$piecewise[[i]]$lm <- lm(breakpointsArgs$formula, data=h, x=TRUE, y=TRUE)

    segmentedArgs <- list(
      obj = r$piecewise[[i]]$lm,
      seg.Z = as.formula(paste("~", yearVar)),
      psi = r$piecewise[[i]]$breaks,
      control = segControl
    )
    segmentedArgs <- modifyList(segmentedArgs, segmented...)
    #r$piecewise[[i]]$sm <- do.call("segmented", segmentedArgs)

    run_segmented <- function()
    {
      mf <- model.frame(r$piecewise[[i]]$lm)

      while (TRUE) {
        withRestarts({
          sm <- do.call("segmented", segmentedArgs)
          break
        },
          restart = function() {
            ## Which breakpoint is closest to the start or end of the time series?
            if (length(segmentedArgs$psi) > 1L)
              segmentedArgs$psi <<- segmentedArgs$psi[-which.min(pmin(segmentedArgs$psi, NROW(mf) - segmentedArgs$psi + 1))]
          })
      }

      sm
    }

    tryCatch({
      withCallingHandlers({
          sm <- run_segmented()
        },
          error = function(e) {
            message("Error: ", e$message)
            if (any(grepl("one coef is NA: breakpoint(s) at the boundary", e$message, fixed=TRUE)))
              invokeRestart("restart")
          }
      )

      r$piecewise[[i]]$sm <- sm
    }, error = function(e) { message("Warning: No breakpoint(s) found") })
  }

  r
}


#' @export
nearest_year_month_from_numeric <- function(yr_part, x, nearest_type = c("nearest", "above", "below"), as_data_frame = FALSE)
{
  nearest_type <- match.arg(nearest_type)

  if (missing(yr_part)) {
    flit <- rev(expand.grid(month = 1:12, year = trunc(x), by = 1))
    flit$yr_part <- flit$year + (2 * flit$month - 1)/24
  }
  else {
    r <- range(yr_part)
    x <- x[1]
    flit <- rev(expand.grid(month = 1:12, year = seq(floor(r[1]), floor(r[2]), by = 1)))
    flit$yr_part <- flit$year + (2 * flit$month - 1)/24
    flit <- flit[flit$yr_part >= r[1] & flit$yr_part <= r[2], ]
  }

  egrid <- switch(nearest_type,
    `above` = flit[flit$yr_part >= x, ],

    `below` = flit[flit$yr_part <= x, ],

    flit
  )

  r <- egrid[nearest(egrid$yr_part, x), c("year", "month")]
  if (!as_data_frame)
    r <- unlist(r)

  r
}


#' @export
create_hadcrut4_zonal_data <- function(x,
  sub_lat = c(-90, 90), sub_long = c(-180, 180),
  kriged = TRUE,
  data_dir = getOption("climeseries_data_dir"),
  hadcrut_url = "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.4.6.0.0.median.nc",
  cw_url = "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/had4_krig_v2_0_0.nc.gz",
  series_suffix = NULL
)
{
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)

  if (is.null(data_dir)) data_dir <- getwd()

  if (!kriged) {
    series <- "HadCRUT4"
    flit <- basename(hadcrut_url)
    download.file(hadcrut_url, paste(data_dir, flit, sep = "/"), mode = "wb", quiet = TRUE)
  } else {
    series <- "Cowtan & Way Krig."
    flit <- basename(cw_url)
    download.file(cw_url, paste(data_dir, flit, sep = "/"), mode = "wb", quiet = TRUE)
    R.utils::gunzip(paste(data_dir, flit, sep = "/"), overwrite = TRUE, remove = FALSE)
    flit <- basename(tools::file_path_sans_ext(cw_url))
  }
  n <- nc_open(paste(data_dir, flit, sep = "/")) # 'print(n)' or just 'n' for details.
  a <- ncvar_get(n, "temperature_anomaly")
  ## Structure of 'a' is temperature_anomaly[longitude, latitude, time], 72 × 36 × Inf (monthly since Jan. 1850)
  lat <- ncvar_get(n, "latitude")
  long <- ncvar_get(n, "longitude")
  times <- ncvar_get(n, "time")
  tunits <- ncatt_get(n,"time", "units")
  nc_close(n)

  tunits
  # $value
  # [1] "days since 1850-1-1 00:00:00"
  dtimes <- as.Date(times, origin = "1850-01-01")
  ## This data set should have the same length as the "time" dimension of 'a':
  h <- dataframe(year = year(dtimes), month = month(dtimes))
  #h <- dataframe(year = year(dtimes), met_year = NA, month = month(dtimes))
  #h$yr_part <- h$year + (h$month - 0.5) / 12L

  flit <- apply(a, 3,
    function(y)
    {
      x <- t(y)
      w <- cos(matrix(rep(lat, ncol(x)), ncol = ncol(x), byrow = FALSE) * (pi / 180)) # Latitude weights.

      ## Use only subgrid for calculations.
      keepSubGrid <- list(lat = lat >= sub_lat[1] & lat <= sub_lat[2], long = long >= sub_long[1] & long <= sub_long[2])
      x1 <- x[keepSubGrid$lat, keepSubGrid$long, drop = FALSE]
      w1 <- w[keepSubGrid$lat, keepSubGrid$long, drop = FALSE]

      nlat <- length(lat[keepSubGrid$lat])
      temp <- NULL
      for (i in seq(1L, nrow(x1), by = nlat)) {
        xi <- data.matrix(x1[i:(i + nlat - 1L), ])
        tempi <- stats::weighted.mean(xi, w1, na.rm = TRUE)

        temp <- c(temp, tempi)
      }

      temp
    })
  is.na(flit) <- is.nan(flit)

  lat_long_to_text <- function(x, sufs) { suf <- sufs[2]; r <- abs(x); if (x < 0) suf <- sufs[1]; r %_% suf }
  subLatText <- sapply(sub_lat, lat_long_to_text, sufs = c("S", "N"), simplify = TRUE)
  subLongText <- sapply(sub_long, lat_long_to_text, sufs = c("W", "E"), simplify = TRUE)

  if (is.null(series_suffix))
    seriesOut <- paste0(series, " (", paste(subLatText, collapse = "-"), ", ", paste(subLongText, collapse = "-"), ")")
  else
    seriesOut <- paste0(series, series_suffix)

  h[[seriesOut]] <- flit
  x <- merge(x, h, by = c("year", "month"), all = TRUE)

  x
}

## usage:
# g <- create_hadcrut4_zonal_data()
# series <- c("Cowtan & Way Krig. Global", "Cowtan & Way Krig. (90S-90N, 180W-180E)")
# plot_climate_data(g, series, yearly = TRUE) # These should overlap entirely.
