#' Plot Climatological Time Series
#'
#' Readily displays annotated climate time series on a common plot with many configurable options.
#'
#' @param x A data set (down)loaded by function \code{\link{get_climate_data}}.
#' @param series A vector containing the column names of the climate series to be plotted. If \code{NULL}, all the series in \code{x} are plotted.
#' @param start,end Integer values of the starting and ending years of the plot, respectively; if either is given as \code{NULL}, the minimum or maximum date in \code{x} is used instead.
#' @param ma The size of the \code{ma}-month moving average for smoothing the climate series.
#' @param baseline An integer year or, more typically, range of years on which the climate-series anomalies will be centered. If \code{NULL}, no baseline centering is done, and the "baseline" attribute of \code{x} (if it exists) is used instead.
#' @param plot_type Passed to \code{\link[stats]{plot.ts}}.
#' @param type What type of plot should be drawn; see \code{?plot} for details.
#' @param col A vector of colors, each for plotting a member of \code{series}, which is passed to \code{\link[stats]{plot.ts}} and \code{\link[graphics]{legend}}.
#' @param col_fun If \code{col = NULL}, a color palette for \code{series} is generated from the function value of \code{col_fun}.
#' @param col_fun... Takes a list of arguments to be passed to function \code{col_fun()}, possibly overriding default values.
#' @param alpha Sets the color transparency of the climate series to a new level in [0, 1]. If \code{alpha} is \code{NA}, existing alpha values are preserved.
#' @param lwd A vector of line widths, each for plotting a member of \code{series}, which is passed to \code{\link[stats]{plot.ts}}.
#' @param omit_series A vector containing the column names of the climate series NOT to be plotted if \code{series = NULL}. This has been added to leave out the Keeling Curve data, but it could later include other regularly updated time-series data, related to climate, which wouldn't normally be plotted in the same window.
#' @param conf_int Logical; if \code{TRUE}, plot 95\% confidence intervals for those series with CI data.
#' @param ci_alpha Sets the color transparency of the temperature-series CIs to a new level in [0, 1]. If \code{ci_alpha} is \code{NA}, existing alpha values are preserved.
#' @param ... Passed to \code{\link[stats]{plot.ts}}.
#'
#' @examples
#' \dontrun{
#' inst <- get_climate_data(download=FALSE, baseline=TRUE)
#'
#' series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global", "BEST Global (Water Ice Temp.)", "JMA Global", "RSS TLT 3.3 70.0/82.5", "UAH TLT 6.0 Global", "RATPAC-A 850-300 mb Global")
#' plot_climate_data(inst, series=series, 1880, ma=12, lwd=2)
#'
#' ########################################
#' ## Plot instrumental series with 95% confidence intervals.
#' ########################################
#'
#' series <- c("Cowtan & Way Krig. Global", "HadCRUT4 Global")
#' plot_climate_data(inst, series=series, 1880, ma=12, lwd=2, conf_int=TRUE)
#'
#' ########################################
#' ## Plot instrumental series and trend line. (N.B. I should build this into 'plot_climate_data()'.)
#' ########################################
#'
#' ## Prepare trend data.
#' baseline <- TRUE
#' d <- get_climate_data(download=FALSE, baseline=baseline)
#' #d[d$month != 1, get_climate_series_names(d)] <- NA # Uncomment to pick out a single month or range of months.
#' m <- list()
#' m$series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "RSS TLT 3.3 70.0/82.5", "UAH TLT 6.0 Global")
#' m$range <- list(start=1979, end=NULL) # Use 'start=2011' to cherry-pick a really rapid increase!
#' m$col <- suppressWarnings(brewer.pal(length(m$series),"Paired"))
#' #m$col <- topo.colors(length(m$series))
#' length(m$col) <- length(m$series); names(m$col) <- m$series
#' m$data <- d[d$year >= ifelse(!is.null(m$range$start), m$range$start, -Inf) & d$year <= ifelse(!is.null(m$range$end), m$range$end, Inf), ]
#' for (s in m$series) {
#'   m[[s]]$lm <- lm(eval(substitute(b ~ yr_part, list(b=as.symbol(s)))), data=m$data)
#'   summary(m[[s]]$lm)
#'   m[[s]]$warming <- coef(m[[s]]$lm)[2] * diff(range(m[[s]]$lm$model[, 2]))
#'   m[[s]]$rate <- coef(m[[s]]$lm)[2] * 10
#'   m[[s]]$rateText <- eval(substitute(expression(paste(Delta, "T = ", r, phantom(l) * degree, "C/dec.", sep="")), list(r=sprintf(m[[s]]$rate, fmt="%+1.3f"))))
#'   m[[s]]$col <- m$col[s]
#' }
#' ## Plot series and trends.
#' plot_climate_data(d, m$series, m$range$start, m$range$end, ma=12, lwd=2, col=m$col, baseline=baseline)
#' legendText <- NULL
#' for (s in m$series) {
#'   abline(coef(m[[s]]$lm)[1L], coef(m[[s]]$lm)[2L], col=m$col[s], lwd=2)
#'   legendText <- c(legendText, m[[s]]$rateText)
#' }
#' legend("bottomright", inset=c(0.2, 0.2), legend=legendText, col=m$col, lwd=2, bty="n", cex=0.8)
#'
#' ########################################
#' ## Plot piecewise linear trends based on changepoint analysis (similar to Cahill et al. 2015, dx.doi.org/10.1088/1748-9326/10/8/084002).
#' ########################################
#'
#' ## Prepare trend data.
#' baseline <- TRUE
#' d <- get_climate_data(download=FALSE, baseline=baseline)
#' m <- list()
#' m$series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global")
#' m$range <- list(start=1880, end=NULL)
#' m$col <- suppressWarnings(brewer.pal(length(m$series),"Paired"))
#' length(m$col) <- length(m$series); names(m$col) <- m$series
#' m$data <- d[d$year >= ifelse(!is.null(m$range$start), m$range$start, -Inf) & d$year <= ifelse(!is.null(m$range$end), m$range$end, Inf), ]
#' for (s in m$series) {
#'   m[[s]]$lm <- lm(eval(substitute(b ~ yr_part, list(b=as.symbol(s)))), data=m$data)
#'   summary(m[[s]]$lm)
#'   m[[s]]$warming <- coef(m[[s]]$lm)[2L] * diff(range(m[[s]]$lm$model[, 2L]))
#'   m[[s]]$rate <- coef(m[[s]]$lm)[2L] * 10
#'   m[[s]]$rateText <- eval(substitute(expression(paste(Delta, "T = ", r, phantom(l) * degree, "C/dec.", sep="")), list(r=sprintf(m[[s]]$rate, fmt="%+1.3f"))))
#'   m[[s]]$col <- m$col[s]
#' }
#' ## Plot series.
#' plot_climate_data(d, m$series, m$range$start, m$range$end, ma=12, lwd=2, col=m$col, baseline=baseline)
#' ## Create changepoint model and plot results.
#' library(segmented)
#' changepoints <- 3L
#' for (s in m$series) {
#'   sm <- segmented::segmented(m[[s]]$lm, ~ yr_part, NA, seg.control(stop.if.error=TRUE, K=changepoints))
#'   print(sm)
#'   plot(sm, add=TRUE, lwd=2, col=m[[s]]$col, rug=FALSE)
#' }
#' }
#'
#' @export
plot_climate_data <- function(x, series=NULL, start=1880, end=NULL, ma=NULL, baseline=NULL, plot_type=c("single", "multiple"), type="l", xlab="Year", ylab=NULL, main=NULL, col=NULL, col_fun=RColorBrewer::brewer.pal, col_fun...=list(name="Paired"), alpha=0.5, lwd=2, omit_series=climeseries:::omit_series, conf_int=FALSE, ci_alpha=0.3, show_trend=FALSE, trend_legend_inset=c(0.2, 0.2), ...)
{
  plot_type <- match.arg(plot_type)

  baselineSaved <- attr(x, "baseline")

  if (!is.null(series))
    x <- x[, c(names(x)[grepl("^yr_|^met_|^year|^month|_uncertainty$", names(x))], series)]
  else {
    if (!is.null(omit_series)) {
      series <- setdiff(get_climate_series_names(x), omit_series)
      x <- x[, c(names(x)[grepl("^yr_|^met_|^year|^month|_uncertainty$", names(x))], series)]
    }
  }
  x <- subset(x, na_unwrap(x[, !grepl("^yr_|^met_|^year|^month|_uncertainty$", names(x), x)])) # Remove trailing NAs.
  attr(x, "baseline") <- baselineSaved

  if (!is.null(baseline))
    x <- recenter_anomalies(x, baseline, conf_int=FALSE)

  s <- make_time_series_from_anomalies(x, conf_int=TRUE)
  s_yr_part <- ts(x[, "yr_part"], unlist(x[1L, c("year", "month")]), frequency=12)
  w <- window(s[, get_climate_series_names(s)], start, end, extend=TRUE)
  w_yr_part <- window(s_yr_part, start, end, extend=TRUE)

  s_raw <- s
  ## We must interpolate missing values in the time series.
  #s <- timeSeries::interpNA(s_raw, "linear")
  s <- interpNA(s_raw, "linear")
  #s <- na.approx(s_raw)
  sma <- MA(s[, get_climate_series_names(s)], ma)
  maText <- ""
  if (!is.null(ma))
    maText <- "(" %_% ma %_% "-month moving average)"

  w_ma <- window(sma, start, end, extend=TRUE)

  baselineText <- ""
  baseline <- attr(x, "baseline")
  if (!is.null(baseline))
    baselineText <- " w.r.t. " %_% min(baseline) %_% "\u2013" %_% max(baseline)

  xlab <- eval(xlab)
  if (is.null(ylab))
    ylab <- eval(substitute(expression(paste("Temperature Anomaly (", phantom(l) * degree, "C)", b, sep="")), list(b=baselineText)))
  else
    ylab <- eval(ylab)
  if (is.null(main))
    main <- "Average Temperature"
  startTS <- start(w_ma); endTS <- end(w_ma)
  if (is.null(end)) endTS <- c(year(Sys.Date()), month(Sys.Date()) - 1)
  main <- paste(main, " (", MOS[startTS[2L]], ". ", startTS[1L], "\u2013", MOS[endTS[2L]], ". ", endTS[1L], ")", sep="")

  if (is.null(col)) {
    col <- seq_along(series)
    col_funArgs <- list(
      n = length(col)
    )
    col_funArgs <- modifyList(col_funArgs, col_fun...)
    col <- suppressWarnings(do.call(col_fun, col_funArgs))
    ## Some other possible function calls:
    #col <- rainbow(length(col))
    #col <- terrain.colors(length(col))
    #col <- topo.colors(length(col))
    #col <- suppressWarnings(brewer.pal(length(col),"Spectral")) # Or "Paired".
    #col <- matlab.like2(length(col)) # From package "colorRamps".
  }
  col <- rep(col, length.out=length(series))
  col <- alpha(col, alpha)
  names(col) <- series

  GetXAxisTicks <- function(min=1800, max=3000, by=10)
  {
    yearGroups <- seq(min, max, by=by)
    plotStart <- start
    if (is.null(plotStart)) plotStart <- min(x$year)
    plotEnd <- end
    if (is.null(plotEnd)) plotEnd <- max(x$year)

    plotStart <- nearest_below(yearGroups, plotStart, TRUE); plotEnd <- nearest_above(yearGroups, plotEnd, TRUE)
    xaxisTicks <- seq(plotStart, plotEnd, by=by)

    return (xaxisTicks)
  }

  xaxisTicks <- GetXAxisTicks()
  if (length(xaxisTicks) < 8L)
    xaxisTicks <- GetXAxisTicks(by=5)

  ## TODO: Some of the following could be done with the default 'plot()' arguments 'panel.first' and 'panel.last'.
  xaxt <- "n"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.
  plot(w_ma, plot.type=plot_type, type="n", xaxs="r", xaxt=xaxt, xlab=xlab, ylab=ylab, main=main, frame.plot=FALSE, ...) # I.e. 'plot.ts()'.
  if (xaxt == "n")
    axis(1, xaxisTicks)
  else
    xaxisTicks <- axTicks(1L)
  if (maText != "") mtext(maText, 3L)

  grid(nx=NA, ny=NULL, col="lightgray", lty="dotted", lwd=par("lwd"))
  abline(v=xaxisTicks, col="lightgray", lty="dotted", lwd=par("lwd"))

  cis <- s[, grepl("_uncertainty$", colnames(s))]
  if (conf_int) { # Plot confidence bands for temp series that have them.
    confintNames <- intersect(series %_% "_uncertainty", colnames(cis))
    if (length(confintNames) != 0L) {
      seriesNames <- str_match(confintNames, "^(.*?)_uncertainty$")[, 2L]
      for (i in seq_along(confintNames)) {
        value <- s[, seriesNames[i]]
        ci <- cis[, confintNames[i]]
        upper <- window(MA(value + ci/2, ma), start, end, extend=TRUE); lower <- window(MA(value - ci/2, ma), start, end, extend=TRUE)
        ciCol <- alpha(col[seriesNames[i]], ci_alpha)
        cidf <- data.frame(yr_part=w_yr_part, lower=lower, upper=upper); cidf <- cidf[complete.cases(cidf), ]
        polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
      }
    }
  }

  par(new=TRUE)
  plot(w_ma, plot.type=plot_type, type=type, col=col, lwd=lwd, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ...) # I.e. 'plot.ts()'.

  legend(x="topleft", legend=series, col=col, lwd=lwd, bty="n", cex=0.8)

  ## Decadal linear trends. Simple, but can be extended later for more control.
  if (show_trend) {
    m <- list()
    m$series <- series
    m$range <- list(start=start, end=end)
    m$col <- col
    m$data <- x[x$year >= ifelse(!is.null(m$range$start), m$range$start, -Inf) & x$year <= ifelse(!is.null(m$range$end), m$range$end, Inf), ]
    for (s in m$series) {
      m[[s]]$lm <- lm(eval(substitute(b ~ yr_part, list(b=as.symbol(s)))), data=m$data)
      m[[s]]$warming <- coef(m[[s]]$lm)[2] * diff(range(m[[s]]$lm$model[, 2]))
      m[[s]]$rate <- coef(m[[s]]$lm)[2] * 10
      m[[s]]$rateText <- eval(substitute(expression(paste(Delta, "T = ", r, phantom(l) * degree, "C/dec.", sep="")), list(r=sprintf(m[[s]]$rate, fmt="%+1.3f"))))
      m[[s]]$col <- m$col[s]
    }

    legendText <- NULL
    for (s in m$series) {
      abline(coef(m[[s]]$lm)[1L], coef(m[[s]]$lm)[2L], col=m[[s]]$col, lwd=2)
      legendText <- c(legendText, m[[s]]$rateText)
    }

    legend("bottomright", inset=trend_legend_inset, legend=legendText, col=m$col, lwd=2, bty="n", cex=0.8)
  }

  return (nop())
}

omit_series <- c("CO2 Mauna Loa", "NCEI US Palmer Z-Index", "NCEI US PDSI", "NCEI US PHDI", "NCEI US PMDI", "NCEI US Precip." )


##  Plot sequential global annual mean surface-temp. trend with confidence intervals (via Chris Colose).
#' @export
plot_sequential_trend <- function(series, start=NULL, end=NULL, use_polygon=FALSE, mark_years=NULL, baseline=FALSE, plot...=list(), ci...=list(), abline...=list(), m_text=NULL, use_current_year=FALSE) # Style suggested by https://tamino.wordpress.com/2014/12/04/a-pause-or-not-a-pause-that-is-the-question/
{
  d <- get_climate_data(download=FALSE, baseline=baseline)
  if (!is.null(start)) d <- d[d$year >= start, ]
  if (!use_current_year) d <- d[d$year < current_year, ] # If 'use_current_year=FALSE', the current, possibly incomplete year is excluded.
  series <- series[1L]
  means <- unclass(by(d[[series]], d$year, mean, na.rm=TRUE))
  yearNames <- names(means)
  means <- as.vector(means); names(means) <- yearNames
  means <- means[na_unwrap(means)]
  rates <- rep(NA, length(means))
  rates <- data.frame(rate=rates, lwr=rates, upr=rates)
  rownames(rates) <- names(means)
  years <- as.numeric(rownames(rates))

  for (i in seq_along(means)) {
    y <- means[i:length(means)]
    x <- as.numeric(names(y))

    m <- lm(y ~ x)
    r <- coef(m)[2L] * 10 # Decadal rate.
    ci <- suppressWarnings(t(confint(m, "x"))) * 10
    rates[i, ] <- c(r, ci)
  }

  if (is.null(start)) start <- head(years, 1L)
  if (is.null(end)) end <- current_year - 5

  attr(rates, "series") <- series
  attr(rates, "trend_start") <- start
  attr(rates, "trend_end") <- tail(years, 1L)

  ccRates <- rates[years <= end & complete.cases(rates), ]
  ccYears <- as.numeric(rownames(ccRates))[years <= end]

  plotArgs <- list(
    x = ccYears,
    y = ccRates[, "rate"],
    type = "o",
    pch = 16,
    ylim = c(-1.0, 1.0),
    lwd = 2,
    col = "black",
    panel.first = quote(abline(h=0.0, col='darkgray', lty="dashed")),
    xlab = "Start year of trend",
    ylab = expression(paste("Trend (", phantom(l) * degree, "C/dec.)", sep="")),
    main = "Linear Temperature Trend (" %_% series %_% ") + 95% CIs"
  )
  plotArgs <- modifyList(plotArgs, plot...)

  if (is.language(plotArgs$main))
    plotArgs$main <- eval(plotArgs$main)

  if (is.null(m_text))
    m_text <- quote("Global annual mean surface-temp. trend from start year to " %_% tail(names(means), 1))

  if (dev.cur() == 1) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.
  do.call("plot.default", plotArgs)
  mtext(eval(m_text))

  if (use_polygon) {
    ciArgs <- list(
      x = c(ccYears, rev(ccYears)),
      y = c(ccRates[, "lwr"], rev(ccRates[, "upr"])),
      col = alpha("gray", 0.6),
      border = NA # Border color; NULL means use par("fg"), NA omits borders.
    )
    ciArgs <- modifyList(ciArgs, ci...)

    do.call("polygon", ciArgs)
  }
  else { # Use error bars to show confidence intervals.
    ciArgs <- list(
      x0 = ccYears,
      y0 = ccRates[, "lwr"],
      x1 =ccYears,
      y1 = ccRates[, "upr"],
      length = 0.03,
      angle = 90,
      code = 3
    )
    #ciArgs <- merge.list(ci..., ciArgs)
    ciArgs <- modifyList(ciArgs, ci...)

    do.call("arrows", ciArgs)
  }

  if (!is.null(mark_years)) {
    ablineArgs <- list(
      v = mark_years,
      col = alpha("red", 0.4)
    )
    ablineArgs <- modifyList(ablineArgs, abline...)

    do.call("abline", ablineArgs)
    text(mark_years, par("yaxp")[2L], mark_years, cex=0.8, srt=270, adj=c(NA, -0.25)) # 'par("usr")' for the whole plotting region.
  }

  return (rates)
}
## usage:
# rates <- plot_sequential_trend("RSS TLT 3.3 70.0/82.5")
# rates <- plot_sequential_trend("GISTEMP Global", use_polygon=TRUE)
# rates <- plot_sequential_trend("GISTEMP Global", use_polygon=FALSE, mark_years=c(1998))
# rates <- plot_sequential_trend("GISTEMP Global", use_polygon=FALSE, mark_years=c(1998), use_current_year=TRUE)


#' Plot Climatological Time Series with Model Results
#'
#' Readily displays annotated climate time series (usually temperature) on a common plot along with CMIP Phase 3 or Phase 5 model projections.
#'
#' @param instrumental An instrumental-temperature dataset (down)loaded by function \code{\link{get_climate_data}}.
#' @param models A modeled-temperature dataset loaded by function \code{\link{get_models_data}}.
#' @param series A vector containing the column names of the instrumental temperature series to be plotted. If \code{NULL}, all the series in \code{instrumental} are plotted; if \code{NA}, plotting of the instrumental temperature series is suppressed.
#' @param scenario A character vector corresponding to specific radiative-forcing pathways represented in subsets of the modeled temperature data. If \code{NULL}, all pathways are included. The options for CMIP3+ data are any subset of \code{c("20C3M", "SRES B1", "SRES A1B", "SRES A2")}; options for CMIP5 include any subset of \code{c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")}.
#' @param start,end Integer values of the starting and ending years of the plot, respectively; if either is given as \code{NULL}, the minimum or maximum date in \code{models} is used instead.
#' @param ma,ma_i The sizes in months of the moving averages for smoothing the modeled and instrumental temperature series, respectively. If \code{ma_i} is not given, its value is the same as \code{ma}. A value of \code{NULL} means that no moving-average smoothing is done.
#' @param baseline An integer year or, more typically, range of years on which the temperature anomalies will be centered. If \code{NULL}, no baseline centering is done, and the "baseline" attribute of \code{models} (if it exists) is used instead.
#' @param ylim The y limits of the plot, i.e. the plot range of the temperature anomalies.
#' @param center_fun The function used to calculate the central tendency of the model runs at each time point; the default of \code{"mean"} is usually sufficient.
#' @param smooth_center Logical; if \code{TRUE}, LOESS-smooth the plot of the models' central tendency.
#' @param envelope_coverage Sets the "coverage" of the models-envelope calculated as \code{envelope_type}; e.g. for \code{envelope_coverage = 0.95} and \code{envelope_type = "range"}, the envelope covers 95\% of the difference between the maximum model value and the minimum, for each time point.
#' @param envelope_type A character vector matching one of several methods (e.g. "quantiles", "range", "normal") for calculating the \code{envelope_coverage} envelope characterizing the spread of the models. The default value is \code{"quantiles"}.
#' @param plot_envelope Logical; if \code{TRUE}, plot the envelope of 95\% model coverage.
#' @param smooth_envelope Logical; if \code{TRUE}, LOESS-smooth the plot of the envelope of 95\% model coverage.
#' @param col_m A vector of colors for each model scenario. If \code{NULL} (the default), a series of grays is used; if a single value, \code{\link{vary_brightness}} is called with that value to get a range of colors based on it.
#' @param col_m_mean The color of the central-tendency line for all the included model runs. The default of \code{NULL} causes a darkish gray to be used; ; if a single value, \code{\link{vary_brightness}} is called with that value to get a range of colors based on it.
#' @param alpha_envelope Sets the color transparency of the model envelopes to a new level in [0, 1]. If \code{alpha_envelope} is \code{NA}, existing alpha values are preserved. If \code{alpha_envelope} is \code{NULL}, the model envelopes are not color-filled.
#' @param legend... Takes a list of arguments to be passed to function \code{\link[graphics]{legend}}, possibly overriding default values.
#' @param plot_i... Takes a list of arguments to be passed to function \code{\link[zoo]{plot.zoo}}, possibly overriding default values, for the instrumental series.
#' @param col_i_fun A color palette for \code{series} is generated from the function value of \code{col_i_fun}. Note that the colors created for the instrumental series here can be entirely overridden by including a \code{col} argument in \code{plot_i...}; or, better, by specifying a function like \code{function(...) "red"} (which also affects the legend).
#' @param col_i_fun... Takes a list of arguments to be passed to function \code{col_i_fun}, possibly overriding default values. E.g. to get colors from function \code{\link[RColorBrewer]{brewer.pal}}, use \code{col_i_fun = RColorBrewer::brewer.pal} and \code{col_i_fun... = list(name="Paired")}.
#' @param alpha_i Sets the color transparency of the temperature series to a new level in [0, 1]. If \code{alpha} is \code{NA}, existing alpha values are preserved.
#' @param conf_int_i Logical; if \code{TRUE}, plot 95\% confidence intervals for those series with CI data.
#' @param ci_alpha_i Sets the color transparency of the temperature-series CIs to a new level in [0, 1]. If \code{ci_alpha_i} is \code{NA}, existing alpha values are preserved.
#' @param omit_series A vector containing the column names of the climate series NOT to be plotted if \code{series = NULL}. This has been added to leave out the Keeling Curve data, but it could later include other regularly updated time-series data, related to climate, which wouldn't normally be plotted in the same window.
#' @param ... Passed to \code{\link[zoo]{plot.zoo}} for the models.
#'
#' @examples
#' \dontrun{
#' inst <- get_climate_data(download=FALSE, baseline=TRUE)
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=FALSE) # Load CMIP5 data from combined R data set.
#'
#' ## Plot all CMIP5 scenario realizations, no instrumental temperature series.
#' plot_models_and_climate_data(inst, cmip5, series=NA, scenario=NULL, start=1950, end=2100, ma=12, baseline=NULL,
#'   center_fun="mean", smooth_envelope=TRUE, col_m_mean="red", ylim=c(-1, 5))
#'
#' ## CMIP5 RCP 4.5 scenario realizations compared to the GISTEMP land+SST series.
#' series <- c("GISTEMP Global")
#' plot_models_and_climate_data(inst, cmip5, series=series, scenario="RCP 4.5", start=1880, end=2020, ma=12, ma_i=12,
#'   baseline=1951:1980, center_fun="mean", smooth_envelope=TRUE, envelope_type="quantiles", envelope_text="quantiles",
#'   ylim=c(-1.0, 1.5), conf_int_i=FALSE, col_i_fun="topo.colors", col_i_fun...=list())
#'
#' ## Same plot, different baseline:
#' plot_models_and_climate_data(inst, cmip5, series=series, scenario="RCP 4.5", start=1880, end=2020, ma=12, ma_i=12,
#'   baseline=1981:2010, center_fun="mean", smooth_envelope=TRUE, envelope_type="quantiles", envelope_text="quantiles",
#'   ylim=c(-1.5, 1.0), conf_int_i=TRUE, col_i_fun=function(...) "red")
#' }
#' @export
plot_models_and_climate_data <- function(instrumental, models, series=NULL, scenario=NULL, start=1880, end=NULL, ma=NULL, ma_i=ma, baseline=NULL, ylim=c(-1.0, 1.0), center_fun="mean", smooth_center=FALSE, envelope_coverage=0.95, envelope_type=c("quantiles", "range", "normal"), plot_envelope=TRUE, smooth_envelope=TRUE, col_m=NULL, col_m_mean=NULL, alpha_envelope=0.2, envelope_text="model coverage", legend...=list(), plot_i...=list(), col_i_fun=RColorBrewer::brewer.pal, col_i_fun...=list(name="Paired"), alpha_i=0.5, conf_int_i=FALSE, ci_alpha_i=0.3, omit_series=c("Keeling"), ...)
{
  envelope_type <- match.arg(envelope_type)

  plotInstrumental <- TRUE
  if (length(series) == 1L && is.na(series))
    plotInstrumental <- FALSE

  instrumental <- recenter_anomalies(instrumental, baseline)
  models <- recenter_anomalies(models, baseline)

  ensemble <- attr(models, "ensemble")
  if (is.null(baseline))
    baseline <- attr(models, "baseline")

  originalScenario <- attr(models, "scenario")
  keepCols <- names(models)
  if (!is.null(scenario)) {
    keepCols <- c(commonColumns, setdiff(names(models), commonColumns)[originalScenario %in% scenario])
    models <- models[, keepCols]

    # Restore some attributes.
    attr(models, "ensemble") <- ensemble
    attr(models, "scenario") <- factor(originalScenario[originalScenario %in% scenario], levels=scenario[scenario %in% originalScenario])
    attr(models, "baseline") <- baseline
  }
  else
    scenario <- levels(attr(models, "scenario"))

  i <- make_time_series_from_anomalies(instrumental, conf_int=TRUE)
  m <- make_time_series_from_anomalies(models)

  i_yr_part <- ts(instrumental[, "yr_part"], unlist(instrumental[1, c("year", "month")]), frequency=12)
  i_yr_part <- window(i_yr_part, start, end, extend=TRUE)
  m_yr_part <- ts(models[, "yr_part"], unlist(models[1L, c("year", "month")]), frequency=12)
  m_yr_part <- window(m_yr_part, start, end, extend=TRUE)

  maText <- ma_iText <- ""
  if (!is.null(ma)) {
    m <- MA(m, ma)
    maText <- "(" %_% ma %_% "-month moving average)"
  }

  i_raw <- i
  ## We must interpolate missing values in the time series.
  #i <- timeSeries::interpNA(i_raw, "linear")
  i <- interpNA(i_raw, "linear")
  ima <- MA(i[, get_climate_series_names(i)], ma_i)
  if (!is.null(ma_i)) {
    if (ma_i != ma)
      ma_iText <- " (" %_% ma_i %_% "-mo. m.a.)"
  }
  else {
    if (!is.null(ma))
      ma_iText <- " (no m.a.)"
  }

  ima <- window(ima, start, end, extend=TRUE)
  m <- window(m, start, end, extend=TRUE)

  ## Convert to 'zoo' objects for plotting.
  ima <- as.zoo(ima); colnames(ima) <- get_climate_series_names(instrumental)
  m <- as.zoo(m)

  baselineText <- ""
  baselineAttribute <- attr(instrumental, "baseline")
  if (!is.null(baselineAttribute))
    baselineText <- " w.r.t. " %_% min(baseline) %_% "\u2013" %_% max(baseline)

  xlab <- "Year"
  ylab <- eval(substitute(expression(paste("Global Temperature Anomaly (", phantom(l) * degree, "C)", b, sep="")), list(b=baselineText)))
  main <- paste(ensemble," Scenario Realizations (", start, "\u2013", end, ")", sep="")

  GetXAxisTicks <- function(min=1800, max=3000, by=10)
  {
    yearGroups <- seq(min, max, by=by)
    plotStart <- start
    if (is.null(plotStart)) plotStart <- min(x$year)
    plotEnd <- end
    if (is.null(plotEnd)) plotEnd <- max(x$year)

    plotStart <- nearest_below(yearGroups, plotStart, TRUE); plotEnd <- nearest_above(yearGroups, plotEnd, TRUE)
    xaxisTicks <- seq(plotStart, plotEnd, by=by)

    return (xaxisTicks)
  }

  xaxisTicks <- GetXAxisTicks()
  if (length(xaxisTicks) < 8L)
    xaxisTicks <- GetXAxisTicks(by=5)

  ## TODO: Some of the following could be done with the default 'plot()' arguments 'panel.first' and 'panel.last'.
  xaxt <- "n"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.
  plot(ima, screens=1L, bty="n", xaxs="r", xaxt=xaxt, xlab=xlab, ylab=ylab, main=main, type="n", ylim=ylim, ...) # I.e. 'plot.zoo()'.
  if (xaxt == "n")
    axis(1, xaxisTicks)
  else
    xaxisTicks <- axTicks(1)
  if (maText != "") mtext(maText, 3L)

  ## Create different colors for each scenario.
  modelColors <- col_m
  if (is.null(modelColors))
    modelColors <- gray.colors(length(scenario), 0.90, 0.85)
  if (length(modelColors) == 1L && length(scenario) > 1L)
    modelColors <- vary_brightness(modelColors, length(scenario))
  if (length(modelColors) < length(scenario))
    modelColors <- rep(modelColors, length.out=length(scenario))
  eachModelColor <- as.vector(unlist(tapply(as.numeric(attr(models, "scenario")), attr(models, "scenario"), function (a) modelColors[a])))
  par(new=TRUE)
  plot(m, screens=1, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=ylim, col=eachModelColor, ...) # I.e. 'plot.zoo()'.

  grid(nx=NA, ny=NULL, col="lightgray", lty="dotted", lwd=par("lwd"))
  abline(v=xaxisTicks, col="lightgray", lty="dotted", lwd=par("lwd"))

  ## Plot model averages.
  year <- attr(m, "index")
  modelsMiddle <- by(t(m), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- NA; if (!all(is.na(x))) rv <- do.call(center_fun, list(x=x, na.rm=TRUE)); return (rv) }) })
  if (smooth_center)
    modelsMiddle <- sapply(modelsMiddle, function(m) predict(loess(m ~ year), data.frame(year=year)), simplify=FALSE)
  meanColor <- col_m_mean
  if (is.null(meanColor))
    meanColor <- gray(0.6)
  if (length(meanColor) == 1L && length(scenario) > 1L)
    meanColor <- vary_brightness(meanColor, length(scenario))
  if (length(meanColor) < length(scenario))
    meanColor <- rep(meanColor, length.out=length(scenario))
  mapply(function(mm, color) points(year, mm, type="l", col=color, lwd=2), modelsMiddle, meanColor)

  ## Calculate 95% model-range envelope.

  ecd <- (1.0 - envelope_coverage) / 2
  modelsRange <- switch(envelope_type,
    range = by(t(m), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- rep(NA, 2); if (!all(is.na(x))) { rv <- range(x, na.rm=TRUE); margin <- diff(rv) * ecd; rv <- rv + c(margin, -margin) }; return (rv) }) }),

    quantiles = by(t(m), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- rep(NA, 2); if (!all(is.na(x))) rv <- quantile(x, c(ecd, 1.0 - ecd), na.rm=TRUE); return (rv) }) }),

    normal =  by(t(m), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- rep(NA, 2); if (!all(is.na(x))) rv <- t.test(x, conf.level=envelope_coverage)$conf_int; return (rv) }) })
  )

  lowerEnvelope <- sapply(modelsRange, function(x) x[1L, ], simplify=FALSE); upperEnvelope <- sapply(modelsRange, function(x) x[2L, ], simplify=FALSE)
  ## Smooth the envelope.
  if (smooth_envelope) {
    lowerEnvelope <- sapply(lowerEnvelope, function(e) predict(loess(e ~ year), data.frame(year=year)), simplify=FALSE)
    upperEnvelope <- sapply(upperEnvelope, function(e) predict(loess(e ~ year), data.frame(year=year)), simplify=FALSE)
  }

  ## Plot the envelope.
  if (plot_envelope) {
    mapply(function(le, color) points(year, le, type="l", col=color, lwd=1, lty=3L), lowerEnvelope, meanColor)
    mapply(function(ue, color) points(year, ue, type="l", col=color, lwd=1, lty=3L), upperEnvelope, meanColor)

    if (!is.null(alpha_envelope)) {
      mapply(function(le, ue, color) {
        cidf <- data.frame(yr_part=m_yr_part, lower=le, upper=ue); cidf <- cidf[complete.cases(cidf), ]
        ciCol <- alpha(color, alpha_envelope)
        polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
      }, lowerEnvelope, upperEnvelope, meanColor)
    }
  }

  ## Plot instrumental series.
  if (plotInstrumental) {
    if (is.null(series))
      series <- setdiff(get_climate_series_names(instrumental), omit_series)
    col_i_funArgs <- list(
      n = length(series)
    )
    col_i_funArgs <- modifyList(col_i_funArgs, col_i_fun...)
    instrumentalColors <- suppressWarnings(alpha(do.call(col_i_fun, col_i_funArgs), alpha_i))
    length(instrumentalColors) <- length(series)
    names(instrumentalColors) <- series

    plot_iArgs <- list(
      x = ima[, series],
      screens = 1L,
      bty = "n",
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      ylim = ylim,
      col = instrumentalColors,
      lwd = 2
    )
    plot_iArgs <- modifyList(plot_iArgs, plot_i...)

    cis <- i[, grepl("_uncertainty$", colnames(i))]
    if (conf_int_i) { # Plot confidence bands for temp series that have them.
      confintNames <- intersect(series %_% "_uncertainty", colnames(cis))
        if (length(confintNames) != 0) {
        seriesNames <- str_match(confintNames, "^(.*?)_uncertainty$")[, 2L]
        for (j in seq_along(confintNames)) {
          value <- i[, seriesNames[j]]
          ci <- cis[, confintNames[j]]
          upper <- window(MA(value + ci/2, ma_i), start, end, extend=TRUE); lower <- window(MA(value - ci/2, ma_i), start, end, extend=TRUE)
          ciCol <- alpha(instrumentalColors[seriesNames[j]], ci_alpha_i)
          cidf <- data.frame(yr_part=i_yr_part, lower=lower, upper=upper); cidf <- cidf[complete.cases(cidf), ]
          polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
        }
      }
    }

    par(new=TRUE)
    do.call("plot", plot_iArgs) # I.e. "plot.zoo".
  }

  ## Make legend.
  numColumns <- 1L
  legendText <- paste(ensemble %_% " ensemble runs", paste(" (", scenario, ")", sep=""), sep="")
  legendColors <- modelColors
  legendLwd <- rep(1, length(scenario))
  legendLty <- rep(1, length(scenario))
  if (plot_envelope) {
    legendText <- c(legendText, sprintf("%1.0f", 100 * envelope_coverage) %_% "% " %_% envelope_text %_% ifelse(smooth_envelope, " (LOESS-smoothed)", ""))
    legendColors <- c(legendColors, "black")
    legendLwd <- c(legendLwd, 1)
    legendLty <- c(legendLty, 3)
  }
  if (plotInstrumental) {
    legendText <- c(legendText, paste(series, ma_iText, sep=""))
    legendColors <- c(legendColors, instrumentalColors[seq_along(series)])
    legendLwd <- c(legendLwd, rep(2, length(series)))
    legendLty <- c(legendLty, rep(1, length(series)))
  }
  numRows <- length(legendText)
  numColumns <- 2L
  legendText <- c(legendText, paste("central tend.", paste(" (", scenario, ")", sep=""), sep=""))
  blanks <- rep(NA, numRows * numColumns - length(legendText))
  legendText <- c(legendText, blanks)
  legendColors <- c(legendColors, meanColor, blanks)
  legendLwd <- c(legendLwd, rep(2, length(scenario)), blanks)
  legendLty <- c(legendLty, rep(1, length(scenario)), blanks)

  legendArgs <- list(
    x = "topleft",
    legend = legendText,
    col = legendColors,
    lwd = legendLwd,
    lty = legendLty,
    bty = "n",
    cex = 0.8,
    ncol = numColumns
  )
  legendArgs <- modifyList(legendArgs, legend...)
  do.call("legend", legendArgs)
}
