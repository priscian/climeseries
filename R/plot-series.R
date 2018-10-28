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
#' series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global", "BEST Global (Water Ice Temp.)", "JMA Global", "RSS TLT 3.3 -70.0/82.5", "UAH TLT 6.0 Global", "RATPAC-A 850-300 mb Global")
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
#' ## Plot instrumental series and trend line.
#' ########################################
#'
#' series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "RSS TLT 3.3 -70.0/82.5", "UAH TLT 6.0 Global")
#' plot_climate_data(inst, series=series, 1979, ma=12, lwd=2, show_trend=TRUE)
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
plot_climate_data <- function(x, series, start=NULL, end=NULL, ma=NULL, baseline=NULL, yearly=FALSE, plot_type=c("single", "multiple"), type="l", xlab="Year", ylab=NULL, unit=NULL, main=NULL, col=NULL, col_fun=RColorBrewer::brewer.pal, col_fun...=list(name="Paired"), alpha=0.5, lwd=2, add = FALSE, conf_int=FALSE, ci_alpha=0.3, trend=FALSE, trend_legend_inset=c(0.2, 0.2), loess=FALSE, loess...=list(), get_x_axis_ticks...=list(), segmented=FALSE, segmented...=list(), plot.segmented...=list(), mark_segments=FALSE, vline...=list(), make_standardized_plot_filename...=list(), start_callback=NULL, end_callback=NULL, save_png=FALSE, save_png_dir, png...=list(), ...)
{
  plot_type <- match.arg(plot_type)

  ## This is to avoid an roxygen error described here: https://github.com/klutometis/roxygen/issues/592
  if (is.null(unit))
    unit <- "\u00b0C"

  savedBaseline <- attr(x, "baseline")

  allNames <- c(get_climate_series_names(x, conf_int=!conf_int, invert=FALSE), series)
  allNames <- intersect(c(common_columns, series, series %_% "_uncertainty"), allNames)
  x <- x[, allNames]
  x <- subset(x, na_unwrap(x[, get_climate_series_names(x, conf_int=TRUE)])) # Remove trailing NAs.
  attr(x, "baseline") <- savedBaseline

  if (!is.null(baseline))
    x <- recenter_anomalies(x, baseline, conf_int=FALSE)

  y <- make_time_series_from_anomalies(x, conf_int=TRUE)
  ## Get date range of 'y' before it's converted to a yearly time series.
  flit <- window_ts(y, start, end, extend=TRUE)
  startTS_abo <- nearest_year_month_from_numeric(flit[, "yr_part"], tsp(flit)[1], "above")
  endTS_abo <- nearest_year_month_from_numeric(flit[, "yr_part"], tsp(flit)[2], "below")
  textRange <- paste(paste(startTS_abo, collapse="."), paste(endTS_abo, collapse="."), sep="-"); flit <- NULL
  if (yearly) {
    y <- make_yearly_data(y)
    y$yr_part <- y$year
    y <- make_time_series_from_anomalies(y, frequency=1L, conf_int=conf_int)
    ma <- NULL
  }
  w <- interpNA(y, "linear", unwrap=TRUE)

  climateSeriesNames <- setdiff(allNames, common_columns)
  w[, climateSeriesNames] <- MA(w[, climateSeriesNames], ma)
  ## [13 Oct. 2017] Make sure to window the time series only AFTER applying the moving average.
  y <- window_ts(y, start, end, extend=TRUE)
  w <- window_ts(w, start, end, extend=TRUE)
  maText <- ""
  if (!is.null(ma))
    maText <- "(" %_% ma %_% "-month moving average)"

  baselineText <- ""
  baseline <- attr(x, "baseline")
  if (!is.null(baseline))
    baselineText <- " w.r.t. " %_% min(baseline) %_% "\u2013" %_% max(baseline)

  if (is.null(ylab))
    ylab <- eval(substitute(expression(paste("Temperature Anomaly (", phantom(l), unit, ")", b, sep="")), list(b=baselineText, unit=unit)))
  if (is.null(main))
    main <- "Average Temperature"

  startTS <- nearest_year_month_from_numeric(w[, "yr_part"], tsp(w)[1], "above")
  endTS <- nearest_year_month_from_numeric(w[, "yr_part"], tsp(w)[2], "below")

  if (yearly)
    main <- paste(main, " (", sprintf("%04d", startTS_abo[1L]), "\u2013", sprintf("%04d", endTS_abo[1L]), ")", sep="")
  else
    main <- paste(main, " (", MOS[startTS[2L]], ". ", sprintf("%04d", startTS[1L]), "\u2013", MOS[endTS[2L]], ". ", sprintf("%04d", endTS[1L]), ")", sep="")

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

  get_x_axis_ticks <- function(min, max, by)
  {
    yearGroups <- seq(min, max, by=by)
    plotStart <- startTS[1]
    plotEnd <- endTS[1]

    plotStart <- nearest_below(yearGroups, plotStart, TRUE); plotEnd <- nearest_above(yearGroups, plotEnd, TRUE)
    xaxisTicks <- seq(plotStart, plotEnd, by=by)

    return (xaxisTicks)
  }

  get_x_axis_ticksArgs <- list(
    min = 1500,
    max = 3000,
    by = 10
  )
  get_x_axis_ticksArgs <- modifyList(get_x_axis_ticksArgs, get_x_axis_ticks...)

  xaxisTicks <- do.call("get_x_axis_ticks", get_x_axis_ticksArgs)
  if (length(xaxisTicks) < 8L) {
    get_x_axis_ticksArgs$by = 5
    get_x_axis_ticksArgs <- modifyList(get_x_axis_ticksArgs, get_x_axis_ticks...)
    xaxisTicks <- do.call("get_x_axis_ticks", get_x_axis_ticksArgs)
  }

  make_standardized_plot_filenameArgs <- list(
    x = w,
    ma = ma,
    range = textRange,
    yearly = yearly,
    conf_int = conf_int,
    baseline = baseline,
    loess = loess,
    trend = trend,
    segmented = segmented,
    series_max_length = 75L
  )
  make_standardized_plot_filenameArgs <- modifyList(make_standardized_plot_filenameArgs, make_standardized_plot_filename...)
  filename <- do.call(make_standardized_plot_filename, make_standardized_plot_filenameArgs)

  if (missing(save_png_dir)) {
    if (!is.null(getOption("climeseries_image_dir")))
      imageDir <- getOption("climeseries_image_dir")
    else
      imageDir <- "."
  }
  else
    imageDir <- save_png_dir

  if (save_png) {
    pngArgs <- list(
      filename = paste(imageDir, filename, sep="/"),
      width = 12.5,
      height = 7.3,
      units = "in",
      res = 600
    )
    pngArgs <- modifyList(pngArgs, png...)
    do.call("png", pngArgs)
  }

  xaxt <- "n"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.

  if (!add)
    plot(w[, get_climate_series_names(w, conf_int=FALSE)], plot.type=plot_type, type="n", xaxs="r", xaxt=xaxt, xlab=xlab, ylab=ylab, main=main, frame.plot=FALSE, ...) # I.e. 'plot.ts()'.
  if (xaxt == "n")
    axis(1, xaxisTicks)
  else
    xaxisTicks <- axTicks(1L)
  if (maText != "") mtext(maText, 3L)

  if (!add) {
    grid(nx=NA, ny=NULL, col="lightgray", lty="dotted", lwd=par("lwd"))
    abline(v=xaxisTicks, col="lightgray", lty="dotted", lwd=par("lwd"))
  }

  if (!is.null(start_callback))
    eval(start_callback)

  if (conf_int) { # Plot confidence bands for temp series that have them.
    confintNames <- intersect(series %_% "_uncertainty", colnames(w))
    if (length(confintNames) != 0L) {
      seriesNames <- str_match(confintNames, "^(.*?)_uncertainty$")[, 2L]
      for (i in seq_along(confintNames)) {
        value <- w[, seriesNames[i]]
        ci <- w[, confintNames[i]]
        upper <- value + ci/2; lower <- value - ci/2
        ciCol <- alpha(col[seriesNames[i]], ci_alpha)
        cidf <- data.frame(yr_part=y[, "yr_part"], lower=lower, upper=upper); cidf <- cidf[complete.cases(cidf), ]
        polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
      }
    }
  }

  ## Convert to 'zoo' object for plotting.
  wz <- as.zoo(w)

  par(new = TRUE)
  if (add) {
    plotSeries <- get_climate_series_names(w, conf_int = FALSE)
    for (i in seq_along(plotSeries))
      lines(wz[, plotSeries[i]], type = type, col = col[i], lwd = lwd, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...) # I.e. 'plot.zoo()'.
  }
  else
    plot(wz[, get_climate_series_names(w, conf_int = FALSE)], screens = 1L, plot.type = plot_type, type = type, col = col, lwd = lwd, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...) # I.e. 'plot.zoo()'.

  legend(x="topleft", legend=series %_% ifelse(loess, " (+ LOESS)", ""), col=col, lwd=lwd, bty="n", cex=0.8)

  ## LOESS smooth.
  if (loess) {
    for (s in series) {
      loessArgs = list(
        formula = eval(substitute(s ~ yr_part, list(s=as.name(s)))),
        data = y,
        span = 0.2
      )
      loessArgs <- modifyList(loessArgs, loess...)

      l <- do.call("loess", loessArgs)

      lines(l$x, l$fit, col=col[s], lwd=2)
    }
  }

  r <- list()

  ## Decadal linear trends.
  if (trend) {
    m <- list()
    m$series <- series
    m$range <- list(start=startTS, end=endTS)
    m$col <- col
    m$data <- y[, c(intersect(common_columns, colnames(y)), series)]
    for (s in m$series) {
      m[[s]]$lm <- lm(eval(substitute(b ~ yr_part, list(b=as.symbol(s)))), data=m$data, x=TRUE)
      m[[s]]$warming <- coef(m[[s]]$lm)[2] * diff(range(m[[s]]$lm$model[, 2]))
      m[[s]]$rate <- coef(m[[s]]$lm)[2] * 10
      m[[s]]$rateText <- eval(substitute(expression(paste(Delta, " = ", r, phantom(l), unit, "/dec.", sep="")), list(r=sprintf(m[[s]]$rate, fmt="%+1.3f"), unit=unit)))
      m[[s]]$col <- m$col[s]
    }

    legendText <- NULL
    for (s in m$series) {
      ## Set clipping region for 'abline()'.
      xRange <- range(wz[!is.na(wz[, s]), "yr_part"], na.rm = TRUE)
      yRange <- range(wz[, s], na.rm = TRUE)
      usr <- par("usr")
      clip(xRange[1], xRange[2], yRange[1], yRange[2])

      abline(m[[s]]$lm, col=m[[s]]$col, lwd=2)

      ## Reset clipping to plot region.
      do.call("clip", as.list(usr))

      legendText <- c(legendText, m[[s]]$rateText)
    }

    if (!is.null(trend_legend_inset))
      legend("bottomright", inset=trend_legend_inset, legend=legendText, col=m$col, lwd=2, bty="n", cex=0.8)

    r$trend <- m
  }

  if (segmented) {
    segmentedArgs <- list(
      x = x,
      series = series,
      col = col,
      start = start,
      end = end
    )
    segmentedArgs <- modifyList(segmentedArgs, segmented...)
    sm <- do.call("fit_segmented_model", segmentedArgs)
    # sapply(sm$piecewise, function(a) a$sm$psi, simplify=FALSE)

    for (i in names(sm$piecewise)) {
      ## Set clipping region for 'plot.segmented()' and 'abline()'.
      xRange <- range(wz[!is.na(wz[, i]), "yr_part"], na.rm = TRUE)
      yRange <- range(wz[, i], na.rm = TRUE)
      usr <- par("usr")
      clip(xRange[1], xRange[2], yRange[1], yRange[2])

      x <- sm$piecewise[[i]]$sm

      if (!is.null(x)) {
        plot.segmentedArgs <- list(
          x = x,
          add = TRUE,
          rug = FALSE,
          lwd = 2,
          #lty = "longdash",
          col = col[i],
          alpha = alpha
        )
        plot.segmentedArgs <- modifyList(plot.segmentedArgs, plot.segmented...)
        dev_null <- do.call("plot", plot.segmentedArgs)

        if (mark_segments) {
          vlineArgs <- list(
            mark_years = sprintf(sm$piecewise[[i]]$sm$psi[, 2], fmt="%1.1f")
          )
          vlineArgs <- modifyList(vlineArgs, vline...)
          do.call("vline", vlineArgs)
        }
      } else {
        lwd <- ifelse(is.null(plot.segmented...$lwd), 2, plot.segmented...$lwd)
        abline(sm$piecewise[[i]]$lm, col=col[i], lwd=lwd)
      }

      ## Reset clipping to plot region.
      do.call("clip", as.list(usr))
    }

    r$segmented <- sm
  }

  if (!is.null(end_callback))
    eval(end_callback)

  if (save_png)
    dev.off()

  cat("Standardized file name:", filename, fill=TRUE); flush.console()

  if (length(r) > 0L)
    return (invisible(r))

  return (nop())
}


## Make a standardized file name. Primarily for internal use in 'plot_climate_data()' and 'plot_models_and_climate_data()'.
make_standardized_plot_filename <- function(x, prefix=NULL, suffix=NULL, conf_int, ma, ma_i, yearly, range, baseline, loess, trend, segmented, series_max_length=Inf, series_override, series_sep="+", sep="_", conf_int_id=".ci", loess_id="loess", trend_id="trend", segmented_id="seg", ext="png", model_details)
{
  modelString <- NULL
  if (!missing(model_details))
    modelString <- paste(model_details, collapse="-")

  if (missing(ma)) ma <- NULL
  if (missing(yearly)) yearly <- FALSE
  if (missing(baseline)) baseline <- NULL
  if (missing(conf_int)) conf_int <- FALSE
  if (missing(loess)) loess <- FALSE
  if (missing(trend)) trend <- FALSE
  if (missing(segmented)) segmented <- FALSE

  seriesString <- NULL
  if (!missing(x)) {
    series <- get_climate_series_names(x)
    if (conf_int) {
      confintNames <- intersect(series %_% "_uncertainty", colnames(x))
      if (length(confintNames) > 0L) {
        index <- unlist(sapply(series, function(a) any(grepl(a, confintNames, fixed=TRUE)), simplify=TRUE))
        series[index] <- series[index] %_% conf_int_id
      }
    }

    for (i in rev(seq_along(series))) {
      seriesString <- paste(series[seq(i)], collapse=series_sep)
      if (nchar(seriesString) < series_max_length) {
        if (i < length(series))
          seriesString <- paste(seriesString, "&c", sep=series_sep)
        break
      }
    }

    ## Replace character incompatible with the OS file system.
    seriesString <- gsub("\\/", "\\-", seriesString)

    startTS <- nearest_year_month_from_numeric(x[, "yr_part"], tsp(x)[1], "above")
    endTS <- nearest_year_month_from_numeric(x[, "yr_part"], tsp(x)[2], "below")
  }

  parts <- list()
  if (!is.null(modelString)) parts$models <- modelString
  if (!is.null(seriesString)) parts$series <- seriesString

  if (!missing(series_override))
    parts$series <- series_override

  if (missing(range))
    parts$range <- paste(paste(startTS, collapse="."), paste(endTS, collapse="."), sep="-")
  else
    parts$range <- range

  parts$moving_average <- "ma" %_% ifelse(is.null(ma), 0, ma)
  if (yearly)
    parts$moving_average <- "yearly"

  if (!missing(ma_i)) {
    parts$moving_average_i <- "mai" %_% ifelse(is.null(ma_i), 0, ma_i)
    if (yearly)
      parts$moving_average_i <- NULL
  }

  if (!is.null(baseline))
    parts$baseline <- "baseline" %_% paste(head(baseline, 1L), tail(baseline, 1L), sep="-")

  if (loess)
    parts$loess <- loess_id

  if (trend)
    parts$trend <- trend_id

  if (segmented)
    parts$segmented <- segmented_id

  r <- paste(paste0(prefix, paste(parts, collapse=sep), suffix), ext, sep=".")

  r
}
## usage:
# make_standardized_plot_filename(w, ma, yearly, conf_int, loess, trend, series_max_length=75)


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
# rates <- plot_sequential_trend("RSS TLT 4.0 -70.0/82.5")
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
plot_models_and_climate_data <- function(instrumental, models, series=NULL, scenario=NULL, start=1880, end=NULL, ma=NULL, ma_i=ma, baseline=NULL, yearly=FALSE, ylim=c(-1.0, 1.0), scenario_text="Scenario Realizations", center_fun="mean", smooth_center=FALSE, envelope_coverage=0.95, envelope_type=c("quantiles", "range", "normal"), plot_envelope=TRUE, smooth_envelope=TRUE, unit=NULL, col_m=NULL, col_m_mean=NULL, alpha_envelope=0.2, envelope_text="model coverage", legend...=list(), plot_i...=list(), col_i_fun=RColorBrewer::brewer.pal, col_i_fun...=list(name="Paired"), alpha_i=0.5, conf_int_i=FALSE, ci_alpha_i=0.3, make_standardized_plot_filename...=list(), start_callback=NULL, end_callback=NULL, save_png=FALSE, save_png_dir, png...=list(), ...)
{
  envelope_type <- match.arg(envelope_type)

  ## This is to avoid an roxygen error described here: https://github.com/klutometis/roxygen/issues/592
  if (is.null(unit))
    unit <- "\u00b0C"

  plotInstrumental <- TRUE
  if (is.null(series))
    plotInstrumental <- FALSE

  if (plotInstrumental) {
    savedBaseline <- attr(instrumental, "baseline")

    allNames <- c(get_climate_series_names(instrumental, conf_int=!conf_int_i, invert=FALSE), series)
    allNames <- intersect(c(common_columns, series, series %_% "_uncertainty"), allNames)
    instrumental <- instrumental[, allNames]
    instrumental <- subset(instrumental, na_unwrap(instrumental[, get_climate_series_names(instrumental, conf_int=TRUE)])) # Remove trailing NAs.
    attr(instrumental, "baseline") <- savedBaseline

    instrumental <- recenter_anomalies(instrumental, baseline)
  }

  models <- recenter_anomalies(models, baseline)

  ensemble <- attr(models, "ensemble")
  if (is.null(baseline))
    baseline <- attr(models, "baseline")
  model_type <- attr(models, "model_type")

  originalScenario <- attr(models, "scenario")
  keepCols <- names(models)
  if (!is.null(scenario)) {
    keepCols <- c(common_columns, setdiff(names(models), common_columns)[originalScenario %in% scenario])
    models <- models[, keepCols, drop=FALSE]

    # Restore some attributes.
    attr(models, "ensemble") <- ensemble
    attr(models, "model_type") <- model_type
    attr(models, "scenario") <- factor(originalScenario[originalScenario %in% scenario], levels=scenario[scenario %in% originalScenario])
    attr(models, "baseline") <- baseline
  }
  else
    scenario <- levels(attr(models, "scenario"))

  m <- make_time_series_from_anomalies(models)
  i <- make_time_series_from_anomalies(instrumental, conf_int=TRUE)

  ## Get date range of 'i' before it was converted to a yearly time series.
  flit <- window_ts(m, start[1], end[1], extend=TRUE)
  startTS <- nearest_year_month_from_numeric(flit[, "yr_part"], tsp(flit)[1], "above")
  endTS <- nearest_year_month_from_numeric(flit[, "yr_part"], tsp(flit)[2], "below")
  textRange <- paste(paste(startTS, collapse="."), paste(endTS, collapse="."), sep="-"); flit <- NULL

  if (yearly) {
    i <- make_yearly_data(i)
    i$yr_part <- i$year
    i <- make_time_series_from_anomalies(i, frequency=1)
    ma_i <- NULL

    m <- make_yearly_data(m)
    m$yr_part <- m$year
    m <- make_time_series_from_anomalies(m, frequency=1)
    ma <- NULL
  }

  wi <- interpNA(i, "linear", unwrap=TRUE)
  wm <- interpNA(m, "linear", unwrap=TRUE)

  if (plotInstrumental) {
    climateSeriesNames <- setdiff(allNames, common_columns)
    wi[, climateSeriesNames] <- MA(wi[, climateSeriesNames, drop=FALSE], ma_i)
  }
  wm[, get_climate_series_names(wm)] <- MA(wm[, get_climate_series_names(wm), drop=FALSE], ma)

  m <- window_ts(m, start[1], end[1], extend=TRUE) # Not necessary?
  wm <- window_ts(wm, start[1], end[1], extend=TRUE)
  i <- window_ts(i, start[1], end[1], extend=TRUE) # Not necessary?
  wi <- window_ts(wi, start[1], end[1], extend=TRUE)

  maText <- ma_iText <- ""
  if (!is.null(ma))
    maText <- "(" %_% ma %_% "-month moving average)"

  if (!is.null(ma_i)) {
    if (ma_i != ma)
      ma_iText <- " (" %_% ma_i %_% "-mo. m.a.)"
  }
  else {
    if (!is.null(ma))
      ma_iText <- " (no m.a.)"
  }

  ## Convert to 'zoo' objects for plotting.
  wiz <- as.zoo(wi)
  wmz <- as.zoo(wm)

  baselineText <- ""
  baselineAttribute <- attr(instrumental, "baseline")
  if (!is.null(baselineAttribute))
    baselineText <- " w.r.t. " %_% min(baseline) %_% "\u2013" %_% max(baseline)

  xlab <- "Year"
  ylab <- eval(substitute(expression(paste("Temperature Anomaly (", phantom(l), unit, ")", b, sep="")), list(b=baselineText, unit=unit)))
  main <- paste(ensemble, " ", scenario_text, sep="")
  if (yearly)
    main <- paste(main, " (", sprintf("%04d", startTS[1L]), "\u2013", sprintf("%04d", endTS[1L]), ")", sep="")
  else
    main <- paste(main, " (", MOS[startTS[2L]], ". ", sprintf("%04d", startTS[1L]), "\u2013", MOS[endTS[2L]], ". ", sprintf("%04d", endTS[1L]), ")", sep="")


  GetXAxisTicks <- function(min=1800, max=3000, by=10)
  {
    yearGroups <- seq(min, max, by=by)
    plotStart <- startTS[1]
    plotEnd <- endTS[1]

    plotStart <- nearest_below(yearGroups, plotStart, TRUE); plotEnd <- nearest_above(yearGroups, plotEnd, TRUE)
    xaxisTicks <- seq(plotStart, plotEnd, by=by)

    return (xaxisTicks)
  }

  xaxisTicks <- GetXAxisTicks()
  if (length(xaxisTicks) < 8L)
    xaxisTicks <- GetXAxisTicks(by=5)

  make_standardized_plot_filenameArgs <- list(
    x = wi,
    ma = ma,
    ma_i = ma_i,
    yearly = yearly,
    range = textRange,
    baseline = baseline,
    conf_int = conf_int_i,
    series_max_length = 75L,
    model_details = list(
      ensemble = tolower(ensemble),
      model_type = gsub("\\s+", "-", gsub("\\s+\\+\\s+", "+", tolower(attr(models, "model_type")))),
      scenario = paste(gsub("\\s+", "", tolower(scenario)), collapse="+"),
      realizations = paste("realizations", envelope_type, sep=".")
    )
  )
  if (!plotInstrumental) { make_standardized_plot_filenameArgs$x <- NULL; make_standardized_plot_filenameArgs$ma_i <- NULL }
  ## N.B. These aren't actually plot args (so they'll fail), but I'll leave them in case of further expansion of inst. plots here:
  if(!is.null(plot_i...$loess)) make_standardized_plot_filenameArgs$loess <- plot_i...$loess
  if(!is.null(plot_i...$trend)) make_standardized_plot_filenameArgs$trend <- plot_i...$trend
  make_standardized_plot_filenameArgs <- modifyList(make_standardized_plot_filenameArgs, make_standardized_plot_filename...)
  filename <- do.call(make_standardized_plot_filename, make_standardized_plot_filenameArgs)

  if (missing(save_png_dir)) {
    if (!is.null(getOption("climeseries_image_dir")))
      imageDir <- getOption("climeseries_image_dir")
    else
      imageDir <- "."
  }
  else
    imageDir <- save_png_dir

  if (save_png) {
    pngArgs <- list(
      filename = paste(imageDir, filename, sep="/"),
      width = 12.5,
      height = 7.3,
      units = "in",
      res = 600
    )
    pngArgs <- modifyList(pngArgs, png...)
    do.call("png", pngArgs)
  }

  xaxt <- "n"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.
  plot(wiz[, get_climate_series_names(wiz, conf_int=FALSE), drop=FALSE], screens=1L, bty="n", xaxs="r", xaxt=xaxt, xlab=xlab, ylab=ylab, main=main, type="n", ylim=ylim, ...) # I.e. 'plot.zoo()'.
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
  plot(wmz[, get_climate_series_names(wmz), drop=FALSE], screens=1, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=ylim, col=eachModelColor, ...) # I.e. 'plot.zoo()'.

  grid(nx=NA, ny=NULL, col="lightgray", lty="dotted", lwd=par("lwd"))
  abline(v=xaxisTicks, col="lightgray", lty="dotted", lwd=par("lwd"))

  if (!is.null(start_callback))
    eval(start_callback)

  ## Plot model averages.
  year <- as.numeric(attr(wmz, "index"))
  modelsMiddle <- by(t(wmz[, get_climate_series_names(wmz), drop=FALSE]), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- NA; if (!all(is.na(x))) rv <- do.call(center_fun, list(x=x, na.rm=TRUE)); return (rv) }) })
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
    range = by(t(wmz[, get_climate_series_names(wmz), drop=FALSE]), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- rep(NA, 2); if (!all(is.na(x))) { rv <- range(x, na.rm=TRUE); margin <- diff(rv) * ecd; rv <- rv + c(margin, -margin) }; return (rv) }) }),

    quantiles = by(t(wmz[, get_climate_series_names(wmz), drop=FALSE]), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- rep(NA, 2); if (!all(is.na(x))) rv <- quantile(x, c(ecd, 1.0 - ecd), na.rm=TRUE); return (rv) }) }),

    normal = by(t(wmz[, get_climate_series_names(wmz), drop=FALSE]), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- rep(NA, 2); if (!all(is.na(x))) rv <- t.test(x, conf.level=envelope_coverage)$conf_int; return (rv) }) })
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
        cidf <- data.frame(yr_part=wmz[, "yr_part"], lower=le, upper=ue); cidf <- cidf[complete.cases(cidf), ]
        ciCol <- alpha(color, alpha_envelope)
        polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
      }, lowerEnvelope, upperEnvelope, meanColor)
    }
  }

  ## Plot instrumental series.
  if (plotInstrumental) {
    col_i_funArgs <- list(
      n = length(series)
    )
    col_i_funArgs <- modifyList(col_i_funArgs, col_i_fun...)
    instrumentalColors <- suppressWarnings(alpha(do.call(col_i_fun, col_i_funArgs), alpha_i))
    length(instrumentalColors) <- length(series)
    names(instrumentalColors) <- series

    plot_iArgs <- list(
      x = wiz[, get_climate_series_names(wiz, conf_int=FALSE)],
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

    cis <- i[, grepl("_uncertainty$", colnames(i)), drop=FALSE]
    if (conf_int_i) { # Plot confidence bands for temp series that have them.
      confintNames <- intersect(series %_% "_uncertainty", colnames(cis))
        if (length(confintNames) != 0) {
        seriesNames <- str_match(confintNames, "^(.*?)_uncertainty$")[, 2L]
        for (j in seq_along(confintNames)) {
          value <- i[, seriesNames[j]]
          ci <- cis[, confintNames[j]]
          upper <- value + ci/2; lower <- value - ci/2
          ciCol <- alpha(instrumentalColors[seriesNames[j]], ci_alpha_i)
          cidf <- data.frame(yr_part=wiz[, "yr_part"], lower=lower, upper=upper); cidf <- cidf[complete.cases(cidf), ]
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

  if (!is.null(end_callback))
    eval(end_callback)

  if (save_png)
    dev.off()

  cat("Standardized file name:", filename, fill=TRUE); flush.console()

  return (nop())
}
