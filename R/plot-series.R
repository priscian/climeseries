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
plot_climate_data <- function(x, series, start=NULL, end=NULL, ma=NULL, baseline=NULL, yearly=FALSE, make_yearly_data...=list(), ma_sides=1L, interpolate = FALSE, plot_type=c("single", "multiple"), as_zoo = TRUE, type="l", bg = scales::alpha("gray", 0.1), xlab="Year", ylab=NULL, unit=NULL, main=NULL, col=NULL, col_fun=colorspace::rainbow_hcl, col_fun...=list(l = 65), alpha=0.9, lwd=2, legend... = list(), add = FALSE, conf_int=FALSE, ci_alpha=0.3, polygon... = list(), trend=FALSE, trend_lwd = lwd, trend_legend_inset=c(0.2, 0.2), print_trend_ci = TRUE, trend_format = ifelse(print_trend_ci, "1.3f", "1.2f"), trend... = list(), extra_trends = list(), loess=FALSE, loess...=list(), loess_series = NULL, lines.loess... = list(), xaxt = "n", get_x_axis_ticks...=list(), segmented=FALSE, segmented...=list(), plot.segmented...=list(), mark_segments=c("none", "lines", "points"), vline...=list(), points.segmented... = list(), make_standardized_plot_filename...=list(), start_callback=NULL, end_callback=NULL, sign = TRUE, sign_callback = rlang::expr(text(graphics::par("usr")[2], graphics::par("usr")[3], labels = "@priscian", adj = c(1.0, -0.5))), save_png=FALSE, save_png_dir, png...=list(), ...)
{
  plot_type <- match.arg(plot_type)
  mark_segments <- match.arg(mark_segments)

  ## This is to avoid an roxygen error described here: https://github.com/klutometis/roxygen/issues/592
  if (is.null(unit))
    unit <- "\u00b0C"

  savedBaseline <- attr(x, "baseline")

  allNames <- c(get_climate_series_names(x, conf_int = conf_int, invert = FALSE), series)
  allNames <- intersect(c(common_columns, series, series %_% "_uncertainty"), allNames)
  x <- x[, allNames]
  x <- subset(x, na_unwrap(x[, get_climate_series_names(x, conf_int = TRUE)])) # Remove trailing NAs.
  attr(x, "baseline") <- savedBaseline

  ## Is data already yearly?
  y <- make_time_series_from_anomalies(x, conf_int = TRUE, frequency = ifelse("yr_part" %nin% names(x), 1L, 12L))
  isAlreadyYearly <- stats::frequency(y) == 1L

  if (!is.null(baseline))
    y <- recenter_anomalies(y, baseline, by_month = !isAlreadyYearly, conf_int = FALSE)
  else
    attr(y, "baseline") <- attr(x, "baseline")
  baseline <- attr(y, "baseline")

  ## Save longer data set for possible autocorrelation correction
  y_full_baselined <- rlang::duplicate(y, shallow = FALSE)

  ## Get date range of 'y' before it's converted to a yearly time series.
  if (!isAlreadyYearly) {
    flit <- window_ts(y, start, end, extend = TRUE)
    startTS_abo <- nearest_year_month_from_numeric(flit[, "yr_part"], stats::tsp(flit)[1], "above")
    endTS_abo <- nearest_year_month_from_numeric(flit[, "yr_part"], stats::tsp(flit)[2], "below")
    flit <- NULL
    if (yearly) {
      make_yearly_dataArgs <- list(
        x = y
      )
      make_yearly_dataArgs <- utils::modifyList(make_yearly_dataArgs, make_yearly_data..., keep.null = TRUE)
      y <- do.call("make_yearly_data", make_yearly_dataArgs)
      y$yr_part <- y$year
      y <- make_time_series_from_anomalies(y, frequency = 1L, conf_int = conf_int)
      ma <- NULL
    }
  } else {
    flit <- window_default(y, start, end, extend = TRUE)
    startTS_abo <- stats::start(flit); startTS_abo[2] <- 0
    endTS_abo <- stats::end(flit); endTS_abo[2] <- 0
    y <- structure(cbind(y, yr_part = y[, "year"]), .Dimnames = list(NULL, c(colnames(y), "yr_part")))
    ma <- NULL
    yearly <- TRUE
  }
  textRange <- paste(paste(startTS_abo, collapse = "."), paste(endTS_abo, collapse = "."), sep = "-")

  w <- y
  local({
    interpCols <- NULL
    if (is.logical(interpolate)) {
      if (all(interpolate)) {
        w <<- interpNA(w, "linear", unwrap = TRUE)
        if (!is.null(ma)) warning("'interpolate = TRUE' with a non-null moving average may lead to inaccurate MAs.")
      }
      else if (any(interpolate)) {
        interpCols <- series[interpolate]
      }
    }
    else {
      interpCols <- interpolate
    }

    if (!is.null(interpCols)) {
      if (!is.null(ma)) warning("'interpolate = TRUE' with a non-null moving average may lead to inaccurate MAs.")
      for (i in interpCols)
        w[, i] <<- drop(interpNA(w[, i], "linear", unwrap = TRUE))
    }
  })

  climateSeriesNames <- setdiff(allNames, common_columns)
  if (!is.null(ma)) {
    w[, climateSeriesNames] <- MA(w[, climateSeriesNames], ma, sides=ma_sides)
  }
  ## [13 Oct. 2017] Make sure to window the time series only AFTER applying the moving average.
  y <- window_ts(y, start, end, extend=TRUE)
  w <- window_ts(w, start, end, extend=TRUE)
  maText <- ""
  if (!is.null(ma))
    maText <- "(" %_% ma %_% "-month moving average)"

  baselineText <- ""
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
    col_funArgs <- utils::modifyList(col_funArgs, col_fun..., keep.null = TRUE)
    col <- suppressWarnings(do.call(col_fun, col_funArgs))
    ## Some other possible function calls:
    #col <- rainbow(length(col))
    #col <- terrain.colors(length(col))
    #col <- topo.colors(length(col))
    #col <- suppressWarnings(brewer.pal(length(col),"Spectral")) # Or "Paired".
    #col <- matlab.like2(length(col)) # From package "colorRamps".
  }
  col <- rep(col, length.out=length(series))
  col <- scales::alpha(col, alpha)
  names(col) <- series

  get_x_axis_ticks <- function(min, max, by)
  {
    yearGroups <- seq(min, max, by = by)
    plotStart <- startTS[1]
    plotEnd <- endTS[1]

    plotStart <- nearest_below(yearGroups, plotStart, TRUE); plotEnd <- nearest_above(yearGroups, plotEnd, TRUE)
    xaxisTicks <- seq(plotStart, plotEnd, by = by)

    return (xaxisTicks)
  }

  get_x_axis_ticksArgs <- list(
    min = 1500,
    max = 3000,
    by = 10
  )
  get_x_axis_ticksArgs <- utils::modifyList(get_x_axis_ticksArgs, get_x_axis_ticks..., keep.null = TRUE)

  xaxisTicks <- do.call("get_x_axis_ticks", get_x_axis_ticksArgs)
  if (length(xaxisTicks) < 8L) {
    get_x_axis_ticksArgs$by = 5
    get_x_axis_ticksArgs <- utils::modifyList(get_x_axis_ticksArgs, get_x_axis_ticks..., keep.null = TRUE)
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
  make_standardized_plot_filenameArgs <- utils::modifyList(make_standardized_plot_filenameArgs, make_standardized_plot_filename..., keep.null = TRUE)
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
    pngArgs <- utils::modifyList(pngArgs, png..., keep.null = TRUE)
    do.call("png", pngArgs)
  }

  ## N.B. My custom x-axis ticks are causing some problems, so let's just ditch them for now (24 Sep 2020).
  xaxt <- xaxt
  #xaxt <- "s"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.

  if (!add) {
    op <- par(mar = c(5, 5, 4, 2) + 0.1)
    plot(w[, get_climate_series_names(w, conf_int=FALSE)], plot.type=plot_type, type="n", xaxs="r", xaxt=xaxt, axes = FALSE, xlab=xlab, ylab=ylab, main=main, frame.plot=FALSE, ...) # I.e. 'plot.ts()'.

    if (!is.null(bg)) {
      graphics::rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = NA, col = bg)
    }

    ## V. https://stackoverflow.com/questions/22470114/removing-top-and-right-borders-from-boxplot-frame-in-r/62185904#62185904
    #graphics::box(bty = "l") # L-shaped box
    graphics::axis(1, lwd = 0, lwd.ticks = 0) # Draw x-axis
    graphics::axis(2, lwd = 0, lwd.ticks = 0) # Draw y-axis
  }
  if (xaxt == "n") {
    #axis(1, xaxisTicks, lty = 0)
    mapply(axis, side = 1, at = xaxisTicks, labels = xaxisTicks, tick = FALSE)
  } else {
    xaxisTicks <- axTicks(1L)
  }
  if (maText != "") mtext(maText, 3L)

  if (!add) {
    grd_lty <- "solid" # Was "dotted"
    grid(nx = NA, ny = NULL, col = "gray", lty = grd_lty, lwd = par("lwd"))
    abline(v = xaxisTicks, col = "gray", lty = grd_lty, lwd = par("lwd"))
  }

  if (!is.null(start_callback))
    eval(start_callback)

  if (conf_int) { # Plot confidence bands for temp series that have them.
    confintNames <- intersect(series %_% "_uncertainty", colnames(w))
    if (length(confintNames) != 0L) {
      seriesNames <- stringr::str_match(confintNames, "^(.*?)_uncertainty$")[, 2L]
      for (i in seq_along(confintNames)) {
        value <- w[, seriesNames[i]]
        ci <- w[, confintNames[i]]
        upper <- value + ci/2; lower <- value - ci/2
        ciCol <- scales::alpha(col[seriesNames[i]], ci_alpha)
        cidf <- data.frame(yr_part=y[, "yr_part"], lower = lower, upper = upper); cidf <- cidf[complete.cases(cidf), ]
        polygonArgs <- list(
          x = c(cidf$yr_part, rev(cidf$yr_part)),
          y = c(cidf$upper, rev(cidf$lower)),
          ## N.B. If a CI doesn't show up, either set 'density = 50' (slow to save) or expand 'ylim'.
          density = NULL, # 50 lines/in produces nice CIs, too -- but leave NULL as the default
          col = ciCol,
          border = NA
        )
        polygonArgs <- utils::modifyList(polygonArgs, polygon..., keep.null = TRUE)

        #polygon(x = c(cidf$yr_part, rev(cidf$yr_part)), y = c(cidf$upper, rev(cidf$lower)), col = ciCol, border = NA)
        do.call(polygon, polygonArgs)
      }
    }
  }

  ## Convert to 'zoo' object for plotting.
  if (as_zoo)
    wz <- zoo::zoo(w)
  else
    wz <- w

  par(new = TRUE)
  if (add) {
    plotSeries <- get_climate_series_names(w, conf_int = FALSE)
    for (i in seq_along(plotSeries))
      lines(wz[, plotSeries[i]], type = type, col = col[i], lwd = lwd, bty = "n",
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...) # I.e. 'plot.zoo()'.
  }
  else
    plot(wz[, get_climate_series_names(w, conf_int = FALSE)], screens = 1L, plot.type = plot_type, type = type,
      col = col, lwd = lwd, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...) # I.e. 'plot.zoo()'.

  #legend(x = "topleft", legend = series %_% ifelse(loess, " (+ LOESS)", ""), col = col, lwd = lwd, bty = "n", cex = 0.8)
  legendArgs <- list(
    x = "topleft",
    legend = series %_% ifelse(loess, " (+ LOESS)", ""),
    col = col,
    lwd = lwd,
    bty = "n",
    cex = 0.8
  )
  legendArgs <- utils::modifyList(legendArgs, legend..., keep.null = TRUE)
  do.call("legend", legendArgs)

  ## LOESS smooth.
  ## N.B. This LOESS fit is done only over the abscissa range & should be used for display purposes only!
  ## For fits over the entire variable, use function 'add_loess_variables()'.
  if (loess) local({
    loessSeries <- series
    if (!is_invalid(loess_series))
      loessSeries <- loess_series
    for (s in loessSeries) {
      loessArgs = list(
        formula = eval(substitute(s ~ yr_part, list(s = as.name(s)))),
        data = y,
        span = NULL
      )
      loessArgs <- utils::modifyList(loessArgs, loess..., keep.null = TRUE)

      #l <- do.call("loess", loessArgs)
      l <- do.call(LOESS, loessArgs)

      lines.loessArgs <- list(
        x = drop(l$x),
        y = l$fit,
        lwd = 2
      )
      lines.loessArgs <- utils::modifyList(lines.loessArgs, lines.loess..., keep.null = TRUE)
      if (is_invalid(lines.loessArgs$col))
        lines.loessArgs$col = col[s]

      do.call(graphics::lines, lines.loessArgs)
    }
  })

  r <- list()

  ## Decadal linear trends.
  if (trend) local({
    trendArgs <- list(
      data = as.data.frame(y[, c(intersect(common_columns, colnames(y)), series)]),
      range = range(as.data.frame(w)[, "yr_part"], na.rm = TRUE),
      lwd = trend_lwd,
      legend_inset = trend_legend_inset,
      trend_multiplier = 10,
      rate_expression = sprintf("expression(Delta ~ \"= %%+%s ± %%%s %s/dec.\")", trend_format, trend_format, unit),
      keep_default_trends = TRUE,
      sort_by_name = FALSE
    )
    if (!print_trend_ci) {
      trendArgs$rate_expression <- sprintf("expression(Delta ~ \"= %%+%s %s/dec.\")", trend_format, unit)
    }
    if (!is_invalid(trend...$keep_default_trends))
      trendArgs$keep_default_trends <- trend...$keep_default_trends
    if (trendArgs$keep_default_trends) {
      if (is_invalid(trendArgs$m))
        trendArgs$m <- list()
      length(trendArgs$m) <- length(series); names(trendArgs$m) <- series
    }
    trendArgs <- utils::modifyList(trendArgs, trend...)
    if (!is_invalid(extra_trends))
      trendArgs$m <- append(trendArgs$m, extra_trends)

    for (i in seq_along(trendArgs$m)) {
      if (is_invalid(trendArgs$m[[i]]$range))
        trendArgs$m[[i]]$range <- trendArgs$range
      if (is_invalid(trendArgs$m[[i]]$col))
        trendArgs$m[[i]]$col <- col[names(trendArgs$m)[i]]
      if (is_invalid(trendArgs$m[[i]]$lwd))
        trendArgs$m[[i]]$lwd <- rep(trendArgs$lwd, length.out = length(trendArgs$m))[i]

      trendArgs$m[[i]]$sdata <- trendArgs$data %>%
        dplyr::select(c(intersect(common_columns, colnames(trendArgs$data)), names(trendArgs$m)[i])) %>%
        dplyr::filter(yr_part >= trendArgs$m[[i]]$range[1] & yr_part <= trendArgs$m[[i]]$range[2])
      trendArgs$m[[i]]$lm <- lm(eval(substitute(b ~ yr_part, list(b = as.symbol(names(trendArgs$m)[i])))),
        data = trendArgs$m[[i]]$sdata, x = TRUE, y = TRUE)
      trendArgs$m[[i]]$change <- coef(trendArgs$m[[i]]$lm)[2] * diff(range(trendArgs$m[[i]]$lm$model[, 2]))
      trendArgs$m[[i]]$rate <- coef(trendArgs$m[[i]]$lm)[2] * trendArgs$trend_multiplier
      trendArgs$m[[i]]$autocorrelation <- correct_monthly_autocorrelation(
        xdata = y_full_baselined[, "yr_part"],
        ydata = y_full_baselined[, names(trendArgs$m)[i]],
        mod = trendArgs$m[[i]]$lm
      )
      if (!print_trend_ci)
        trendArgs$m[[i]]$rateText <- eval_js(sprintf(trendArgs$rate_expression, trendArgs$m[[i]]$rate))
      else
        trendArgs$m[[i]]$rateText <-
          eval_js(sprintf(trendArgs$rate_expression, trendArgs$m[[i]]$rate, trendArgs$m[[i]]$autocorrelation$decadal_2sigma))
    }
    if (trendArgs$sort_by_name)
      trendArgs$m <- trendArgs$m[sort(names(trendArgs$m))]

    legendText <- NULL
    for (i in seq_along(trendArgs$m)) {
      ## Set clipping region for 'abline()'.
      xRange <- range(trendArgs$m[[i]]$sdata[!is.na(trendArgs$m[[i]]$sdata[, names(trendArgs$m)[i]]), "yr_part"], na.rm = TRUE)
      yRange <- range(trendArgs$m[[i]]$sdata[, names(trendArgs$m)[i]], na.rm = TRUE)

      usr <- par("usr")
      clip(xRange[1], xRange[2], yRange[1], yRange[2])

      abline(trendArgs$m[[i]]$lm, col = trendArgs$m[[i]]$col, lwd = trendArgs$m[[i]]$lwd)

      ## Reset clipping to plot region.
      do.call("clip", as.list(usr))

      legendText <- c(legendText, trendArgs$m[[i]]$rateText)
    }

    if (!is.null(trendArgs$legend_inset))
      legend("bottomright", inset = trendArgs$legend_inset, legend = legendText, col = sapply(trendArgs$m, function(a) a$col),
        lwd = sapply(trendArgs$m, function(a) a$lwd), bty = "n", cex = 0.8)

    r$trend <<- trendArgs$m
  })

  if (segmented) local({
    segmentedArgs <- list(
      x = y,  # Was 'x = x', which is wrong for certain baseline changes
      series = series,
      start = start,
      end = end,
      make_yearly_data... = make_yearly_data...
    )
    segmentedArgs <- utils::modifyList(segmentedArgs, segmented..., keep.null = TRUE)
    if (is_invalid(segmentedArgs$col))
      segmentedArgs$col <- col[segmentedArgs$series]
    sm <- do.call("fit_segmented_model", segmentedArgs)
    # sapply(sm$piecewise, function(a) a$sm$psi, simplify=FALSE)

    for (i in names(sm$piecewise)) {
      ## Set clipping region for 'plot.segmented()' and 'abline()'.
      xRange <- range(wz[!is.na(wz[, i]), "yr_part"], na.rm = TRUE)
      yRange <- range(wz[, i], na.rm = TRUE)
      usr <- par("usr")
      clip(xRange[1], xRange[2], yRange[1], yRange[2])

      x <- sm$piecewise[[i]]$sm

      plot.segmentedArgs <- list(
        x = x,
        add = TRUE,
        rug = FALSE,
        lwd = 2,
        #lty = "longdash",
        col = col[i],
        alpha = alpha
      )
      plot.segmentedArgs <- utils::modifyList(plot.segmentedArgs, plot.segmented..., keep.null = TRUE)
      ## 'alpha' may not have any effect, so explicitly apply it to 'col':
      plot.segmentedArgs$col <- scales::alpha(plot.segmentedArgs$col, plot.segmentedArgs$alpha)

      if (!is.null(x) && inherits(x, "segmented")) {
        dev_null <- do.call("plot", plot.segmentedArgs)

        if (mark_segments != "none") {
          ## Reset clipping to whole plot region.
          do.call("clip", as.list(usr))

          if (mark_segments == "lines") {
            vlineArgs <- list(
              mark_years = sprintf(sm$piecewise[[i]]$sm$psi[, 2], fmt="%1.1f")
            )
            vlineArgs <- utils::modifyList(vlineArgs, vline..., keep.null = TRUE)
            do.call("vline", vlineArgs)
          } else if (mark_segments == "points") {
            points.segmentedArgs <- list(
              x = sm$piecewise[[i]]$sm,
              col = col[i],
              pch = 4, # Like '×'
              alpha = alpha
            )
            points.segmentedArgs <- utils::modifyList(points.segmentedArgs, points.segmented..., keep.null = TRUE)
            ## 'alpha' may not have any effect, so explicitly apply it to 'col':
            points.segmentedArgs$col <- scales::alpha(points.segmentedArgs$col, points.segmentedArgs$alpha)
            points.segmentedArgs$alpha <- NULL
            do.call("points", points.segmentedArgs)
          }

          ## Turn clipping back on for any further plotting.
          clip(xRange[1], xRange[2], yRange[1], yRange[2])
        }
      } else {
        abline(sm$piecewise[[i]]$lm, col = scales::alpha(plot.segmentedArgs$col, plot.segmentedArgs$alpha), lwd = plot.segmentedArgs$lwd)
      }

      ## Reset clipping to plot region.
      do.call("clip", as.list(usr))
    }

    r$segmented <<- sm
  })

  if (!is.null(end_callback))
    poly_eval(end_callback)

  if (sign)
    poly_eval(sign_callback)

  if (save_png)
    dev.off()

  if (!add)
    par(op)

  cat("Standardized file name:", stringr::str_replace_all(filename, "%%", "%"), fill=TRUE); flush.console()

  if (length(r) > 0L)
    return (invisible(r))

  return (nop())
}


## Make a standardized file name. Primarily for internal use in 'plot_climate_data()' and 'plot_models_and_climate_data()'.
make_standardized_plot_filename <- function(x, prefix=NULL, suffix=NULL, conf_int, ma, ma_i, yearly, range, baseline, loess, trend, segmented, series_max_length=Inf, series_override, series_sep="+", sep="_", conf_int_id=".ci", loess_id="loess", trend_id="trend", segmented_id="seg", ext="png", model_details, illegal_replacement = "%%")
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

  if (is.logical(illegal_replacement) && illegal_replacement)
    illegal_replacement <- "%%"
  if (!is.logical(illegal_replacement))
    r <- stringr::str_replace_all(r, "[\\\\/:\"\\*\\?<>\\|]+", illegal_replacement)

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
  plotArgs <- utils::modifyList(plotArgs, plot..., keep.null = TRUE)

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
      col = scales::alpha("gray", 0.6),
      border = NA # Border color; NULL means use par("fg"), NA omits borders.
    )
    ciArgs <- utils::modifyList(ciArgs, ci..., keep.null = TRUE)

    do.call("polygon", ciArgs)
  }
  else { # Use error bars to show confidence intervals.
    ciArgs <- list(
      x0 = ccYears,
      y0 = ccRates[, "lwr"],
      x1 = ccYears,
      y1 = ccRates[, "upr"],
      length = 0.03,
      angle = 90,
      code = 3
    )
    #ciArgs <- merge.list(ci..., ciArgs)
    ciArgs <- utils::modifyList(ciArgs, ci..., keep.null = TRUE)

    do.call("arrows", ciArgs)
  }

  if (!is.null(mark_years)) {
    ablineArgs <- list(
      v = mark_years,
      col = scales::alpha("red", 0.4)
    )
    ablineArgs <- utils::modifyList(ablineArgs, abline..., keep.null = TRUE)

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
plot_models_and_climate_data <- function(instrumental, models, series=NULL, scenario=NULL, start=1880, end=NULL, ma=NULL, ma_i=ma, baseline=NULL, yearly=FALSE, ma_sides=1L, ylim=c(-1.0, 1.0), bg = scales::alpha("gray", 0.1), scenario_text="Scenario Realizations", center_fun="mean", smooth_center=FALSE, envelope_coverage=0.95, envelope_type=c("quantiles", "range", "normal"), plot_envelope=TRUE, smooth_envelope=TRUE, smooth_span = NULL, unit=NULL, col_m=NULL, col_m_mean=NULL, alpha_envelope=0.1, envelope_text="model coverage", legend...=list(), plot_i...=list(), col_i_fun=colorspace::rainbow_hcl, col_i_fun...=list(l = 65), alpha_i=0.9, conf_int_i=FALSE, ci_alpha_i=0.3, make_standardized_plot_filename...=list(), start_callback=NULL, sign = TRUE, sign_callback = rlang::expr(text(graphics::par("usr")[2], graphics::par("usr")[3], labels = "@priscian", adj = c(1.0, -0.5))), end_callback=NULL, save_png=FALSE, save_png_dir, png...=list(), ...) # Was 'smooth_span = 0.4'
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

    allNames <- c(get_climate_series_names(instrumental, conf_int=conf_int_i, invert=FALSE), series)
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

  ## In case the "scenario" attribute isn't a factor:
  attr(models, "scenario") <- as.factor(attr(models, "scenario"))

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
    wi[, climateSeriesNames] <- MA(wi[, climateSeriesNames, drop=FALSE], ma_i, sides=ma_sides)
  }
  wm[, get_climate_series_names(wm)] <- MA(wm[, get_climate_series_names(wm), drop=FALSE], ma, sides=ma_sides)

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
  wiz <- zoo::as.zoo(wi)
  wmz <- zoo::as.zoo(wm)

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
  make_standardized_plot_filenameArgs <- utils::modifyList(make_standardized_plot_filenameArgs, make_standardized_plot_filename..., keep.null = TRUE)
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
    pngArgs <- utils::modifyList(pngArgs, png..., keep.null = TRUE)
    do.call("png", pngArgs)
  }

  ## N.B. My custom x-axis ticks are causing some problems, so let's just ditch them for now (24 Sep 2020).
  xaxt <- "n"
  #xaxt <- "s"
  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width=12.5, height=7.3) # New default device of 1200 × 700 px at 96 DPI.
  op <- par(mar = c(5, 5, 4, 2) + 0.1)
  plot(wiz[, get_climate_series_names(wiz, conf_int = FALSE), drop = FALSE], screens = 1L, bty = "n", xaxs = "r", xaxt = xaxt, yaxt = "n", xlab = xlab, ylab = ylab, main = main, type = "n", ylim = ylim, ...) # I.e. 'plot.zoo()'.

  if (!is.null(bg)) {
    graphics::rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], border = NA, col = bg)
  }

  ## V. https://stackoverflow.com/questions/22470114/removing-top-and-right-borders-from-boxplot-frame-in-r/62185904#62185904
  #graphics::box(bty = "l") # L-shaped box
  graphics::axis(1, lwd = 0, lwd.ticks = 0) # Draw x-axis
  graphics::axis(2, lwd = 0, lwd.ticks = 0) # Draw y-axis

  if (xaxt == "n")
    axis(1, xaxisTicks, lty = 0)
  else
    xaxisTicks <- axTicks(1)
  if (maText != "") mtext(maText, 3L)

  ## Create different colors for each scenario.
  modelColors <- col_m
  if (is.null(modelColors))
    #modelColors <- gray.colors(length(scenario), 0.90, 0.85)
    modelColors <- gray.colors(length(scenario), 0.85, 0.80) # 25 Apr 2022
  if (length(modelColors) == 1L && length(scenario) > 1L)
    modelColors <- vary_brightness(modelColors, length(scenario))
  if (length(modelColors) < length(scenario))
    modelColors <- rep(modelColors, length.out=length(scenario))
  eachModelColor <- as.vector(unlist(tapply(as.numeric(attr(models, "scenario")), attr(models, "scenario"), function (a) modelColors[a])))
  #par(new = TRUE); plot(wmz[, get_climate_series_names(wmz), drop=FALSE], screens=1, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", col=eachModelColor, ...) # I.e. 'plot.zoo()'.
  a_ply(wmz[, get_climate_series_names(wmz), drop = FALSE], 2, function(a) { lines(a, col = eachModelColor) }) # I.e. 'lines.zoo()'.

  grd_lty <- "solid" # Was "dotted"
  grid(nx=NA, ny=NULL, col="lightgray", lty=grd_lty, lwd=par("lwd"))
  abline(v=xaxisTicks, col="lightgray", lty=grd_lty, lwd=par("lwd"))

  if (!is.null(start_callback))
    poly_eval(start_callback)

  ## Plot model averages.
  year <- as.numeric(attr(wmz, "index"))
  modelsMiddle <- by(t(wmz[, get_climate_series_names(wmz), drop=FALSE]), attr(models, "scenario"), function(m) { apply(t(m), 1L, function(x) { rv <- NA; if (!all(is.na(x))) rv <- do.call(center_fun, list(x=x, na.rm=TRUE)); return (rv) }) })
  if (smooth_center)
    modelsMiddle <- sapply(modelsMiddle, function(m) predict(LOESS(m ~ year, span = NULL), data.frame(year=year)), simplify=FALSE)
  meanColor <- col_m_mean
  if (is.null(meanColor))
    #meanColor <- gray(0.6)
    meanColor <- grDevices::gray(0.4)
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
    lowerEnvelope <- sapply(lowerEnvelope, function(e) predict(LOESS(e ~ year, span = smooth_span), data.frame(year=year)), simplify=FALSE)
    upperEnvelope <- sapply(upperEnvelope, function(e) predict(LOESS(e ~ year, span = smooth_span), data.frame(year=year)), simplify=FALSE)
  }

  ## Plot the envelope.
  if (plot_envelope) {
    mapply(function(le, color) points(year, le, type="l", col=color, lwd=1, lty=3L), lowerEnvelope, meanColor)
    mapply(function(ue, color) points(year, ue, type="l", col=color, lwd=1, lty=3L), upperEnvelope, meanColor)

    if (!is.null(alpha_envelope)) {
      mapply(function(le, ue, color) {
        cidf <- data.frame(yr_part=wmz[, "yr_part"], lower=le, upper=ue); cidf <- cidf[complete.cases(cidf), ]
        ciCol <- scales::alpha(color, alpha_envelope)
        polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
      }, lowerEnvelope, upperEnvelope, meanColor)
    }
  }

  ## Plot instrumental series.
  if (plotInstrumental) {
    col_i_funArgs <- list(
      n = length(series)
    )
    col_i_funArgs <- utils::modifyList(col_i_funArgs, col_i_fun..., keep.null = TRUE)
    instrumentalColors <- suppressWarnings(scales::alpha(do.call(col_i_fun, col_i_funArgs), alpha_i))
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
    plot_iArgs <- utils::modifyList(plot_iArgs, plot_i..., keep.null = TRUE)

    cis <- i[, grepl("_uncertainty$", colnames(i)), drop=FALSE]
    if (conf_int_i) { # Plot confidence bands for temp series that have them.
      confintNames <- intersect(series %_% "_uncertainty", colnames(cis))
        if (length(confintNames) != 0) {
        seriesNames <- stringr::str_match(confintNames, "^(.*?)_uncertainty$")[, 2L]
        for (j in seq_along(confintNames)) {
          value <- i[, seriesNames[j]]
          ci <- cis[, confintNames[j]]
          upper <- value + ci/2; lower <- value - ci/2
          ciCol <- scales::alpha(instrumentalColors[seriesNames[j]], ci_alpha_i)
          cidf <- data.frame(yr_part=wiz[, "yr_part"], lower=lower, upper=upper); cidf <- cidf[complete.cases(cidf), ]
          polygon(x=c(cidf$yr_part, rev(cidf$yr_part)), y=c(cidf$upper, rev(cidf$lower)), col=ciCol, border=NA)
        }
      }
    }

    par(new=TRUE); do.call("plot", plot_iArgs) # I.e. "plot.zoo".
    #a_ply(wiz[, get_climate_series_names(wiz, conf_int = FALSE), drop = FALSE], 2, function(a) { lines(a, col = plot_iArgs$col, lwd = plot_iArgs$lwd) }) # I.e. 'lines.zoo()'.
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
    #legendColors <- c(legendColors, instrumentalColors[seq_along(series)])
    legendColors <- c(legendColors, plot_iArgs$col)
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
  legendArgs <- utils::modifyList(legendArgs, legend..., keep.null = TRUE)
  do.call("legend", legendArgs)

  if (!is.null(end_callback))
    poly_eval(end_callback)

  if (sign)
    poly_eval(sign_callback)

  if (save_png)
    dev.off()

  par(op)

  cat("Standardized file name:", stringr::str_replace_all(filename, "%%", "%"), fill=TRUE); flush.console()

  return (nop())
}
