#' @export
correlate_co2_temperature <- function(series, start_year=1880, end_year=current_year - 1, data, ylab, main_base="Temperature", text_x=380, text_y=-0.4, baseline=FALSE, download=FALSE){
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


## Based on the technique described at https://tamino.wordpress.com/2012/01/08/trend-and-cycle-together/.
#' @export
remove_periodic_cycle <- function(
  inst,
  series,
  center = TRUE,
  period = 1,
  num_harmonics = 4,
  loess... = list(),
  unwrap = TRUE,
  keep_series = TRUE,
  keep_interpolated = FALSE,
  keep_loess = FALSE,
  suffix = " (anomalies)",
  is_unc = FALSE, unc_suffix = "_uncertainty", fit_unc = FALSE,
  ...
){
  uncertaintyDf <- NULL

  if (!is_unc && !is_invalid(inst[[series %_% unc_suffix]])) {
    ## Get all arguments of this function to pass on for recursion.
    recursiveArgs <- get_all_args()
    recursiveArgs$center <- FALSE
    recursiveArgs$is_unc <- TRUE

    uncertaintyDf <- do.call(remove_periodic_cycle, recursiveArgs)
  }

  d <- inst[, c(intersect(common_columns, names(inst)), series %_% ifelse(!is_unc, "", unc_suffix))]
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series %_% ifelse(!is_unc, "", unc_suffix)]))
  if (is_unc && !fit_unc) {
    d[[series %_% suffix %_% unc_suffix]] <- d[[series %_% unc_suffix]]

    return (d)
  }

  d[[series %_% " (interpolated)" %_% ifelse(!is_unc, "", unc_suffix)]] <-
    drop(interpNA(d[, series %_% ifelse(!is_unc, "", unc_suffix)], "fmm"))

  if (is.null(period)) { # Estimate period from data.
    spectralDensity <- spectrum(y)
    period <- 1 / spectralDensity$freq[spectralDensity$spec == max(spectralDensity$spec)]
  }

  ## Get residuals from LOESS fit.
  loessArgs = list(
    formula = eval(substitute(
s ~ yr_part,
      list(s = as.name(series %_% " (interpolated)" %_% ifelse(!is_unc, "", unc_suffix)))
)),
    data = d,
    span = 0.2,
    na.action = na.exclude
  )
  loessArgs <- utils::modifyList(loessArgs, loess..., keep.null = TRUE)

  #l <- do.call(stats::loess, loessArgs)
  l <- do.call(LOESS, loessArgs)
  if (keep_loess)
    d[[series %_% " (LOESS fit)" %_% ifelse(!is_unc, "", unc_suffix)]] <- predict(l)
  r <- residuals(l)

  ## Construct model formula for given no. of harmonics.
  fBase <- "r ~ "; f <- NULL
  for (i in seq(num_harmonics))
    f <- c(f, paste0(c("sin", "cos"), paste0("(", 2 * i, " * pi / period * yr_part)")))
  f <- as.formula(paste0(fBase, paste0(f, collapse = " + ")))

  rfit <- lm(f, data = d, na.action = na.exclude, ...)
  uncycled <- d[[series %_% ifelse(!is_unc, "", unc_suffix)]] - predict(rfit)

  if (is.logical(center))
    d[[series %_% suffix %_% ifelse(!is_unc, "", unc_suffix)]] <-
      scale(uncycled, center = center, scale = FALSE)[, 1]
  else
    d[[series %_% suffix %_% ifelse(!is_unc, "", unc_suffix)]] <-
      uncycled - mean(uncycled[d$year %in% center], na.rm = TRUE)

  if (!keep_series) {
    d[[series %_% ifelse(!is_unc, "", unc_suffix)]] <- NULL
    if (!is.null(uncertaintyDf))
      uncertaintyDf[[series %_% unc_suffix]] <- NULL
  }

  if (!keep_interpolated)
    d[[series %_% " (interpolated)" %_% ifelse(!is_unc, "", unc_suffix)]] <- NULL

  if (!is.null(uncertaintyDf))
    d <- merge(d, uncertaintyDf[c("yr_part", get_climate_series_names(uncertaintyDf,
      conf_int = TRUE
))], all.x = TRUE, by = "yr_part", sort = TRUE)

  d %>% as.data.frame
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


## Create temperature series with the influence of some exogenous factors removed.
## Based on Foster & Rahmstorf 2011, dx.doi.org/10.1088/1748-9326/6/4/044022.
#' @export
remove_exogenous_influences <- function(
x, series,
  start = NULL, end = NULL,
  lags = list(`MEI Aggregate Global` = NULL, `SAOD Aggregate Global` = NULL, `TSI Aggregate Global` = NULL),
  aggregate_vars_fun = add_default_aggregate_variables,
  period = 1, num_harmonics = 4,
  max_lag = 12, bs_df = NULL, bs_degree = 3,
  center_on_mean = TRUE,
  suffix = " (adj.)"
){
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)

  if (!is.null(aggregate_vars_fun))
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
    l <- expand.grid(sapply(lags, function(a) {
 r <- seq(0, max_lag); if (!is.null(a)) r <- a; r
 }, simplify = FALSE))
    aic <- apply(
l, 1,
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
    if (center_on_mean)
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


## Fit segmented linear models to selected climate data.
#' @export
fit_segmented_model <- function(
  x,
  series,
  col = suppressWarnings(RColorBrewer::brewer.pal(length(series),"Paired")),
  start = NULL, end = NULL,
  yearly = TRUE,
  breakpoints... = list(),
  segmented... = list(), seg.control... = list(seed = 100),
  make_yearly_data... = list(),
  ...
){
  r <- list(data = x, series = series)
  r$range <- list(start = start, end = end)
  r$col <- col
  length(r$col) <- length(r$series); names(r$col) <- r$series

  if (!yearly) {
    g <- r$data
  } else {
    make_yearly_dataArgs <- list(
      x = r$data
    )
    make_yearly_dataArgs <- utils::modifyList(make_yearly_dataArgs, make_yearly_data..., keep.null = TRUE)
    g <- as.data.frame(do.call("make_yearly_data", make_yearly_dataArgs))
    if (!is.null(start)) start <- trunc(start)
    if (!is.null(end)) end <- trunc(end)
  }

  yearVar <- ifelse(yearly, "year", "yr_part")

  r$piecewise <- list()
  for (i in r$series) {
    r$piecewise[[i]] <- list()
    r$piecewise[[i]]$col <- r$piecewise$col[i]

    #h <- oss(g, i)[na_unwrap(g[[i]]), , drop = FALSE]
    h <- oss(g, i)[na_unwrap(g[, i]), , drop = FALSE] %>% as.data.frame
    h <- h[h[[yearVar]] >= ifelse(!is.null(start), start, -Inf) & h[[yearVar]] <= ifelse(!is.null(end), end, Inf), ]

    breakpointsArgs <- list(
      formula = eval(substitute(Y ~ X, list(X = as.name(yearVar), Y = as.name(i)))),
      data = h,
      breaks = NULL
    )
    breakpointsArgs <- utils::modifyList(breakpointsArgs, breakpoints..., keep.null = TRUE)
    r$piecewise[[i]]$bp <- do.call(strucchange::breakpoints, breakpointsArgs)

    r$piecewise[[i]]$breaks <- r$piecewise[[i]]$bp$X[, yearVar][r$piecewise[[i]]$bp$breakpoint]

    seg.controlArgs <- list(
      #stop.if.error = TRUE,
      fix.npsi = TRUE,
      K = length(r$piecewise[[i]]$breaks),
      n.boot = 250,
      random = FALSE,
      h = 0.3
    )
    seg.controlArgs <- utils::modifyList(seg.controlArgs, seg.control..., keep.null = TRUE)
    segControl <- do.call(segmented::seg.control, seg.controlArgs)

    r$piecewise[[i]]$lm <- lm(breakpointsArgs$formula, data = h, x = TRUE, y = TRUE)

    segmentedArgs <- list(
      obj = r$piecewise[[i]]$lm,
      seg.Z = as.formula(paste("~", yearVar)),
      psi = r$piecewise[[i]]$breaks,
      control = segControl
    )
    segmentedArgs <- utils::modifyList(segmentedArgs, segmented..., keep.null = TRUE)
    #r$piecewise[[i]]$sm <- do.call("segmented", segmentedArgs)

    run_segmented <- function()    {
      mf <- model.frame(r$piecewise[[i]]$lm)

      while (TRUE) {
        withRestarts(
{
          sm <- do.call(segmented::segmented, segmentedArgs)
          break
        },
          restart = function() {
            ## Which breakpoint is closest to the start or end of the time series?
            if (length(segmentedArgs$psi) > 1L)
              segmentedArgs$psi <<- segmentedArgs$psi[-which.min(pmin(segmentedArgs$psi, NROW(mf) - segmentedArgs$psi + 1))]
          }
)
      }

      sm
    }

    tryCatch(
{
      withCallingHandlers(
{
          sm <- run_segmented()
        },
          error = function(e) {
            message("Error: ", e$message)
            if (any(grepl("one coef is NA: breakpoint(s) at the boundary", e$message, fixed = TRUE)))
              invokeRestart("restart")
          }
      )

      r$piecewise[[i]]$sm <- sm
    },
 error = function(e) {
 message("Warning: No breakpoint(s) found")
 }
)
  }

  r
}


## Based on Foster & Rahmstorf 2011, dx.doi.org/10.1088/1748-9326/6/4/044022
## V. http://www.ysbl.york.ac.uk/~cowtan/applets/trend/trend.js
##   & http://www.ysbl.york.ac.uk/~cowtan/applets/trend/trendapp.js
#' @export
correct_monthly_autocorrelation <- function(
  xdata,
  ydata,
  model,
  autocorrel_period = c(1980, 2010),
  slope_coef = "yr_part",
  remove_missings = TRUE,
  santer = FALSE
){
  ## Covariance with lag
  autocovariance <- function(data, j, remove_missings)  {
    if (remove_missings)
      data <- data[!is.na(data)]

    n <- length(data); sx <- 0.0; cx <- 0.0
    for (i in seq(n))
      sx <- sx + data[i]
    sx <- sx / n
    for (i in seq(n - j))
      cx <- cx + (data[i] - sx) * (data[i + j] - sx)

    return (cx / n)
  }

  ## Degrees of freedom correction
  data_per_degree_of_freedom <- function(xdata, ydata, autocorrel_period)  {
    xy <- dataframe(xdata = xdata, ydata = ydata)
    xyac <- xy %>%
      dplyr::filter(xdata >= min(autocorrel_period) & xdata <= max(autocorrel_period))
    if (is_invalid(xyac$ydata))
      xyac <- xy
    mod_ac <- lm(ydata ~ xdata, data = xyac)
    ## Redefine 'xdata' & 'ydata'
    xdata <- xyac$xdata; ydata <- xyac$ydata
    for (i in seq(length(xdata)))
      ydata[i] <- ydata[i] - coefficients(mod_ac)["xdata"] * xdata[i]
    cov_ <- autocovariance(ydata, 0, remove_missings = remove_missings)
    rho1 <- autocovariance(ydata, 1, remove_missings = remove_missings) / cov_
    rho2 <- autocovariance(ydata, 2, remove_missings = remove_missings) / cov_

    ## This sometimes returns negative values; investigate why that's so sometime.
    return (1.0 + (2.0 * rho1) / (1.0 - rho2/rho1))
  }

  ## Santer &al 2000 correction dx.doi.org/10.1029/1999JD901105
  santer_correct <- function(xdata, ydata, model, slope_coef)  {
    if (missing(xdata)) {
      # xdata = model$x[, slope_coef]
      # ydata = model$y

      index_df <- residuals(model) %>%
 {
 dataframe(index = as.numeric(names(.)))
 }
      flit <- model$x[, slope_coef]
      if (is_invalid(names(flit))) {
        flit <- model$x[, slope_coef, drop = FALSE] %>%
          {
 structure(.[, 1], .Names = dimnames(.)[1])
 }
      }
      xdata <- dplyr::full_join(flit %>%
        {
 dataframe(index = as.numeric(names(.)), x = .)
 }, index_df) %>%
 dplyr::arrange(index) %>%
        {
 structure(dplyr::pull(., x), .Names = .$index)
 } %>%
 interpNA() %>%
 `[`(, 1)
      ydata <- dplyr::full_join(model$y %>%
        {
 dataframe(index = as.numeric(names(.)), y = .)
 }, index_df) %>%
 dplyr::arrange(index) %>%
        {
 structure(dplyr::pull(., y), .Names = .$index)
 }
    }
    temp_corr_matrix <- structure(cbind(
      ydata,
      keystone::shift(ydata, -1L, roll = FALSE)
    ), .Dimnames = list(as.character(xdata), c("x_t", "x_t-1"))) %>% as.data.frame
    temp_corr_matrix_resid <- structure(cbind(
      stats::residuals(model),
      keystone::shift(stats::residuals(model), 1L, roll = FALSE)
    ), .Dimnames = list(as.character(xdata), c("x_t", "x_t-1"))) %>% as.data.frame
    N <- stats::df.residual(model)
    if (!(temp_corr_matrix_resid %>% complete.cases %>% any))
      return (structure(NA, N_eff = N))
    r <- stats::cor(temp_corr_matrix_resid$x_t, temp_corr_matrix_resid$`x_t-1`, use = "complete.obs")
    N_eff <- N * ((1 - r)/(1 + r))
    #`s_e^2` <- (1/(N_eff - 2)) * sum(stats::residuals(model)^2, na.rm = TRUE)
    `s_e^2` <- function(N) (1/N) * sum(stats::residuals(model)^2, na.rm = TRUE)
    #`s_b^2` <- `s_e^2`(N_eff - 2)/sum((xdata - mean(xdata, na.rm = TRUE))^2, na.rm = TRUE)
    `s_b^2` <- function(N) `s_e^2`(N)/sum((xdata - mean(xdata, na.rm = TRUE))^2, na.rm = TRUE)
    t_b <- stats::coef(model)[slope_coef]/sqrt(`s_b^2`(N_eff - 2)) %>% as.vector
    #t_b0 <- stats::coef(model)[slope_coef]/sqrt(`s_b^2`(N)) %>% as.vector
    ## Multiplicative constant for 'correct_stats()' to work is (t_b0/`t_b0_N-2`)^2

    ## This is a 'nu' value that makes 'tval_corrected' in 'correct_stats()' equal to 't_b' here:
    structure((`s_b^2`(N - 2)/`s_b^2`(N)) * (N - 2)/(N_eff - 2), N_eff = N_eff - 2)
  }

  ## Correct t-stat & p-value
  correct_stats <- function(model, nu, slope_coef)  {
    Qr <- stats:::qr.lm(model)
    p <- model$rank
    p1 <- 1L:p
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    r <- model$residuals
    rss <- sum(r^2)
    rdf <- model$df.residual
    resvar <- rss/rdf
    se <- sqrt(diag(R) * resvar)
    est <- model$coefficients[Qr$pivot[p1]]
    tval <- est/se
    sigma_w <- se[names(est) == slope_coef]
    N_eff <- attr(nu, "N_eff"); nu <- nu %>% as.vector
    sigma_c <- sigma_w * sqrt(nu)
    tval_corrected <- est[slope_coef]/(sigma_c)
    `Pr(>|t|)` <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE) # Two-tailed
    `Pr(>|t|) corrected` <-
      2 * stats::pt(abs(tval_corrected), ifelse(is.null(N_eff), rdf, N_eff), lower.tail = FALSE) # Two-tailed

    list(
      sigma_w = sigma_w,
      tval = tval[slope_coef], pval = `Pr(>|t|)`[slope_coef],
      sigma_c = sigma_c,
      tval_corrected = tval_corrected, pval_corrected = `Pr(>|t|) corrected`,
      nu = nu
    )
  }

  if (!santer)
    nu <- data_per_degree_of_freedom(xdata, ydata, autocorrel_period)
  else {
    if (is_invalid(model$x))
      nu <- santer_correct(xdata, ydata, model, slope_coef)
    else
      nu <- santer_correct(model = model, slope_coef = slope_coef)
  }

  if (!santer && (is_invalid(nu) || nu <= 0.0)) {
    if (is_invalid(model$x))
      nu <- santer_correct(xdata, ydata, model, slope_coef)
    else
      nu <- santer_correct(model = model, slope_coef = slope_coef)
    warning("'nu' value is invalid; Santer &al 2000 correction made for autocorrelation")
  }
  if (is_invalid(nu) || nu <= 0.0) {
    nu <- 1
    warning("'nu' value is invalid; no correction made for autocorrelation")
  }

  cs <- correct_stats(model, nu, slope_coef)

  c(
    cs,
    decadal_slope = (10 * coefficients(model)[slope_coef]) %>% as.vector,
    decadal_2sigma = 2 * 10 * cs$sigma_c,
    use.names = TRUE
  )
}

## usage:
# d <- get_climate_data(download = FALSE, baseline = TRUE)
# series <- "GISTEMP v4 Global"
# r <- plot_climate_data(d, series, 1970, 2020.99, trend = TRUE, save_png = FALSE)
# lmod <- r$trend[[series]]$lm
# correct_monthly_autocorrelation(lmod$x[, "yr_part"], lmod$y, lmod)


## Simulated temperature series generator
#' @export
simulate_temp_series <- function(
  mean_temperature = 15.0, # Mean temperature/climatology in °C
  amplitude = 10.0, # Amplitude of seasonal variation (°C)
  trend_rate = 0.2, # Long-term warming trend (°C/decade)
  start_year = 1970,
  total_years = current_year - start_year,
  months_per_year = 12,
  noise_sd = 0.3, # Try to match global land+SSTs
  var_name = "simulated temperature",
  seed = 666 # If NULL, not reproducible
){
  total_months <- (total_years + 1) * months_per_year

  ## Generate time values
  yr_part <- sapply(start_year:(start_year + total_years), `+`,
    e1 = (1:months_per_year - 0.5) / months_per_year
) %>% as.vector
  times <- seq(total_months)

  ## Generate seasonal pattern
  seasonal_pattern <- amplitude * sin(2 * pi * times / months_per_year)

  ## Generate long-term trend
  trend <- (trend_rate / 10 * times) / months_per_year

  ## Generate random noise
  if (!is.null(seed))
    set.seed(seed)
  noise <- stats::rnorm(n = total_months, mean = 0, sd = noise_sd)

  ## Combine components to simulate temperature data
  simulated_temperature <- mean_temperature + seasonal_pattern + trend + noise

  d <- dataframe(month = 1:months_per_year, yr_part = yr_part) %>%
    dplyr::mutate(year = trunc(yr_part), .before = "month") %>%
    dplyr::mutate(!!var_name := simulated_temperature)

  d
}

## usage:
# r <- plot_climate_data(simulate_temp_series(), "simulated temperature", yearly = TRUE, baseline = TRUE, trend = TRUE)
