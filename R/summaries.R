#' @export
make_yearly_data <- function(x, na_rm = TRUE, unwrap = TRUE, baseline = FALSE, incomplete_years_to_na = FALSE){
  if (missing(x))
    x <- get_climate_data(download = FALSE)

  if (incomplete_years_to_na) {
    series <- get_climate_series_names(x, conf_int = TRUE)
    yearTab <- table(x[, "year"])
    incompleteYears <- as.numeric(names(yearTab)[yearTab != 12])

    ## For incomplete years, make all elements NA.
    dev_null <- sapply(series, function(a) {
 is.na(x[, a]) <<- x[, "year"] %in% incompleteYears; nop()
 }); rm(dev_null)
  }

  ## This doesn't account for the "_uncertainty" columns, though, whose squares should be averaged then 'sqrt()'ed.
  #r0 <- data.table::data.table(x)[, lapply(.SD, function(a) { r <- NA_real_; if (!all(is.na(a))) r <- mean(a, na.rm=na_rm); r }), .SDcols = -common_columns[common_columns %nin% "year"], by = year]

  ## Use a better mean estimate for the "_uncertainty" columns.
  ## V. stats.stackexchange.com/questions/25848/how-to-sum-a-standard-deviation/26647#26647
  cnames <- get_climate_series_names(x, conf_int = TRUE)
  l <- list(cnames[stringr::str_ends(cnames, "_uncertainty", negate = TRUE)], cnames[stringr::str_ends(cnames, "_uncertainty", negate = FALSE)])
  r <- list(
.vars = tibble::lst(!!l[[1]], !!l[[2]]),
      .funs = tibble::lst(
        function(a) {
 r <- NA_real_; if (!all(is.na(a))) r <- mean(a, na.rm = na_rm); r
 },
        function(a) {
 r <- NA_real_; if (!all(is.na(a))) r <- sqrt(mean(a^2, na.rm = na_rm)); r
 }
      )
) %>%
    ## For applying multiple functions to different columns in 'summarize_at()', see:
    ## https://stackoverflow.com/questions/41109403/r-dplyr-summarise-multiple-functions-to-selected-variables/53981812#53981812
    purrr::pmap(~ x %>%
 as.data.frame %>%
 dplyr::group_by(year) %>%
 dplyr::summarize_at(.x, .y)) %>%
    purrr::reduce(dplyr::inner_join, by = "year")

  if (unwrap)
    r <- r[na_unwrap(r), ]

  r <- recenter_anomalies(as.data.frame(r), baseline = baseline, by_month = FALSE)

  r
}

## usage:
## Reproduce a plot here: https://tamino.wordpress.com/2017/01/01/tony-hellers-snow-job/.
# g <- make_yearly_data(na_rm = FALSE) # Allow NA values for 'mean()'; possibly better for very seasonally sensitive series.
# series <- "Rutgers NH Snow Cover"
# h <- eval(substitute(g[na_unwrap(SERIES)][year >= min(year) & !is.na(SERIES)], list(SERIES = as.name(series))))
# plot(h$year, h[[series]]/1e6, lwd = 2, pch = 19, type = "o")


#' @export
show_warmest_years <- function(
  x,
  series,
  num_top_years = 10,
  start_year = NULL, end_year = current_year - 1,
  baseline = FALSE,
  simplify = TRUE # TRUE to include the actual anomaly values
){
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = baseline)

  if (is.null(start_year)) start_year <- min(x$year, na.rm = TRUE)
  xx <- x %>% dplyr::filter(year >= start_year & year <= end_year)

  y <- make_yearly_data(oss(x, series))

  l <- sapply(y[, -1, drop = FALSE],
    function(x)    {
      r <- dplyr::arrange(dataframe(year = y$year, temp = x), desc(temp))[seq(num_top_years), ]

      if (simplify) r$year else r
    },
 simplify = simplify
)

  l
}

## usage:
# series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global", "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5", "UAH TLT 6.0 Global", "JRA-55 Surface Air Global", "ERA5 Surface Air Global", "NCEP/NCAR R1 Surface Air Global")
# show_warmest_years(series = series)


#' @export
get_yearly_difference <- function(
  series,
  start, end = current_year - 1,
  data,
  digits = 3,
  unit = "\u00b0C",
  loess = FALSE,
  plot_baseline = TRUE,
  save_png = FALSE,
  ...
){
  if (missing(data))
    data <- get_climate_data(download = FALSE, baseline = FALSE)

  # if (loess) data <- add_loess_variables(data, series, ...) # Ends up being a poor fit for yearly data
  g <- make_yearly_data(data)
  if (loess)
    g <- add_loess_variables(g, series)
  h <- g[c(which(g$year == start), which(g$year == end)), series %_% ifelse(loess, " (LOESS fit)", ""), drop = FALSE] %>%
    `rownames<-`(c(start, end))

  plot_climate_data(g, series %>% unique, start, end,
 yearly = FALSE, baseline = plot_baseline, lwd = 2, conf_int = FALSE,
    make_standardized_plot_filename... = list(suffix = ""), loess = loess, save_png = save_png, ...
)

  ## N.B. Use e.g. stringi::stri_escape_unicode("°") to get Unicode value(s) easily.
  cat("Difference in ", unit ," from ", start, "\u2013", end, sep = "", fill = TRUE)
  print(t(h[2, ] - h[1, ]) %>% `colnames<-`("diff"), digits = digits, row.names = FALSE)
  cat(fill = TRUE)
  cat("Decadal rate in ", unit ,"/dec. from ", start, "\u2013", end, sep = "", fill = TRUE)
  print((10 * t(h[2, ] - h[1, ]) / (end - start)) %>% `colnames<-`("rate"), digits = digits, row.names = FALSE)

  attr(h, "range") <- c(start = start, end = end)

  #browser()
  return (h)
}

## usage:
# series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT5 Global", "BEST Global (Air Ice Temp.)", "JMA Global")
# series <- c("NCEI Global", "BEST Global (Air Ice Temp.)", "HadCRUT5 Global", "JRA-3Q Surface Air Global")
# ytd <- get_yearly_difference(series, 1880)
# ytd <- get_yearly_difference(series, 1880, loess = TRUE)
# ytd <- get_yearly_difference(series, 1880, loess = TRUE, loess... = list(span = 0.4))
# ytd <- get_yearly_difference(series, 1970)


## Basically a "show hottest year" function, but slightly configurable.
#' @export
show_single_value <- function
(
  series,
  baseline = TRUE,
  data,
  fun = which.max,
  value_name = "temp anom. (\u00b0C)",
  format = "%.3f",
  this_year = current_year,
  ...
){
  if (missing(data))
    data <- get_climate_data(download = FALSE, baseline = baseline)

  ## N.B. Data must have complete year-month pairs for this to be accurate!
  ## This doesn't work correctly, so check:
  complete <- data %>%
 dplyr::select(!!series) %>%
    dplyr::group_by(data$year) %>%
    dplyr::group_map(
      function(x, y)      {
        x %>%
 dplyr::mutate_all(function(m) !is.na(m)) %>%
          dplyr::summarize_all(all) %>%
          dplyr::bind_cols(y, .)
      }
) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::rename(year = 1)

  baseline <- attr(data, "baseline")
  g <- make_yearly_data(data)[, c("year", series)]

  single <- sapply(series,
    function(a)    {
      m <- fun(g[[a]], ...)
      r <- data.frame(year = g$year[m], check.names = FALSE)
      r[[value_name]] <- g[[a]][m]
      r[["complete?"]] <- c("no", "yes")[complete[[a]][m] + 1]

      r
    },
 simplify = FALSE
) %>%
    purrr::reduce(dplyr::bind_rows)
  rownames(single) <- series
  single[["last complete"]] <- sapply(complete[, -1], function(a) {
 complete$year[a %>%
 which %>%
 max]
 })

  this_year_rank <- sapply(
g[, -1],
    function(a) {
      o <- order(a, decreasing = TRUE)
      rank_map <- structure(seq(NROW(g)) %>% `is.na<-`(is.na(a[o])), .Names = g$year[o])

      rank_map[this_year %>% as.character] %>% as.vector
    }
)
  single[[paste(this_year, "rank")]] <- this_year_rank

  print(single %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(!!value_name := sprintf(format, .[[value_name]])) %>%
    tibble::column_to_rownames()  )
  if (!is.null(baseline))
    cat("\nBaseline: ", min(baseline), "\u2014", max(baseline), fill = TRUE, sep = "")

  attr(single, "baseline") <- baseline

  single
}

## usage:
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global", "BEST Global (Water Ice Temp.)", "JMA Global", "RSS TLT 3.3 -70.0/82.5", "UAH TLT 6.0 Global")
# hottest <- show_single_value(series)


#' @export
nearest_year_month_from_numeric <- function(yr_part, x, nearest_type = c("nearest", "above", "below"), as_data_frame = FALSE){
  nearest_type <- match.arg(nearest_type)

  if (missing(yr_part)) {
    flit <- rev(expand.grid(month = 1:12, year = trunc(x), by = 1))
    flit$yr_part <- flit$year + (2 * flit$month - 1)/24
  } else {
    r <- range(yr_part)
    x <- x[1]
    flit <- rev(expand.grid(month = 1:12, year = seq(floor(r[1]), floor(r[2]), by = 1)))
    flit$yr_part <- flit$year + (2 * flit$month - 1)/24

    ## Allow fuzzy equality of the start- & endpoints (sometimes necessary).
    isEqualStart <- is_equal(flit$yr_part, r[1])
    isEqualEnd <- is_equal(flit$yr_part, r[2])
    flit <- flit[(isEqualStart | flit$yr_part > r[1]) & (flit$yr_part < r[2] | isEqualEnd), ]
  }

  isEqual <- is_equal(flit$yr_part, x)
  egrid <- switch(nearest_type,
    `above` = flit[isEqual | flit$yr_part > x, ],
    `below` = flit[flit$yr_part < x | isEqual, ],
    flit
  )

  r <- egrid[nearest(egrid$yr_part, x), c("year", "month")]
  if (!as_data_frame)
    r <- unlist(r)

  r
}


