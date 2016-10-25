#' @export
correlate_co2_temperature <- function(series, start_year=1880, end_year=current_year - 1, text_x=380, text_y=-0.4, baseline=TRUE, download=FALSE)
{
  d <- get_climate_data(download=download, baseline=baseline)
  e <- get_climate_data(download=download, baseline=FALSE)

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
  ylab <- eval(substitute(expression(paste(series, " Temp. Anomaly (", phantom(l) * degree, "C) w.r.t. ", b, sep="")), list(b="1981" %_% "\u2013" %_% "2010", series=as.symbol(series))))
  main <- eval(substitute(expression(paste("Temperature vs. CO", phantom()[2], " (", startYear, "\u2013", endYear, ")", sep="")), list(endYear=as.symbol(end_year), startYear=as.symbol(start_year))))

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
  library(directlabels)

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
    geom_dl(aes(label=year), method = list(dl.trans(x=x + 0.2), "last.points", cex = 0.8)) +
    # coord_cartesian(ylim(c(-4, 4)) + # No clipping.
    labs(list(title="Year-to-Date Temperature Anomalies", subtitle=subtitle, y=ylab))

  print(g)

  return(d4)
}

## usage:
# plot_horse_race("GISTEMP Global")
# plot_horse_race("NCEI US Avg. Temp.", 10) # Use -10 for bottom 10 years.


#' @export
get_old_yearly_gistemp <- function(series="GISTEMP Global Land 1998", uri="http://web.archive.org/web/19990220235952/http://www.giss.nasa.gov/data/gistemp/GLB.Ts.txt")
{
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  x <- NULL

  gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

  ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
  ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
  curl <- getCurlHandle()
  curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
  tryCatch({
    r <- getURL(uri, curl=curl)
    tab <- gsub("^(?!\\d{4}\\s+).*$", "", strsplit(r, '\n')[[1L]], perl=TRUE)
    tab <- tab[tab != ""]
    x <- read.table(text=tab, header=FALSE, as.is=TRUE, na.strings=c("***", "****"), skip=0L, check.names=FALSE)
  }, error=Error, warning=Error)

  flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
  for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
  flit <- dplyr::arrange(flit, V1, month)

  d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
  #d$temp <- gissGlobalMean + (d$temp / 100) # Don't need to do this.
  #d$temp <- gissGlobalMean + round(rep(runif(12), length.out=length(d$temp)), 3) + (d$temp / 100) # For testing only.
  d$temp <- d$temp / 100

  names(d)[names(d) == "temp"] <- series

  return (d)
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=FALSE)
# g <- get_old_yearly_gistemp()
# d <- recenter_anomalies(merge(inst, g, all=TRUE), 1951:1980) # Should be the same baseline, but make sure.
# series <- c("GISTEMP Global Land", "GISTEMP Global Land 1998")
# plot_climate_data(d, series=series, ma=12, lwd=2, conf_int=FALSE, show_trend=TRUE)


#' @export
get_old_monthly_gistemp <- function(series="GISTEMP Global Nov. 2015", uri="http://web.archive.org/web/20151218065405/http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt", skip=7L, end_year=2015)
{
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  x <- NULL

  yearGroups <- seq(1880, 2080, by=20)
  groupLength <- 22
  skip <- skip # Skip over notes at start of data.
  ## Must read only a specific number of rows before the trailing notes:
  numRows <- nearest_below(yearGroups, end_year) * groupLength - (nearest_above(yearGroups, end_year, TRUE) - end_year) - skip + 1
  gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

  ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
  ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
  curl <- getCurlHandle()
  curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
  tryCatch({
    r <- getURL(uri, curl=curl)
    r <- gsub("\\*\\*\\*\\*\\*", " ****", r)
    x <- read.table(text=r, header=TRUE, as.is=TRUE, na.strings=c("***", "****"), skip=skip, nrow=numRows, check.names=FALSE)
  }, error=Error, warning=Error)

  ## Remove duplicate rows with repeated column names.
  x <- x[!(duplicated(x) | duplicated(x, fromLast=TRUE)), ]

  flit <- reshape2::melt(x[, 1L:13L], id.vars="Year", variable.name="month", value.name="temp")
  for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
  flit <- dplyr::arrange(flit, Year, month)

  d <- data.frame(year=flit$Year, yr_part=flit$Year + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
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
#   get_old_monthly_gistemp("GISTEMP Global Nov. 2005", "http://web.archive.org/web/20051227031241/http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt", 8L, 2005),
#   env$d[, c(climeseries:::commonColumns, "GISTEMP Global May 2016")],
#   get_old_monthly_gistemp()
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries:::commonColumns), lapply(allSeries, names))))), allSeries)
# d <- recenter_anomalies(d, 1951:1980) # Should be the same baseline, but make sure.
# series <- c("GISTEMP Global Nov. 2005", "GISTEMP Global Nov. 2015", "GISTEMP Global May 2016", "GISTEMP Global")
# plot_climate_data(d, series=series, ma=12, lwd=2, conf_int=FALSE, show_trend=TRUE)
