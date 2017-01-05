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
# plot_horse_race("GISTEMP Global")
# plot_horse_race("UAH TLT 6.0 Global", 10)
# plot_horse_race("NCEI US Avg. Temp.", 10) # Use -10 for bottom 10 years.


#' @export
get_yearly_gistemp <- function(series="GISTEMP Met. Stations Oct. 2005", uri="https://web.archive.org/web/20051029130103/http://data.giss.nasa.gov/gistemp/graphs/Fig_A.txt", skip=0L)
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
#   get_yearly_gistemp("GISTEMP Global Met. Stations Current", "http://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A.txt")
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries:::commonColumns), lapply(allSeries, names))))), allSeries)
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

  ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
  ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
  curl <- getCurlHandle()
  curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
  #tryCatch({
    r <- getURL(uri, curl=curl)
    r <- gsub("*****", " ****", r, fixed=TRUE)
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
#   env$d[, c(climeseries:::commonColumns, "GISTEMP Global May 2016")],
#   get_old_monthly_gistemp()
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries:::commonColumns), lapply(allSeries, names))))), allSeries)
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
    webPage <- getURL(uri)
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
remove_periodic_cycle <- function(inst, series, center=TRUE, period=1, num_harmonics=4, loess...=list(), unwrap=TRUE, ...)
{
  d <- inst[, c(climeseries:::commonColumns, series)]
  if (unwrap)
    d <- subset(d, na_unwrap(d[, series]))
  d[[series %_% " (interpolated)"]] <- interpNA(d[, series], "linear")

  if (is.null(period)) { # Estimate period from data.
    spectralDensity <- spectrum(y)
    period <- 1 / spectralDensity$freq[spectralDensity$spec == max(spectralDensity$spec)]
  }

  ## Get residuals from LOESS fit.
  loessArgs = list(
    formula = eval(substitute(s ~ yr_part, list(s=as.name(series %_% " (interpolated)")))),
    data = d,
    span = 0.2
  )
  loessArgs <- modifyList(loessArgs, loess...)

  l <- do.call("loess", loessArgs)
  d[[series %_% " (LOESS fit)"]] <- l$fit
  r <- l$resid

  ## Construct model formula for given no. of harmonics.
  fBase <- "r ~ "; f <- NULL
  for (i in seq(num_harmonics))
    f <- c(f, paste0(c("sin", "cos"), paste0("(", 2 * i, " * pi / period * yr_part)")))
  f <- as.formula(paste0(fBase, paste0(f, collapse=" + ")))

  rfit <- lm(f, data=d, ...)
  uncycled <- d[[series]] - rfit$fit

  if (is.logical(center))
    d[[series %_% " (anomalies)"]] <- scale(uncycled, center=center, scale=FALSE)
  else
    d[[series %_% " (anomalies)"]] <- uncycled - mean(uncycled[d$year %in% center], na.rm=TRUE)

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
            r <- integratex(attr(y, "alt"), y)
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
      ## TODO: I might also want to weight these by sample size...? Or otherwise account for sparseness.
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
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries:::commonColumns), lapply(allSeries, names))))), allSeries)
# series <- c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global")
# plot_climate_data(d, series, start=1985, ylab="SAOD", main="Global Mean Stratospheric Aerosol Optical Depth")
## Save only OSIRIS data as CSV file.
# keepRows <- na_unwrap(d$`OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global`)
# write.csv(d[keepRows, c("year", "month", "yr_part", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global")], "./OSIRIS-SAOD_2001.11-2016.7.csv", row.names=FALSE)
