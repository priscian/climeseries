ReadAndMungeInstrumentalData <- function(series, path, baseline, verbose=TRUE)
{
  Error <- function(e) {
    cat(series %_% " " %_% type %_% " series not available.", fill=TRUE)

    if (verbose) {
      m <- NA
      if (inherits(e, "simpleWarning")) m <- "[WARNING]"
      if (inherits(e, "simpleError")) m <- "[ERROR]"

      if (!is.na(m))
        cat("   ==> ", m, " ", e$message, sep="", fill=TRUE)
    }
    flush.console()

    verbose <<- FALSE
  }

  type <- "temperature" # The default.
  if (is.list(path)) {
    type <- path$type
    path <- path$path
  }

  if (verbose)
    cat("Processing " %_% series %_% " " %_% type %_% " series.... ")

  ## N.B. The monthly midpoint calculations below would be clearer, though no more correct, as (month_num - 0.5)/12; v. http://davidappell.blogspot.com/2015/05/wood-for-trees-you-cant-trust-it.html for an example.
  d <- switch(series,
    `GISTEMP SH` =,
    `GISTEMP NH` =,
    GISTEMP = (function(p) { # GISS is weirdly formatted, not conducive to easy reading.
      x <- NULL

      yearGroups <- seq(1880, 2080, by=20)
      groupLength <- 22
      skip <- 7L # Skip over notes at start of data.
      ## Must read only a specific number of rows before the trailing notes:
      numRows <- nearest_below(yearGroups, current_year) * groupLength - (nearest_above(yearGroups, current_year, TRUE) - current_year) - skip
      gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

      ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
      ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
      curl <- getCurlHandle()
      curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        r <- getURL(p, curl=curl)
        x <- read.table(tc <- textConnection(r), header=TRUE, as.is=TRUE, na.strings=c("***", "****"), skip=skip, nrow=numRows, check.names=FALSE); close(tc)
      }, error=Error, warning=Error)

      ## Remove duplicate rows with repeated column names.
      x <- x[!(duplicated(x) | duplicated(x, fromLast=TRUE)), ]

      flit <- reshape2::melt(x[, 1L:13L], id.vars="Year", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, Year, month)

      d <- data.frame(year=flit$Year, yr_part=flit$Year + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      #d$temp <- gissGlobalMean + (d$temp / 100) # Don't need to do this.
      #d$temp <- gissGlobalMean + round(rep(runif(12), length.out=length(d$temp)), 3) + (d$temp / 100) # For testing only.
      d$temp <- d$temp / 100

      return (d)
    })(path),

    NCEI = (function(p) {
      x <- NULL

      skip <- 3L # Changed from 2 (22 Mar. 2016).

      tryCatch({
        x <- read.csv(p, header=TRUE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})(\\d{2})"
      yearMatches <- str_match(x$Year, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(yearMatches[, 3L])

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$Value, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    HadCRUT3 =,
    HadCRUT3v =,
    HadCRUT4 = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, fill=TRUE, check.names=FALSE)
      }, error=Error, warning=Error)

      y <- x[!duplicated(x[, 1L], fromLast=TRUE), ]
      x <- x[!duplicated(x[, 1L]), ]

      flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      rawTemps <- reshape2::melt(y[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(rawTemps)) rawTemps[[i]] <- as.numeric(rawTemps[[i]])
      rawTemps <- dplyr::arrange(rawTemps, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "0.000", but it's possible for an anomaly to equal 0.0, so:
      is.na(d$temp) <- d$temp == 0.000 & rawTemps$temp == 0

      return (d)
    })(path),

    `Cowtan & Way Hybrid` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})\\.(\\d{3})"
      yearMatches <- str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      ## Add value of 95% total uncertainty to data frame.
      ## 'x$V3' is 1 × sigma, so 1.96 × sigma is a 95% CI. V. http://www-users.york.ac.uk/~kdc3/papers/coverage2013/series.html for details, http://www.skepticalscience.com/kevin_cowtan_agu_fall_2014.html for a plot with uncertainty bands.
      d[[series %_% "_uncertainty"]] <- 1.96 * x$V3

      return (d)
    })(path),

    BEST = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE, comment.char="%")
      }, error=Error, warning=Error)

      x <- x[!duplicated(x[, c("V1", "V2")]), ]

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
      ## "Uncertainties represent the 95% confidence interval for statistical and spatial undersampling effects as well as ocean biases." From http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt.
      d[[series %_% "_uncertainty"]] <- x$V4

      return (d)
    })(path),

    JMA = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- tempfile()
        download.file(p, flit, quiet=TRUE)
        con <- file(flit) # R does transparent decompression of certain compressed files, e.g. ".gz".
        comments <- str_extract(readLines(con), "^#.*$")
        comments <- comments[!is.na(comments)]
        m <- read.table(tc <- textConnection(comments), comment.char=""); close(tc) # 'm' for "meta".
        x <- read.table(con, skip=skip, comment.char="#")
        unlink(flit)
      }, error=Error, warning=Error)

      ## 'x' is gridded data from which we need to calculate monthly global average temperatures, hopefully like the Japan Meteorological Agency does here: http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/ann_wld.html.

      ## Missing values are given as -9999.00.
      is.na(x) <- x == -9999.00

      nlat <- nrow(x) / nrow(m) # No. of latitude grid points.
      w <- cos(matrix(rep(seq(87.5, -87.5, length.out=nlat), ncol(x)), ncol=ncol(x), byrow=FALSE) * (pi / 180)) # Latitude weights.

      temp <- NULL

      for (i in seq(1L, nrow(x), by=nlat)) {
        xi <- data.matrix(x[i:(i + nlat - 1L), ])
        tempi <- stats::weighted.mean(xi, w, na.rm=TRUE)

        temp <- c(temp, tempi)
      }

      d <- data.frame(year=m$V2, yr_part=m$V2 + (2 * m$V3 - 1)/24, month=m$V3, temp=temp, check.names=FALSE, stringsAsFactors=FALSE)

      ## [24 Aug. 2015] Scrape JMA summary pages to get monthly anomalies (baseline 1981–2010) before the year-end data dump is posted.

      syear <- year(Sys.Date()) # This should be okay, because the JMA never updates its global temps till the next year.
      ## Oops, but in January the yearly data may not have been posted yet:
      if (month(Sys.Date()) == 1) syear <- syear - 1
      mos <- tolower(MOS)
      surl <- paste("http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/", mos, "_wld.html", sep="")

      e <- data.frame()
      tryCatch({
        for (i in seq_along(surl)) {
          y <- readLines(tc <- textConnection(getURL(surl[i]))); close(tc)
          if (any(grepl("\\s*in " %_% MONTHS[i] %_% " " %_% syear, y))) {
            tempRe <- "\\s*was\\s+((?:\\+|-)\\d+\\.\\d+)"
            lineNo <- grep(tempRe, y)
            sflit <- data.frame(year=syear, yr_part=syear + (2 * i - 1)/24, month=i, temp=NA, check.names=FALSE, stringsAsFactors=FALSE)
            if (length(lineNo > 0L)) {
              lineNo <- lineNo[1L]
              stemp <- str_match(y[lineNo], tempRe)[2L]
              sflit$temp <- as.numeric(stemp)
            }
            e <- rbind(e, sflit)
          }
        }
        base <- recenter_anomalies(recenter_anomalies(d, 1981:2010, by_month=TRUE, digits=Inf), 1971:2000, by_month=TRUE, digits=Inf, return_baselines_only=TRUE) # 1971–2000 is the JMA gridded-data baseline.
        base <- base[d$year == syear - 1]; length(base) <- nrow(e)
        e$temp <- round(e$temp - base, digits=Inf)
        d <- rbind(d, e)
      }, error=Error, warning=Error)

      return (d)
    })(path),

    `RSS TLT 3.3` =,
    `RSS TMT 3.3` =,
    `RSS TMT 4.0` = (function(p) {
      x <- NULL

      skip <- 5L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-99.9".
      is.na(d$temp) <- d$temp == -99.90

      return (d)
    })(path),

    `UAH TLT 5.6` = (function(p) {
      x <- NULL

      skip <- 5L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE, comment.char="D") # "D" is a hack to skip over the "DECADAL TREND" summary at the end.
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
      # Missing values are given as "-99.99".
      is.na(d$temp) <- d$temp == -99.99

      return (d)
    })(path),

    `UAH TLT 6.0` = (function(p) {
      x <- NULL

      skip <- 4L

      tryCatch({
        x <- read.fwf(p, widths=c(7L, 5L, 8L), header=FALSE, skip=skip, check.names=FALSE, comment.char="D") # "D" is a hack to skip over the "DECADAL TREND" summary at the end.
      }, error=Error, warning=Error)

      x <- x[complete.cases(x), ] # Necessary because of the fixed-width column reading.
      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-999.000".
      is.na(d$temp) <- d$temp == -999.000

      return (d)
    })(path),

    `RATPAC-A 850-300 mb` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        temp <- sub("^\\s", "#", readLines(p))
        y <- read.fortran(tc <- textConnection(temp), format=c("2I4", "7F7.0"), header=FALSE, skip=skip, check.names=FALSE, comment.char="#"); close(tc)
      }, error=Error, warning=Error)

      ## Use only mid-troposphere 850–300 mb readings here, but 'y' also contains 300–100 and 100–50 mb data.
      z <- y[seq(nrow(y)/3), ]
      z$month <- (z$V2 * 3) - 2 # V. http://www1.ncdc.noaa.gov/pub/data/ratpac/readme.txt

      temp <- expand.grid(month=1:12, V1=unique(z$V1))
      x <- merge(temp, z, by=c("V1", "month"), all.x=TRUE)
      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$month - 1)/24, month=x$month, temp=x$V5, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "999.000".
      is.na(d$temp) <- d$temp == 999.000

      ## Remove trailing NA's.
      d <- d[na_unwrap(d$temp), ]

      return (d)
    })(path),

    `NCEP Surface Air SH` =,
    `NCEP Surface Air NH` =,
    `NCEP Surface Air` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        ## Scrape Web page for data.
        webPage <- getURL(p)
        webPage <- readLines(tc <- textConnection(webPage)); close(tc)
        pageTree <- htmlTreeParse(webPage, useInternalNodes=TRUE)
        ## The data table is in a PRE node (the only one, hopefully).
        pre <- XML::xpathSApply(pageTree, "//*/pre", xmlValue)
        ## Clean up the table by removing descriptive text.
        tab <- gsub("^(?!\\d{4}\\s+).*$", "", strsplit(pre, '\n')[[1L]], perl=TRUE)
        x <- read.table(tc <- textConnection(tab), header=FALSE, skip=skip, fill=TRUE, check.names=FALSE); close(tc)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-999.999".
      is.na(d$temp) <- d$temp == -999.999

      return (d)
    })(path),

    Keeling = (function(p) {
      x <- NULL

      skip <- 57L

      tryCatch({
        x <- read.csv(p, header=FALSE, skip=skip, check.names=FALSE, comment.char="\"")
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V5, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-99.99".
      is.na(d$temp) <- d$temp == -99.99

      return (d)
    })(path)
  )

  if (is.null(d) && verbose) tryCatch(message(""), message=Error)
  else if (verbose) { cat("Done.", fill=TRUE); flush.console() }

  if (!is.null(d)) {
    if (!is.null(baseline)) {
      useBaseline <- TRUE
      if (is.logical(baseline)) {
        if (baseline)
          baseline <- defaultBaseline
        else {
          useBaseline <- FALSE
          d$base <- 0.0
          baseline <- NULL
        }
      }

      if (useBaseline) {
        ## Calculate monthly average temperatures over baseline period.
        flit <- subset(d, d$year %in% baseline)
        bma <- tapply(flit$temp, flit$month, mean, na.rm=TRUE)
        d$base <- NA
        null <- sapply(names(bma), function(x) { d$base[d$month == x] <<- bma[x] }); null <- NULL
      }
    }
    else
      d$base <- 0.0

    ## Center anomalies on average baseline-period temperatures.
    d$anom <- round(d$temp - d$base, 3L)
    d[[series]] <- d$anom

    d$met_year <- NA

    attr(d, "baseline") <- baseline
  }

  return (d)
}


DownloadInstrumentalData <- function(paths, baseline, verbose, dataDir, filenameBase)
{
  e <- new.env()

  for (series in names(paths)) {
    d <- ReadAndMungeInstrumentalData(series, paths[[series]], baseline=baseline, verbose=verbose)
    if (!is.null(d))
      e[[series]] <- d
  }

  d <- NULL
  for (i in ls(e)) {
    uncertainty <- grep("_uncertainty$", names(e[[i]]), value=TRUE)
    if (is.null(d))
      d <- e[[i]][, c(commonColumns, i, uncertainty)]
    else
      d <- merge(d, e[[i]][, c(commonColumns, i, uncertainty)], by=commonColumns, all=TRUE)
  }

  attr(d, "baseline") <- NULL
  if (length(e) > 0L)
    attr(d, "baseline") <- attr(e[[ls(e)[1L]]], "baseline")

  d$met_year <- shift(d$year, -1L, roll=FALSE)
  d$met_year[is.na(d$met_year)] <- max(d$year, na.rm=TRUE) + 1

  suffix <- format(Sys.Date(), "%Y%m%d")

  tempPath <- paste(dataDir, filenameBase %_% ifelse(is.null(baseline) || (is.logical(baseline) && !baseline), "raw_", "") %_% suffix, sep="/")
  save(d, file=tempPath %_% ".RData")
  dput(d, file=tempPath %_% ".dput")
  write.csv(d, file=tempPath %_% ".csv", row.names=FALSE)

  return (d)
}


LoadInstrumentalData <- function(dataDir, filenameBase, baseline=NULL)
{
  d <- NULL

  fileExtension <- "RData"
  filenames <- sort(grep("^.*?" %_% filenameBase %_% ifelse(is.null(baseline) || (is.logical(baseline) && !baseline), "raw_", "") %_% "\\d{8}" %_% "\\." %_% fileExtension %_% "$", list.files(dataDir, full.names=TRUE), value=TRUE), decreasing=TRUE)
  if (length(filenames) != 0) {
    load(filenames[1L], envir=environment()) # File extension "RData".
    #d <- dget(filenames[1L]) # File extension "dput".
    #d <- read.csv(filenames[1L]) # File extension "csv".
  }

  if (!is.null(baseline))
    d <- recenter_anomalies(d, baseline)

  return (d)
}


#' Get Monthly Climatological Data from a Local Data Set or from the Internet
#'
#' Loads climatological data from a saved data set or downloads and saves it directly from the Internet.
#'
#' @param download Logical: if \code{TRUE}, download the latest instrumental data from the Internet; if \code{FALSE}, load the most recent instrumental data set from the default data directory.
#' @param data_dir The path of the primary directory for storing downloaded instrumental temperature data. The default is a global value defined in the file "constants.R".
#' @param filename_base The starting filename for climate-series data files, to which "raw_" and/or a date representation will be appended.
#' @param urls A list of named URLs pointing to instrumental data online; the default is a global value defined in the package file "constants.R".
#' @param baseline An integer year or, more typically, range of years on which the temperature anomalies will be centered. If \code{NULL}, no baseline centering is done, and the string "raw_" will be appended to \code{filename_base} before the instrumental data is saved or loaded.
#' @param verbose Logical; passed to \code{DownloadInstrumentalData}.
#'
#' @return A data frame containing monthly climatological data sets with the following columns:
#'   \item{year}{The year CE.}
#'   \item{met_year}{The "meteorological year" starting the previous December.}
#'   \item{yr_part}{Monthly midpoint as a fraction of a year, i.e. (month - 0.5)/12.}
#'   \item{month}{Month of the year as an integer value.}
#'   \item{BEST}{Berkeley Earth Surface Temperature (BEST) global average combined land+SST temperature anomaly.}
#'   \item{BEST_uncertainty}{BEST 95\% confidence interval of uncertainty in the temperature anomaly.}
#'   \item{Cowtan & Way Hybrid}{Cowtan & Way hybrid reconstruction of HadCRUT4 global average combined land+SST temperature anomaly. (See \url{dx.doi.org/10.1002/qj.2297}.)}
#'   \item{Cowtan & Way Hybrid_uncertainty}{Cowtan & Way hybrid 95\% confidence interval of uncertainty in the temperature anomaly.}
#'   \item{GISTEMP}{Goddard Institute for Space Studies (GISS) global average combined land+SST temperature anomaly.}
#'   \item{GISTEMP NH}{GISS northern hemisphere average combined land+SST temperature anomaly.}
#'   \item{GISTEMP SH}{GISS southern hemisphere average combined land+SST temperature anomaly.}
#'   \item{HadCRUT3}{UK Met Office Hadley Centre global average combined land+SST temperature anomaly (deprecated).}
#'   \item{HadCRUT3v}{UK Met Office Hadley Centre variance-adjusted global average combined land+SST temperature anomaly (deprecated).}
#'   \item{HadCRUT4}{UK Met Office Hadley Centre global average combined land+SST temperature anomaly.}
#'   \item{JMA}{Japan Meteorological Agency (JMA) global average combined land+SST temperature anomaly.}
#'   \item{Keeling}{Scripps Institution of Oceanography CO2 measurements at Mauna Loa.}
#'   \item{NCEI}{U.S. National Centers for Environmental Information (NCEI) global average combined land+SST temperature anomaly.}
#'   \item{NCEP Surface Air}{National Centers for Environmental Prediction (NCEP) NCEP/NCAR reanalysis estimate of global average surface air temperature.}
#'   \item{NCEP Surface Air NH}{NCEP/NCAR reanalysis estimate of northern hemisphere average surface air temperature.}
#'   \item{NCEP Surface Air SH}{NCEP/NCAR reanalysis estimate of southern hemisphere average surface air temperature.}
#'   \item{RATPAC-A 850-300 mb}{NOAA's Radiosonde Atmospheric Temperature Products for Assessing Climate (RATPAC) global average tropospheric temperature anomaly (850–300 mb).}
#'   \item{RSS TLT 3.3}{Remote Sensing Systems (RSS) Temperature Lower Troposphere (TLT) global average temperature anomaly (v. 3.3).}
#'   \item{RSS TMT 3.3}{RSS Temperature Middle Troposphere (TMT) global average temperature anomaly (v. 3.3).}
#'   \item{RSS TMT 4.0}{RSS Temperature Middle Troposphere (TMT) global average temperature anomaly (v. 4.0).}
#'   \item{UAH TLT 5.6}{University of Alabama in Huntsville (UAH) Temperature Lower Troposphere (TLT) global average temperature anomaly (v. 5.6).}
#'   \item{UAH TLT 6.0}{University of Alabama in Huntsville (UAH) Temperature Lower Troposphere (TLT) global average temperature anomaly (v. 6.0).}
#'
#' @examples
#' \dontrun{
#' ## Download both centered and "raw" data.
#'
#' d <- get_climate_data(download=TRUE, baseline=TRUE)
#' e <- get_climate_data(download=TRUE, baseline=FALSE)
#'
#' ## Load both centered and "raw" data.
#'
#' d <- get_climate_data(download=FALSE, baseline=TRUE)
#' e <- get_climate_data(download=FALSE, baseline=FALSE)
#'
#' ## Which year is the warmest?
#'
#' inst <- get_climate_data(download=FALSE, baseline=TRUE)
#' series <- setdiff(names(inst), c(commonColumns, c("Keeling")))
#' yearType <- "year" # "year" or "met_year" = meteorological year.
#' annual <- sapply(series, function(s) { rv = tapply(inst[[s]], inst[[yearType]], mean, na.rm=TRUE); rv = rv[!is.nan(rv)]; rv })
#'
#' ## How many months for 'lastYear' have data?
#' lastYear <- as.integer(format(Sys.Date(), "%Y")) - 1
#' sapply(inst[inst[[yearType]] %in% lastYear, series], function(s) sum(!is.na(s)))
#' mapply(function(x, y) x[y][1L], annual, sapply(annual, function(s) { order(s, decreasing=TRUE) }))
#'
#' ## Calculate max. anomaly for years not in 'excludeDate'. (Allows exclusion of e.g. partial current year.)
#' excludeDate <- as.integer(format(Sys.Date(), "%Y")) # Or excludeDate <- c(2016)
#' annualLt <- sapply(annual, function(s) { s[!(as.numeric(names(s)) %in% excludeDate)] })
#' mapply(function(x, y) x[y][1L], annualLt, sapply(annualLt, function(s) { order(s, decreasing=TRUE) }))
#' }
#'
#' @export
get_climate_data <- function(download, data_dir, filename_base, urls=climeseries:::instrumentalUrls, baseline=1981:2010, verbose=TRUE)
{
  if (missing(data_dir)) {
    if (!is.null(getOption("climeseries_data_dir")))
      data_dir <- getOption("climeseries_data_dir")
    else
      data_dir <- dataDir
  }

  if (missing(filename_base)) {
    if (!is.null(getOption("climeseries_filename_base")))
      filename_base <- getOption("climeseries_filename_base")
    else
      filename_base <- filenameBase
  }

  d <- NULL

  if (download)
    d <- DownloadInstrumentalData(urls, baseline, verbose, data_dir, filename_base)
  else
    d <- LoadInstrumentalData(data_dir, filename_base, baseline)

  return (d)
}


#' Get Names of Climate Time Series from Data Set
#'
#' Extracts column names from downloaded climate data that correspond specifically to climatological time series.
#'
#' @param x A data frame (down)loaded by function \code{\link{get_climate_data}}.
#' @param conf_int Logical; if \code{FALSE}, omit columns containing confidence intervals.
#'
#' @return A character vector of column names.
#'
#' @export
get_climate_series_names <- function(x, conf_int=FALSE)
{
  colNames <- names(x)
  if (is.null(colNames)) colNames <- colnames(x)

  return (colNames[!grepl("(^yr_|^met_|^year|^month" %_% ifelse(conf_int, "", "|_uncertainty$") %_% ")", colNames)])
}


#' Recenter Climate-Series Anomalies on a Different Baseline
#'
#' Centers or recenters climate time series on a range of baseline years; the data can already have been centered on a baseline, or can be non-centered to start with.
#'
#' @param x A data frame (down)loaded by function \code{\link{get_climate_data}}.
#' @param baseline An integer year or, more typically, range of years on which the temperature anomalies will be centered. If \code{NULL}, no baseline centering is done; if \code{TRUE}, the default baseline of 1981–2010 is used.
#' @param digits An integer indicating the number of decimal places to be used; passed to \code{round()}.
#' @param by_month Logical; use \code{TRUE} for monthly data, \code{FALSE} for yearly data.
#' @param ... Passed to \code{\link{get_climate_series_names}}.
#'
#' @return The data frame argument \code{x} recentered on \code{baseline}.
#'
#' @export
recenter_anomalies <- function(x, baseline=defaultBaseline, digits=3L, by_month=TRUE, return_baselines_only=FALSE, ...)
{
  if (is.null(baseline))
    return (x)
  else if (is.logical(baseline)) {
    if (baseline) baseline <- defaultBaseline
    else return (x)
  }

  baselineAttribute <- attr(x, "baseline")
  if (!is.null(baselineAttribute)) {
    if (length(baselineAttribute) == length(baseline) && all(baselineAttribute == baseline))
      return (x)
  }

  tempSeries <- get_climate_series_names(x, ...)

  flit <- subset(x, x$year %in% baseline)

  for (i in tempSeries) {
    if (by_month) {
      bma <- tapply(flit[[i]], flit$month, mean, na.rm=TRUE)
      base <- rep(NA_real_, nrow(x))
      null <- sapply(names(bma), function(s) { v <- bma[s]; if (is.nan(v)) v <- 0.0; base[x$month == s] <<- v }); null <- NULL
    }
    else { # By year.
      bma <- tapply(flit[[i]], flit$year, mean, na.rm=TRUE)
      base <- mean(bma)
    }

    ## Center anomalies on average baseline-period temperatures.
    #x[[i]] <- trunc((x[[i]] - base) * 10^digits) / 10^digits
    x[[i]] <- round(x[[i]] - base, digits)
  }
  attr(x, "baseline") <- baseline

  if (return_baselines_only)
    return (base)
  else
    return (x)
}


#' Make Time Series Objects from Climate Data
#'
#' Creates \code{ts} objects from downloaded climate data.
#'
#' @param x A data set (down)loaded by function \code{\link{get_climate_data}}.
#' @param frequency The number of observations per unit of time (e.g. no. of months per year for monthly temperature data); passed to \code{link[stats]{ts}}.
#' @param ... Passed to \code{\link{get_climate_series_names}}.
#'
#' @return An object of class \code{\link[stats]{ts}}.
#'
#' @export
make_time_series_from_anomalies <- function(x, frequency=12L, ...)
{
  if (is.ts(x))
    return (x)

  d <- x[, get_climate_series_names(x, ...), drop=FALSE]

  startTime <- unlist(x[1L, c("year", "month")])

  s <- ts(d, start=startTime, frequency=frequency)

  return (s)
}
