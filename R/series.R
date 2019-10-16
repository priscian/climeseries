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
    `GISTEMP SH Land` =,
    `GISTEMP NH Land` =,
    `GISTEMP Global Land` =,
    `GISTEMP SH` =,
    `GISTEMP NH` =,
    `GISTEMP Global` = (function(p) { # GISS .TXT files are weirdly formatted, so use the .CSVs instead.
      x <- NULL

      skip <- 1L

      ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
      ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
      curl <- getCurlHandle()
      curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        #r <- getURL(p, curl=curl)
        #x <- read.csv(text=r, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
        x <- read.csv(p, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x[, 1L:13L], id.vars="Year", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, Year, month)

      d <- data.frame(year=flit$Year, yr_part=flit$Year + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `GISTEMP Zonal` =,
    `GISTEMP Zonal Land` = (function(p) {
      x <- NULL

      skip <- 0L

      ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
      ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
      curl <- getCurlHandle()
      curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        #r <- getURL(p, curl=curl)
        #x <- read.csv(text=r, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
        x <- read.csv(p, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- x[, -1]
      colnames(flit) <- paste(series, colnames(flit))
      d <- cbind(data.frame(year=x$Year, month=6, check.names=FALSE, stringsAsFactors=FALSE), flit)
      d <- base::merge(expand.grid(month=1:12, year=d$year), d, by=c("year", "month"), all=TRUE)
      d$yr_part <- d$year + (2 * d$month - 1)/24

      return (d)
    })(path),

    `NCEI US Avg. Temp.` =,
    `NCEI US Max. Temp.` =,
    `NCEI US Min. Temp.` =,
    `NCEI US Precip.` =,
    `NCEI US PDSI` =,
    `NCEI US PHDI` =,
    `NCEI US PMDI` =,
    `NCEI US Palmer Z-Index` =,
    `NCEI SH Land` =,
    `NCEI NH Land` =,
    `NCEI Global Land` =,
    `NCEI SH Ocean` =,
    `NCEI NH Ocean` =,
    `NCEI Global Ocean` =,
    `NCEI SH` =,
    `NCEI NH` =,
    `NCEI Global` = (function(p) {
      x <- NULL

      tryCatch({
        flit <- strsplit(httr::content(httr::GET(p), "text", encoding="ISO-8859-1"), "\r*\n")[[1L]]
        flit <- flit[trimws(flit) != ""]
        flit <- flit[grep("^\\d", flit, perl=TRUE)]
        x <- read.csv(header=FALSE, skip=0L, text=flit, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})(\\d{2})"
      yearMatches <- str_match(x[[1]], re) # Column name "Year" or "Date".
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(yearMatches[, 3L])

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x[[2]], check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-9999".
      is.na(d$temp) <- d$temp == -9999

      if (grepl("^NCEI US\\s+.*?Temp.*?$", series)) { # US temps given in Fahrenheit.
        d$temp <- fahr_to_celsius(d$temp)
      }

      return (d)
    })(path),

    `ERSSTv4` = (function(p) {
      skip <- 0L

      tryCatch({
        fileNames <- strsplit(getURL(p, dirlistonly=TRUE), "\r*\n")[[1L]]
        ## Keep only monthly data files.
        re <- "^aravg\\.mon\\.(?<type>.+?)\\.(?<lat1>.+?)\\.(?<lat2>.+?)\\..*?\\.asc$"
        fileNames <- grep(re, fileNames, value=TRUE, perl=TRUE)
        m <- regexpr(re, fileNames, perl=TRUE)
        namedMatches <- cbind(file=fileNames, parse_one(fileNames, m))
        namedMatches[, "type"] <- capwords(sub("_", " + ", namedMatches[, "type"]))
        cat(fill=TRUE)
        l <- apply(namedMatches, 1,
          function(y)
          {
            seriesName <- paste("ERSSTv4", y["type"], y["lat1"] %_% "-" %_% y["lat2"])
            cat("    Processing file", y["file"], fill=TRUE); flush.console()
            x <- read.table(p %_% y["file"], header=FALSE, skip=skip, fill=TRUE, check.names=FALSE)
            d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, conf_int=1.96 * sqrt(x$V4), check.names=FALSE, stringsAsFactors=FALSE)
            ## N.B. See ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/timeseries/readme.timeseries for "total error variance."
            names(d)[names(d) == "temp"] <- seriesName
            names(d)[names(d) == "conf_int"] <- seriesName %_% "_uncertainty"

            d
          }
        )
      }, error=Error, warning=Error)

      return (l)
    })(path),

    `HadSST3 SH` =,
    `HadSST3 NH` =,
    `HadSST3 Tropics` =,
    `HadSST3 Global` =,
    `HadCRUT4 SH` =,
    `HadCRUT4 NH` =,
    `HadCRUT4 Tropics` =,
    `HadCRUT4 Global` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, fill=TRUE, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})/(\\d{2})"
      yearMatches <- str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(yearMatches[, 3L])

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      ## Apparently there are no missing values, so no need for their conversion to NA.

      ## Add value of 95% total uncertainty to data frame.
      ## "Columns 9 and 10 are the lower and upper bounds of the 95% confidence interval of the combination of measurement and sampling and bias uncertainties. Columns 11 and 12 are the lower and upper bounds of the 95% confidence interval of the combined effects of all the uncertainties described in the HadCRUT4 error model (measurement and sampling, bias and coverage uncertainties)." From http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/series_format.html.
      ## [23 Aug. 2016] But even the Met Office doesn't use the full uncertainty ensemble here: http://www.metoffice.gov.uk/research/monitoring/climate/surface-temperature.
      d[[series %_% "_uncertainty"]] <- x$V10 - x$V9
      #d[[series %_% "_uncertainty"]] <- x$V12 - x$V11

      return (d)
    })(path),

    `HadCET` = (function(p) {
      x <- NULL

      skip <- 7L

      tryCatch({
        x <- read.table(p, header=FALSE, as.is=TRUE, na.strings=c("-99.9", "-99.99"), skip=skip, check.names=FALSE, stringsAsFactors=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `Cowtan & Way Krig. Global` = (function(p) {
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

    `BEST Global Land` =,
    `BEST SH Land` =,
    `BEST NH Land` =,
    `BEST US` =,
    `BEST Antarctica` =,
    `BEST Greenland` =,
    `BEST Global` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE, comment.char="%")
        dev_null <- sapply(names(x), function(y) is.na(x[[y]]) <<- is.nan(x[[y]]))
      }, error=Error, warning=Error)

      dupIndex <- duplicated(x[, c("V1", "V2")])
      if (any(dupIndex)) {
        if (grepl("Land_and_Ocean_complete", path)) { # The Berkeley Earth Land + Ocean data set.
          ## "with Sea Ice Temperature Inferred from Air Temperatures"
          y <- x[!dupIndex, ]
          d1 <- data.frame(year=y$V1, yr_part=y$V1 + (2 * y$V2 - 1)/24, month=y$V2, `BEST Global (Air Ice Temp.)`=y$V3, check.names=FALSE, stringsAsFactors=FALSE)
          d1Series <- grep("^yr_|^met_|^year|^month|_uncertainty$", names(d1), value=TRUE, invert=TRUE)
          d1[[d1Series %_% "_uncertainty"]] <- y$V4

          ## "with Sea Ice Temperature Inferred from Water Temperatures"
          y <- x[dupIndex, ]
          d2 <- data.frame(year=y$V1, yr_part=y$V1 + (2 * y$V2 - 1)/24, month=y$V2, `BEST Global (Water Ice Temp.)`=y$V3, check.names=FALSE, stringsAsFactors=FALSE)
          d2Series <- grep("^yr_|^met_|^year|^month|_uncertainty$", names(d2), value=TRUE, invert=TRUE)
          d2[[d2Series %_% "_uncertainty"]] <- y$V4

          d <- list(d1, d2); names(d) <- c(d1Series, d2Series)
        }
        else
          stop("Unknown data set.")
      }
      else {
        d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
        ## "Uncertainties represent the 95% confidence interval for statistical and spatial undersampling effects as well as ocean biases." From http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt.
        d[[series %_% "_uncertainty"]] <- x$V4
      }

      return (d)
    })(path),

    `JMA Global` = (function(p) {
      x <- NULL

      skip <- 1L

      tryCatch({
        flit <- gsub("\\*", "", readLines(p), fixed=FALSE)
        x <- read.csv(text=flit, header=FALSE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x, id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `JMA Global (gridded)` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- tempfile()
        download.file(p, flit, quiet=TRUE)
        con <- file(flit) # R does transparent decompression of certain compressed files, e.g. ".gz".
        comments <- str_extract(readLines(con), "^#.*$")
        comments <- comments[!is.na(comments)]
        m <- read.table(text=comments, comment.char="") # 'm' for "meta".
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
          y <- readLines(tc <- textConnection(httr::content(httr::GET(surl[i]), "text", encoding="Shift_JIS"))); close(tc)
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

    `RSS TLS 3.3 Land` =,
    `RSS TLS 3.3 Ocean` =,
    `RSS TLS 3.3` =,
    `RSS TLT 3.3 Land` =,
    `RSS TLT 3.3 Ocean` =,
    `RSS TLT 3.3` =,
    `RSS TLT 4.0 Land` =,
    `RSS TLT 4.0 Ocean` =,
    `RSS TLT 4.0` =,
    `RSS TMT 3.3 Land` =,
    `RSS TMT 3.3 Ocean` =,
    `RSS TMT 3.3` =,
    `RSS TMT 4.0 Land` =,
    `RSS TMT 4.0 Ocean` =,
    `RSS TMT 4.0` =,
    `RSS TTS 3.3 Land` =,
    `RSS TTS 3.3 Ocean` =,
    `RSS TTS 3.3` =,
    `RSS TTT 3.3 Land` =,
    `RSS TTT 3.3 Ocean` =,
    `RSS TTT 3.3` =,
    `RSS TTT 4.0 Land` =,
    `RSS TTT 4.0 Ocean` =,
    `RSS TTT 4.0` = (function(p) {
      x <- NULL

      skip <- 3L

      tryCatch({
        temp <- trimws(readLines(p, n=2L)) # I.e. 'skip - 1'.
        temp[1] <- trimws(substring(temp[1], abs(diff(nchar(temp))) - 1))
        flitNames <- paste(series, apply(read.table(text=temp, stringsAsFactors=FALSE), 2, paste, collapse=""))
        flitNames <- gsub("\\.([A-Za-z])", ". \\1", flitNames) # Add spaces after periods preceding letters in column names.
        flitNames <- gsub("/$", "", flitNames) # Remove any trailing '/' slashes from column names.
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.matrix(x[, -(1:2)])
      ## Missing values are given as "-99.9".
      is.na(flit) <- flit == -99.90
      colnames(flit) <- flitNames
      d <- cbind(d, flit)

      return (d)
    })(path),

    `UAH TLS 5.6` =,
    `UAH TMT 5.6` =,
    `UAH TLT 5.6` =,
    `UAH TLS 6.0` =,
    `UAH TMT 6.0` =,
    `UAH TLT 6.0` =,
    `UAH TTP 6.0` = (function(p) {
      x <- NULL

      skip <- 1L

      tryCatch({
        flit <- readLines(p)
        flit <- flit[trimws(flit) != ""]
        flit <- split_at(flit, which(duplicated(flit)))[[1]]
        flit <- gsub("(\\d)(-\\d)", "\\1 \\2", flit)
        x <- read.table(header=FALSE, skip=skip, text=flit, check.names=FALSE, comment.char = "*")
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.matrix(x[, -(1:2)])
      ## There are no missing values in these data sets, and no missing-value value is given.
      #is.na(flit) <- flit == -99.99
      colParts <- list(c("Global", "NH", "SH", "Tropics", "NH Extratropics", "SH Extratropics", "NH Polar", "SH Polar"), c("", " Land", " Ocean"))
      colNames <- as.vector(t(outer(colParts[[1]], colParts[[2]], paste, sep=""))) # 't()' preserves the order.
      colNames <- paste(series, c(colNames, "USA 48", "USA 48 + Alaska", "Australia"))
      colnames(flit) <- colNames
      d <- cbind(d, flit)

      return (d)
    })(path),

    `RATPAC-A Seasonal Layers` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        tempFile <- tempfile()
        download.file(p, tempFile, quiet=TRUE)
        con <- unz(tempFile, "RATPAC-A-seasonal-layers.txt")
        flit <- readLines(con)
        close(con); unlink(tempFile)
        re <- "^\\s+(\\d{1})"
        pressureLayers <- trimws(grep(re, flit, value=TRUE, perl=TRUE))
        flit <- sub(re, "#\\1", flit)
        flit <- gsub("\t", " ", flit) # Remove some curious tabs in the headers.
        #flit <- sub("^\\s+", "", flit)
        flit <- split_at(flit, grep("^#", flit, perl=TRUE))
        y <- lapply(flit,
          function(x)
          {
            r <- read.fortran(tc <- textConnection(x), format=c("2I4", "7F7.0"), header=TRUE, skip=skip, check.names=FALSE, comment.char="#"); close(tc)

            ## Since the headers are read in badly, replace them outright.
            names(r) <- c("year", "season", "NH", "SH", "Global", "Tropics", "NH Extratropics", "SH Extratropics", "20N-S")
            r
          }
        )
      }, error=Error, warning=Error)

      d <- mapply(pressureLayers, y,
        FUN = function(l, y)
        {
          y$month <- (y$season * 3) - 2 # V. http://www1.ncdc.noaa.gov/pub/data/ratpac/readme.txt
          flit <- expand.grid(month=1:12, year=unique(y$year))
          flit <- merge(flit, y, by=c("year", "month"), all.x=TRUE)
          d <- data.frame(year=flit$year, yr_part=flit$year + (2 * flit$month - 1)/24, month=flit$month, check.names=FALSE, stringsAsFactors=FALSE)
          flit <- data.matrix(flit[, -(1:3)])
          is.na(flit) <- flit == 999.000
          d <- cbind(d, flit)
          names(d)[!(names(d) %in% common_columns)] <- paste("RATPAC-A", l, names(d)[!(names(d) %in% common_columns)])

          d
        }, SIMPLIFY = FALSE
      )

      return (d)
    })(path),

    `RATPAC-A Annual Levels` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        tempFile <- tempfile()
        download.file(p, tempFile, quiet=TRUE)
        con <- unz(tempFile, "RATPAC-A-annual-levels.txt")
        flit <- readLines(con)
        close(con); unlink(tempFile)
        re <- "^\\s+(\\D{1})"
        regions <- sub("\\s*(-)\\s*", "\\1", trimws(grep(re, flit, value=TRUE, perl=TRUE)), perl=TRUE)
        flit <- sub(re, "#\\1", flit)
        flit <- split_at(flit, grep("^#", flit, perl=TRUE))
        y <- lapply(flit,
          function(x)
          {
            r <- read.table(text=x, header=TRUE, skip=skip, check.names=FALSE, comment.char="#")

            r
          }
        )
      }, error=Error, warning=Error)

      d <- mapply(regions, y,
        FUN = function(l, y)
        {
          y$month <- 6
          flit <- expand.grid(month=1:12, year=unique(y$year))
          flit <- merge(flit, y, by=c("year", "month"), all.x=TRUE)
          d <- data.frame(year=flit$year, yr_part=flit$year + (2 * flit$month - 1)/24, month=flit$month, check.names=FALSE, stringsAsFactors=FALSE)
          flit <- data.matrix(flit[, -(1:3)])
          is.na(flit) <- flit == 999.000
          d <- cbind(d, flit)
          names(d)[!(names(d) %in% common_columns)] <- paste("RATPAC-A", names(d)[!(names(d) %in% common_columns)], "mb", l)

          d
        }, SIMPLIFY = FALSE
      )

      return (d)
    })(path),

    `NCEP Surface Air SH` =,
    `NCEP Surface Air SH Polar` =,
    `NCEP Surface Air NH` =,
    `NCEP Surface Air NH Polar` =,
    `NCEP Surface Air Tropics` =,
    `NCEP Surface Air USA 48` =,
    `NCEP Surface Air Global` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        ## Scrape Web page for data.
        webPage <- readLines(tc <- textConnection(httr::content(httr::GET(p), "text", encoding="ISO-8859-1"))); close(tc)
        pageTree <- htmlTreeParse(webPage, useInternalNodes=TRUE)
        ## The data table is in a PRE node (the only one, hopefully).
        pre <- XML::xpathSApply(pageTree, "//*/pre", xmlValue)
        ## Clean up the table by removing descriptive text.
        tab <- gsub("^(?!\\d{4}\\s+).*$", "", strsplit(pre, '\n')[[1L]], perl=TRUE)
        x <- read.table(text=tab, header=FALSE, skip=skip, fill=TRUE, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-999.999".
      is.na(d$temp) <- d$temp == -999.999

      return (d)
    })(path),

    `CO2 Mauna Loa` = (function(p) {
      x <- NULL

      skip <- 57L

      tryCatch({
        x <- read.csv(p, header=FALSE, skip=skip, check.names=FALSE, comment.char="\"")
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V5, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-99.99".
      is.na(d$temp) <- d$temp == -99.99

      return (d)
    })(path),

    `CO2 NOAA ESRL` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, check.names=FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V4, check.names=FALSE, stringsAsFactors=FALSE)
      ## Missing values are given as "-99.99".
      is.na(d$temp) <- d$temp == -99.99

      return (d)
    })(path),

    `CO2 Cape Grim` = (function(p) {
      x <- NULL

      tryCatch({
        flit <- readLines(p)
        flit <- flit[trimws(flit) != ""]
        flit <- flit[grep("^\\d{4}", flit, perl=TRUE)]
        x <- read.csv(header=FALSE, skip=0L, text=flit, fill=TRUE, check.names=FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V5, check.names=FALSE, stringsAsFactors=FALSE)
      ## 'x$V3' is 1 × sigma, so 1.96 × sigma is a 95% CI, I think.
      d[[series %_% "_uncertainty"]] <- 1.96 * x$V6

      return (d)
    })(path),

    `PIOMAS Arctic Sea Ice Volume` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x, id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      is.na(d$temp) <- d$temp == -1.0

      return (d)
    })(path),

    `NSIDC Sea Ice` = (function(p) {
      x <- NULL

      tryCatch({
        x <- mapply(climeseries:::MOS, sprintf("%02d", seq_along(climeseries:::MOS)),
          FUN = function(x, y)
          {
            r <- data.frame()
            for (i in c("north", "south")) {
              flit <- readLines(paste(p, i, "monthly", "data", paste(toupper(substr(i, 1, 1)), y, "extent_v3.0.csv", sep="_"), sep="/"))
              flit <- read.csv(text=flit[!grepl("^\\s+", flit)], header=TRUE, check.names=FALSE)
              r <- rbind(r, flit)
            }

            r
          }, SIMPLIFY = FALSE
        )
      }, error=Error, warning=Error)

      flit <- dplyr::arrange(Reduce(rbind, x), region, year, mo)
      y <- data.frame(year=flit$year, month=flit$mo, region=flit$region %_% "H", check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.matrix(flit[, -(1:4)])
      is.na(flit) <- flit == -9999
      y <- cbind(y, flit)
      y <- by(y, y$region, identity, simplify=FALSE)

      re <- "^(extent|area)"
      for (i in names(y)) {
        n <- names(y[[i]])
        names(y[[i]])[grep(re, n, perl=TRUE)] <- paste(series, trimws(capwords(sub(re, i %_% " \\1", n[grep(re, n, perl=TRUE)]))))
      }
      d <- base::merge(y[[1]][, -3], y[[2]][, -3], by=c("year", "month"), all=TRUE) # -3 means leave out column "region".
      globalColumns <- paste(series, "Global", c("Extent", "Area"))
      d[[globalColumns[1]]] <- rowSums(d[, grep("extent", names(d), ignore.case=TRUE)])
      d[[globalColumns[2]]] <- rowSums(d[, grep("area", names(d), ignore.case=TRUE)])
      d$yr_part <- d$year + (2 * d$month - 1)/24
      d <- dplyr::arrange(d, yr_part)

      return (d)
    })(path),

    `PMOD TSI` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, skip=1L, comment.char=";", check.names=FALSE, colClasses=c(V1="character"))
      }, error=Error, warning=Error)

      dates <- as.POSIXct(x$V2 * 86400, origin="1980-01-01")
      d <- data.frame(year(dates), month(dates), check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.matrix(x[, -(1:2)])
      is.na(flit) <- flit < -98
      d <- cbind(d, flit)
      names(d) <- c("year", "month", "PMOD TSI (new VIRGO)", "PMOD TSI (orig. VIRGO)")

      ## This data is daily, so it needs to be turned into monthly averages.
      d <- cbind(d[!duplicated(d[, 1:2]), 1:2], Reduce(rbind, by(d[, -(1:2)], list(d$month, d$year), colMeans, na.rm=TRUE, simplify=FALSE)))

      d$yr_part <- d$year + (2 * d$month - 1)/24

      return (d)
    })(path),

    `SORCE TSI` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, skip=0L, comment.char=";", check.names=FALSE, stringsAsFactors=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})(\\d{2})(\\d{2})"
      yearMatches <- str_match(trunc(x$V1), re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, month=monthValue, check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.frame(x$V5, x$V9, check.names=FALSE, stringsAsFactors=FALSE)
      names(flit) <- c(series, series %_% "_uncertainty")
      flit <- data.matrix(flit)
      is.na(flit) <- flit == 0.0
      d <- cbind(d, flit)

      ## This data is daily, so it needs to be turned into monthly averages.
      d <- cbind(d[!duplicated(d[, 1:2]), 1:2], Reduce(rbind, by(d[, -(1:2)], list(d$month, d$year), colMeans, na.rm=TRUE, simplify=FALSE)))

      d$yr_part <- d$year + (2 * d$month - 1)/24

      return (d)
    })(path),

    `TSI Reconstructed` = (function(p) {
      x <- NULL

      tryCatch({
        #x <- read.table(p, comment.char=";", check.names=FALSE) # N.B. This is currently (19 Aug 2019) offline; use local copy instead.
        x <- read.table(system.file("extdata/tsi/TIM_TSI_Reconstruction.txt", package = "climeseries"), comment.char = ";", check.names = FALSE)
      }, error=Error, warning=Error)

      yrs <- as.numeric(sub("(.+?)\\..+?", "\\1", x$V1, perl=TRUE))
      d <- data.frame(year=yrs, month=6, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      ## Remove any duplicated years (which seems to be a problem).
      d <- subset(d, !duplicated(d[, "year"]))
      ## Since this is the oldest data set so far, make sure all months are accounted for at the start of the data set.
      d <- base::merge(expand.grid(month=1:12, year=d$year), d, by=c("year", "month"), all=TRUE)
      d$yr_part <- d$year + (2 * d$month - 1)/24

      return (d)
    })(path),

    `Rutgers NH Snow Cover` =,
    `Rutgers Eurasia Snow Cover` =,
    `Rutgers N. America Snow Cover` =,
    `Rutgers N. America (No Greenland) Snow Cover` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, check.names=FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, month=x$V2, yr_part=x$V1 + (2 * x$V2 - 1)/24, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `NOAA Sunspot No.` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, comment.char="*")
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, month=x$V2, yr_part=x$V1 + (2 * x$V2 - 1)/24, temp=x$V4, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `CSIRO Global Mean Sea Level` = (function(p) {
      x <- NULL

      skip <- 1L # Ignore header

      tryCatch({
        CSIRO_down <- TRUE # Set to TRUE if the CSIRO FTP site fails.
        alt_p <- system.file("extdata/latest/CSIRO_Alt.csv", package="climeseries")
        if (!CSIRO_down)
          download.file(p, alt_p, mode = "wb", quiet = TRUE)
        x <- read.csv(ifelse(CSIRO_down, alt_p, p), header = FALSE, skip = skip, check.names = FALSE, na.strings = "#N/A")
      }, error = Error, warning = Error)

      re <- "(\\d{4})\\.(\\d{3})"
      yearMatches <- str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `NOAA Global Mean Sea Level` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.csv(p, header=TRUE, skip=skip, check.names=FALSE, comment.char="#")
      }, error=Error, warning=Error)

      l <- na.omit(tbl_dt(melt(x, id="year")), cols="value")
      setnames(l, c("yr_part", "source", "temp"))
      setcolorder(l, c(1, 3, 2))

      r <- range(trunc(l$yr_part))
      flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
      flit$yr_part <- flit$year + (2 * flit$month - 1)/24
      flit <- tbl_dt(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(l)
      m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
      m <- dplyr::full_join(flit, m, by=c("year", "month"))

      ## Don't use, but keep available. I may have to switch entirely to data tables to add it as a column attribute to the final data set.
      satSources <- m[!duplicated(m, by=c("year", "month")), .(year, month, source)]

      d <- m[, !"source", with=FALSE][, .(temp = mean(temp, na.rm=TRUE)), by=.(year, month)]
      set(d, i=which(is.nan(d$temp)), j="temp", value=NA)
      d[, yr_part := year + (2 * month - 1)/24]

      return (as.data.frame(d))
    })(path),

    `CSIRO Reconstructed Global Mean Sea Level` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- tempfile()
        download.file(p, flit, quiet=TRUE)
        con <- unz(flit, "church_white_gmsl_2011_up/CSIRO_Recons_gmsl_mo_2015.csv")
        x <- read.csv(con, header=TRUE, as.is=TRUE, skip=skip, check.names=FALSE)
        unlink(flit)
      }, error=Error, warning=Error)

      colnames(x) <- c("yr_part", series, "_uncertainty")

      r <- range(trunc(x$yr_part))
      flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
      flit$yr_part <- flit$year + (2 * flit$month - 1)/24
      flit <- tbl_dt(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(x)
      m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
      m <- dplyr::full_join(flit, m, by=c("year", "month"))
      ## Uncertainties are 1 × sigma (Church & White 2011, dx.doi.org/10.1007/s10712-011-9119-1), so 1.96 × sigma is a 95% CI.
      m[, `_uncertainty` := 1.96 * `_uncertainty`]
      setnames(m, "_uncertainty", series %_% "_uncertainty")
      d <- as.data.frame(m)

      return (d)
    })(path),

    `Antarctica Land Ice Mass Variation` =,
    `Greenland Land Ice Mass Variation` =,
    `Ocean Mass Variation` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        ## Workaround for new PO.DAAC drive.
        creds <- options("climeseries_podaac_creds")[[1]]
        x0 <- httr::GET(p, httr::authenticate(user = creds$user, password = creds$password))

        x <- read.table(text = httr::content(x0), header = FALSE, skip = skip, check.names = FALSE, comment.char = "H")
      }, error = Error, warning = Error)

      x <- x[, 1:3] # Ocean mass has more than 3 columns.
      colnames(x) <- c("yr_part", series, "_uncertainty")

      r <- range(trunc(x$yr_part))
      flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
      flit$yr_part <- flit$year + (2 * flit$month - 1)/24
      flit <- tbl_dt(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(x)
      m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
      m <- m[, lapply(.SD, mean, na.rm=TRUE), by=.(year, month), .SDcols=c(series, "_uncertainty")] # Remove year/month duplicates by averaging.
      m <- dplyr::full_join(flit, m, by=c("year", "month"))
      ## Uncertainties are 1 × sigma, so 1.96 × sigma is a 95% CI.
      m[, `_uncertainty` := 1.96 * `_uncertainty`]
      setnames(m, "_uncertainty", series %_% "_uncertainty")
      d <- as.data.frame(m)

      return (d)
    })(path),

    `Multivariate ENSO Index` =,
    `Extended Multivariate ENSO Index` = (function(p) {
      x <- NULL

      tryCatch({
        flit <- readLines(tc <- textConnection(httr::content(httr::GET(p), "text", encoding="ISO-8859-1"))); close(tc)
        flit <- flit[trimws(flit) != ""]
        flit <- flit[grep("^\\d{4}\\s+", flit, perl=TRUE)]
        flit <- gsub("\t", " ", flit)
        x <- read.table(header=FALSE, skip=1L, text=flit, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x, id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      is.na(d$temp) <- d$temp == -999.00

      return (d)
    })(path),

    `MODIS Aerosol Optical Thickness (550 nm)` = (function(p) {
      curl <- getCurlHandle()
      curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        ## Get new session ID.
        r <- getURLContent("http://giovanni.gsfc.nasa.gov/giovanni/daac-bin/service_manager.pl", curl=curl)
        xml_ <- xmlParse(r, useInternalNodes=TRUE)
        sessionId <- XML::xpathSApply(xml_, "/session", xmlAttrs)["id"]
        submitUrl <- sub("@@DATE@@", format(today(), "%Y-%m-%d"), sub("@@SESSIONID@@", sessionId, p))

        ## Set off build of MODIS AOD data set.
        sessionContent <- getURLContent(submitUrl, curl=curl)
        sessionJson <- fromJSON(sessionContent, simplifyVector=TRUE, flatten=TRUE)
        resultsetId <- sessionJson$session$resultset$id
        resultId <- sessionJson$session$resultset$result[[1]]$id

        ## Check progress of build of MODIS AOD data set.
        percentComplete <- 0
        cat(fill=TRUE)
        while (percentComplete != 100) {
          cat("    Data build", percentComplete %_% "% complete.", fill=TRUE); flush.console()
          progressUrlBase <- "http://giovanni.gsfc.nasa.gov/giovanni/daac-bin/service_manager.pl?session=@@SESSIONID@@&resultset=@@RESULTSETID@@&result=@@RESULTID@@&portal=GIOVANNI&format=json"
          progressUrl <- sub("@@RESULTID@@", resultId, sub("@@RESULTSETID@@", resultsetId, sub("@@SESSIONID@@", sessionId, progressUrlBase)))
          progressContent <- getURLContent(progressUrl, curl=curl)
          progressJson <- fromJSON(progressContent, simplifyVector=TRUE, flatten=TRUE)
          percentComplete <- progressJson$session$result$result[[1]]$status[[1]]$percentComplete[[1]]$value
          Sys.sleep(15) # Pause for 15 s.
        }
        cat("    Data build", percentComplete %_% "% complete.", fill=TRUE); flush.console()

        ## Get resulting data set.
        dataUrl <- "http://giovanni.gsfc.nasa.gov/giovanni/" %_% progressJson$session$result$result[[1]]$data[[1]]$fileGroup[[1]]$dataFile[[1]]$dataUrl[[1]]$value
        skip <- 7L
        x <- read.csv(dataUrl, header=TRUE, as.is=TRUE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})-(\\d{2})"
      dateMatches <- str_match(x[[1]], re)
      yearValue <- as.numeric(dateMatches[, 2L])
      monthValue <- as.numeric(dateMatches[, 3L])
      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x[[2]], check.names=FALSE, stringsAsFactors=FALSE)
      is.na(d$temp) <- d$temp == -9999

      #browser()
      return (d)
    })(path),

    `GISS Stratospheric Aerosol Optical Depth (550 nm)` = (function(p) {
      x <- NULL

      skip <- 4L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})\\.(\\d{3})"
      yearMatches <- str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.matrix(x[, -1])# * 1000
      ## Missing values.
      #is.na(flit) <- flit == missingValue
      colnames(flit) <- paste(series, c("Global", "NH", "SH"))
      d <- cbind(d, flit)

      return (d)
    })(path),

    ## This is abandoned for now because of the amount of data; instead, do FTP download first, then process locally.
    `OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Alt` = (function(p) {
      skip <- 0L

      datasetPathBase <- "/HDFEOS/SWATHS/OSIRIS\\Odin Aerosol MART/"
      datasetPaths <- datasetPathBase %_% c("Data Fields/AerosolExtinction", "Geolocation Fields/" %_% c("Altitude", "Latitude", "Longitude"))
      names(datasetPaths) <- c("extinction", "alt", "long", "lat")
      #tryCatch({
        dirNames <- strsplit(getURL(p, dirlistonly=TRUE), "\r*\n")[[1L]]
        x <- list()
        cat(fill=TRUE)
        for (i in dirNames) {
          fileNames <- strsplit(getURL(paste0(p, i, "/"), dirlistonly=TRUE), "\r*\n")[[1L]]
          ## Make sure the following regex doesn't change or lead to mixed file versions.
          fileNames <- grep("^OSIRIS-Odin_L2-Aerosol-Limb-MART", fileNames, value=TRUE)
          x[[i]] <- list()
          for (j in fileNames) {
            re <- ".*?_(\\d{4})m(\\d{4})\\..*$"
            dateMatches <- str_match(j, re)
            yyyymmdd <- paste(dateMatches[, 2:3], collapse="")

            cat("    Downloading file", j, fill=TRUE); flush.console()
            flit <- tempfile()
            download.file(paste0(p, i, "/", j), flit, mode="wb", quiet=TRUE)

            x[[i]][[j]] <- list()
            for (k in names(datasetPaths))
              x[[i]][[j]][[k]] <- h5read(flit, datasetPaths[k])
          }
        }
      #}, error=Error, warning=Error)

      save(x, "OSIRIS-Odin_L2-Aerosol-Limb-MART_v5-07.RData")

      browser()
      return (d)
    })(path),

    `OSIRIS Stratospheric Aerosol Optical Depth (550 nm)` = (function(p) {
      ## Global
      saod_daily_gl <- create_osiris_saod_data()

      ## NH
      dailyFilenameNh <- "OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm_NH.RData"
      seriesNameNh <- "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) NH"
      saod_daily_nh <- create_osiris_saod_data(filename=dailyFilenameNh, series_name=seriesNameNh)

      ## SH
      dailyFilenameSh <- "OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm_SH.RData"
      seriesNameSh <- "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) SH"
      saod_daily_sh <- create_osiris_saod_data(filename=dailyFilenameSh, series_name=seriesNameSh)

      ## Merge.
      allSeries <- list(saod_daily_gl, saod_daily_nh, saod_daily_sh)
      d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(common_columns), lapply(allSeries, names))))), allSeries)

      return (d)
    })(path),

    `ESRL AMO` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- readLines(tc <- textConnection(httr::content(httr::GET(p), "text", encoding="ISO-8859-1"))); close(tc)
        flit <- trimws(flit[grep("^\\s\\d{4}\\s", flit, perl=TRUE)])
        x <- read.table(header=FALSE, skip=skip, text=flit, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x, id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)
      is.na(d$temp) <- d$temp == -99.990

      return (d)
    })(path),

    ## This processing is obsolete. ERA5 monthly data coming soon at https://cds.climate.copernicus.eu
    `ERA-Interim 2m` = (function(p) {
      currentMonth <- current_month; currentYear <- current_year
      if (currentMonth == 1) { currentYearLastMonth <- currentYear - 1; currentMonthLastMonth <- 12 }
      else currentMonthLastMonth <- currentMonth - 1; currentYearLastMonth <- currentYear

      uri <- sub("@@MONTHNUM@@", sprintf("%02d", currentMonth), sub("@@YEARNUM@@", currentYear, p))
      uri <- sub("@@MONTHNUM_LASTMONTH@@", sprintf("%02d", currentMonthLastMonth), sub("@@YEARNUM_LASTMONTH@@", currentYearLastMonth, uri))
      if (!url.exists(uri)) { ## Does the previous month's data exist?
        if (currentMonth == 1) { currentYear <- currentYear - 1; currentMonth <- 12 }
        else currentMonth <- currentMonth - 1

        if (currentMonthLastMonth == 1) { currentYearLastMonth <- currentYearLastMonth - 1; currentMonthLastMonth <- 12 }
        else currentMonthLastMonth <- currentMonthLastMonth - 1

        uri <- sub("@@MONTHNUM@@", sprintf("%02d", currentMonth), sub("@@YEARNUM@@", currentYear, p))
        uri <- sub("@@MONTHNUM_LASTMONTH@@", sprintf("%02d", currentMonthLastMonth), sub("@@YEARNUM_LASTMONTH@@", currentYearLastMonth, uri))
      }

      x <- NULL

      skip <- 2

      tryCatch({
        flit <- trimws(readLines(uri))
        flit <- flit[flit != ""]
        x <- read.csv(header=FALSE, skip=skip, text=flit, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})(\\d{2})"
      dateMatches <- str_match(x[[1]], re)
      yearValue <- as.numeric(dateMatches[, 2L])
      monthValue <- as.numeric(dateMatches[, 3L])

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, check.names=FALSE, stringsAsFactors=FALSE)
      #flit <- x[, -1]; colnames(flit) <- paste(series, capwords(colnames(flit)))
      flit <- x[, -1]; colnames(flit) <- paste(series, c("Global", "European"))
      d <- cbind(d, flit)

      return (d)
    })(path),

    `ERA-Interim 2m Global` =,
    `ERA5 2m Global` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header = FALSE, as.is = TRUE, skip = skip, check.names = FALSE)
      }, error = Error, warning = Error)

      flit <- reshape2::melt(x, id.vars = "V1", variable.name = "month", value.name = "temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year = flit$V1, yr_part = flit$V1 + (2 * flit$month - 1)/24, month = flit$month, temp = flit$temp, check.names = FALSE, stringsAsFactors = FALSE)
      ## Missing values are given as "-999.9".
      is.na(d$temp) <- d$temp == -999.9

      return (d)
    })(path),

    `NCEI Ocean Heat Content` = (function(p) {
      ma <- c("3month", "pentad")
      basins <- c(a="Atlantic", i="Indian", p="Pacific", w="Global")
      depths <- "0-" %_% c(700, 2000) %_% "m"
      quarts <- c("1-3", "4-6", "7-9", "10-12")

      threeMonthCombs <- expand.grid(ma[1] %_% "/h22-", names(basins), depths, quarts, stringsAsFactors=FALSE)
      threeMonthFiles <- apply(cbind(p, as.matrix(threeMonthCombs), ".dat"), 1, paste0, collapse="")
      flit <- threeMonthCombs[, 2:3]; colnames(flit) <- c("basin", "depth"); flit$basin <- basins[flit$basin]
      threeMonthFiles <- cbind(url=threeMonthFiles, flit, stringsAsFactors=FALSE)

      pentadCombs <- expand.grid(ma[2] %_% "/pent_h22-", names(basins), depths, stringsAsFactors=FALSE)
      pentadFiles <- apply(cbind(p, as.matrix(pentadCombs), ".dat"), 1, paste0, collapse="")
      flit <- pentadCombs[, 2:3]; colnames(flit) <- c("basin", "depth"); flit$basin <- basins[flit$basin]
      pentadFiles <- cbind(url=pentadFiles, flit, stringsAsFactors=FALSE)

      skip <- 0

      ## Quarterly OHC
      l1 <- plyr::dlply(threeMonthFiles, .(basin, depth),
        function(a) {
          l <- alply(a, 1,
            function(aa) {
              tryCatch({
                x <- read.table(aa$url, header=TRUE, skip=skip, check.names=FALSE)
              }, error=Error, warning=Error)

              x
            }
          )

          l <- plyr::arrange(Reduce(rbind, l), YEAR)
          ## Columns 3, 5, 7 are 1 × sigma, so 1.96 × sigma is a 95% CI.
          l_ply(c(3, 5, 7), function(a) l[[a]] <<- 1.96 * l[[a]])
          flit1 <- paste("NCEI", a$basin[1], "Ocean Heat Content", a$depth[1]) %_% c("", " NH", " SH")
          flit2 <- flit1 %_% "_uncertainty"
          colNames <- c("yr_part", c(flit1, flit2)[order(c(seq_along(flit1), seq_along(flit2) + 0.5))])
          colnames(l) <- colNames

          r <- range(trunc(l$yr_part))
          flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
          flit$yr_part <- flit$year + (2 * flit$month - 1)/24

          l <- merge(flit, l, by="yr_part", all=TRUE)

          l
        }
      )

      ## Pentadal OHC
      l2 <- plyr::dlply(pentadFiles, .(basin, depth),
        function(a) {
          l <- alply(a, 1,
            function(aa) {
              tryCatch({
                x <- read.table(aa$url, header=TRUE, skip=skip, check.names=FALSE)
              }, error=Error, warning=Error)

              x
            }
          )

          l <- plyr::arrange(Reduce(rbind, l), YEAR)
          ## Columns 3, 5, 7 are 1 × sigma, so 1.96 × sigma is a 95% CI.
          l_ply(c(3, 5, 7), function(a) l[[a]] <<- 1.96 * l[[a]])
          flit1 <- paste("NCEI", a$basin[1], "Ocean Heat Content", a$depth[1]) %_% c("", " NH", " SH") %_% " (Pentadal)"
          flit2 <- flit1 %_% "_uncertainty"
          colNames <- c("yr_part", c(flit1, flit2)[order(c(seq_along(flit1), seq_along(flit2) + 0.5))])
          colnames(l) <- colNames

          r <- range(trunc(l$yr_part))
          flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
          flit$yr_part <- flit$year + (2 * flit$month - 1)/24
          flit <- tbl_dt(flit)
          data.table::setkey(flit, yr_part)

          m <- tbl_dt(l)
          m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
          m <- dplyr::full_join(flit, m, by=c("year", "month"))

          as.data.frame(m)
        }
      )

      d <- c(l1, l2)

      return (d)
    })(path)
  )

  if (is.null(d) && verbose) tryCatch(message(""), message=Error)
  else if (verbose) { cat("Done.", fill=TRUE); flush.console() }

  if (!is.null(d)) {
    if (!is.data.frame(d)) {
      d <- Reduce(merge_fun_factory(by=c(Reduce(intersect, c(list(common_columns), lapply(d, names)))), all=TRUE), d)
    }

    if (any(duplicated(d[, c("year", "month"), drop=FALSE]))) stop("Data set has duplicated year/month rows.")

    climeNames <- !grepl("^yr_|^met_|^year|^month|_uncertainty$", names(d), d)
    d[, climeNames] <- as.data.frame(apply(d[, climeNames, drop=FALSE], 2, # Weird results without 'as.data.frame()' here.
      function(x)
      {
        if (!is.null(baseline)) {
          useBaseline <- TRUE
          if (is.logical(baseline)) {
            if (baseline)
              baseline <<- defaultBaseline
            else {
              useBaseline <- FALSE
              base <- rep(0.0, length(x))
              baseline <<- NULL
            }
          }

          if (useBaseline) {
            ## Calculate monthly average temperatures over baseline period.
            flit <- x[d$year %in% baseline]
            bma <- tapply(flit, d$month[d$year %in% baseline], mean, na.rm=TRUE)
            base <- NA
            null <- sapply(names(bma), function(x) { base[d$month == x] <<- bma[x] }); null <- NULL
          }
        }
        else
          base <- rep(0.0, length(x))

        ## Center anomalies on average baseline-period temperatures.
        anom <- round(x - base, 4L)

        anom
      }
    ))

    if (sum(climeNames, na.rm=TRUE) == 1L && names(d)[climeNames] == "temp")
      d[[series]] <- d$temp
    d$met_year <- NA

    d <- subset(d, na_unwrap(d[, !grepl("^yr_|^met_|^year|^month|_uncertainty$", names(d), d)]))
    attr(d, "baseline") <- baseline
  }

  return (d)
}


DownloadInstrumentalData <- function(paths, baseline=TRUE, verbose, dataDir, filenameBase)
{
  env <- new.env()

  for (series in names(paths)) {
    d <- ReadAndMungeInstrumentalData(series, paths[[series]], baseline=FALSE, verbose=verbose)
    if (!is.null(d))
      env[[series]] <- d
  }

  d <- NULL
  for (i in ls(env)) {
    uncertainty <- grep("_uncertainty$", names(env[[i]]), value=TRUE)
    climeNames <- names(env[[i]])[!grepl("^yr_|^met_|^year|^month|_uncertainty$|^temp$", names(env[[i]]))]
    if (is.null(d))
      d <- env[[i]][, c(common_columns, climeNames, uncertainty)]
    else
      d <- merge(d, env[[i]][, c(common_columns, climeNames, uncertainty)], by=common_columns, all=TRUE)
  }

  # attr(d, "baseline") <- NULL
  # if (length(env) > 0L)
  #   attr(d, "baseline") <- attr(env[[ls(env)[1L]]], "baseline")

  d <- make_met_year(d)

  suffix <- format(Sys.Date(), "%Y%m%d")

  tempPath <- paste(dataDir, filenameBase %_% "raw_" %_% suffix, sep="/")
  save(d, file=tempPath %_% ".RData")
  write.csv(d, file=tempPath %_% ".csv", row.names=FALSE)

  d <- recenter_anomalies(d, defaultBaseline)

  tempPath <- paste(dataDir, filenameBase %_% suffix, sep="/")
  save(d, file=tempPath %_% ".RData")
  write.csv(d, file=tempPath %_% ".csv", row.names=FALSE)

  return (d)
}


#' @export
make_met_year <- function(x, add = TRUE)
{
  met_year <- shift(x[, "year"], -1L, roll = FALSE)
  metRow <- which(is.na(met_year))
  met_year[metRow] <- max(x[, "year"], na.rm = TRUE)
  if (x[, "month"][metRow] == 12)
    met_year[metRow] <- met_year[metRow] + 1
  if (add) {
    #return (cbind(x, met_year = met_year))
    x <- dataframe(x)
    x$met_year <- met_year

    return (x)
  }
  else
    return (met_year)
}


#' @export
make_yr_part <- function(x, add = TRUE)
{
  #yr_part <- x[, "year"] + (x[, "month"] - 0.5) / 12
  yr_part <- x[, "year"] + (2 * x[, "month"] - 1) / 24

  if (add)
    return (cbind(x, yr_part = yr_part))
  else
    return (yr_part)
}


LoadInstrumentalData <- function(dataDir, filenameBase, baseline=NULL)
{
  d <- NULL

  getDataFilenames <- function(dataDir, fileExtension)
  {
    sort(grep("^.*?" %_% filenameBase %_% ifelse(is.null(baseline) || (is.logical(baseline) && !baseline), "raw_", "") %_% "\\d{8}" %_% "\\." %_% fileExtension %_% "$", list.files(dataDir, full.names=TRUE), value=TRUE), decreasing=TRUE)
  }

  fileExtension <- "RData"
  filenames <- getDataFilenames(dataDir, fileExtension)
  fileFound <- FALSE
  if (length(filenames) != 0) {
    cat("Loading file ", basename(filenames[1L]), "...", sep="")
    load(filenames[1L], envir=environment()) # File extension "RData".
    cat(". Done.", fill=TRUE); flush.console()
    #d <- dget(filenames[1L]) # File extension "dput".
    #d <- read.csv(filenames[1L]) # File extension "csv".

    fileFound <- TRUE
  }
  else {
    testDirs <- "."
    if (!is.null(getOption("climeseries_data_dir")))
      testDirs <- c(testDirs, getOption("climeseries_data_dir"))
    testDirs <- c(testDirs, system.file("extdata", "latest", package="climeseries"))

    for (i in testDirs) {
      filenames <- getDataFilenames(i, fileExtension)
      if (length(filenames) != 0) {
        cat("Loading file ", basename(filenames[1L]), "...", sep="")
        load(filenames[1L], envir=environment())
        cat(". Done.", fill=TRUE); flush.console()
        fileFound <- TRUE
        break
      }
    }

    if (!fileFound)
      stop("No 'climeseries' data sets found.")
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
#'   \item{HadCRUT4}{UK Met Office Hadley Centre global average combined land+SST temperature anomaly.}
#'   \item{HadCRUT4 NH}{UK Met Office Hadley Centre northern hemisphere average combined land+SST temperature anomaly.}
#'   \item{HadCRUT4 SH}{UK Met Office Hadley Centre southern hemisphere average combined land+SST temperature anomaly.}
#'   \item{HadCRUT4 Tropics}{UK Met Office Hadley Centre tropics (30S–30N latitude) average combined land+SST temperature anomaly.}
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
#'
#' ## Load both centered and "raw" data.
#'
#' d <- get_climate_data(download=FALSE, baseline=TRUE)
#' e <- get_climate_data(download=FALSE, baseline=FALSE)
#'
#' ## Which year is the warmest?
#'
#' inst <- get_climate_data(download=FALSE, baseline=TRUE)
#' series <- setdiff(names(inst), c(climeseries::common_columns, c("CO2 Mauna Loa")))
#' yearType <- "year" # "year" or "met_year" = meteorological year.
#' annual <- sapply(series, function(s) { rv <- tapply(inst[[s]], inst[[yearType]], mean, na.rm=TRUE); rv <- rv[!is.nan(rv)]; rv })
#'
#' ## How many months for last year have data?
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
get_climate_data <- function(download, data_dir, filename_base, urls=climeseries::data_urls, omit=omitUrlNames, only=NULL, baseline=TRUE, annual_mean=FALSE, verbose=TRUE)
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

  if (download) {
    if (!is.null(only))
      urls <- urls[names(urls) %in% only]
    if (!is.null(omit))
      urls <- urls[!names(urls) %in% omit]
    d <- DownloadInstrumentalData(urls, baseline, verbose, data_dir, filename_base)
  }
  else
    d <- LoadInstrumentalData(data_dir, filename_base, baseline)

  if (annual_mean) {
    seriesNames <- get_climate_series_names(d, conf_int=FALSE)
    temp <- aggregate(d[, seriesNames], list(year=d$year), mean, na.rm=TRUE)
    temp <- data.matrix(temp)
    is.na(temp) <- is.nan(temp)
    d <- as.data.frame(temp)
  }

  return (d)
}

## Get column names for including in the README file:
# cat("1. " %_% names(e), sep="\n")
## Get count of individual data sets:
# length(get_climate_series_names(e))


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
get_climate_series_names <- function(x, conf_int=FALSE, invert=TRUE)
{
  colNames <- colnames(x)

  return (colNames[grep("(^yr_|^met_|^year|^month" %_% ifelse(conf_int, "", "|_uncertainty$") %_% ")", colNames, invert=invert)])
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
recenter_anomalies <- function(x, baseline=defaultBaseline, digits=4L, by_month=TRUE, return_baselines_only=FALSE, ...)
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
      ## N.B. This next step is both a time & memory sink, the latter being more problematic; optimize it!
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
make_time_series_from_anomalies <- function(x, frequency = 12L, offset = 0.5, ...)
{
  if (is.ts(x))
    return (x)

  startTime <- switch(as.character(frequency),
    `1` = (function() {
      if (is.matrix (x))
        r <- min(x[, "year"], na.rm = TRUE)
      else
        r <- min(x$year, na.rm = TRUE)

      r
    })(),

    `12` = (function() {
      r <- unlist(x[1L, c("year", "month")])
      r[2] <- r[2] + offset

      r
    })()
  )

  s <- ts(x, start = startTime, frequency = frequency)

  return (s)
}


## Correctly 'window()' time series on standard 'yr_part' values.
#' @export
window_ts <- function(x, start = NULL, end = NULL, ...)
{
  z <- window_default(x, start, end, ...)
  z[, "yr_part"] <- time(z)

  z
}
