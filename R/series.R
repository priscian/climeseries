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
        r <- getURL(p, curl=curl)
        x <- read.csv(text=r, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
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
        r <- getURL(p, curl=curl)
        x <- read.csv(text=r, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
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

      skip <- 3L # Changed from 2 (22 Mar. 2016).
      if (type == "drought")
        skip <- 2L

      tryCatch({
        x <- read.csv(p, header=TRUE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})(\\d{2})"
      yearMatches <- str_match(x[[1]], re) # Column name "Year" or "Date".
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(yearMatches[, 3L])

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$Value, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
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
      d[[series %_% "_uncertainty"]] <- x$V12 - x$V11

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

    `RSS TLT 3.3 Land` =,
    `RSS TLT 3.3 Ocean` =,
    `RSS TLT 3.3` =,
    `RSS TMT 3.3 Land` =,
    `RSS TMT 3.3 Ocean` =,
    `RSS TMT 3.3` =,
    `RSS TMT 4.0 Land` =,
    `RSS TMT 4.0 Ocean` =,
    `RSS TMT 4.0` = (function(p) {
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

    `UAH TMT 5.6` =,
    `UAH TLT 5.6` =,
    `UAH TMT 6.0` =,
    `UAH TLT 6.0` = (function(p) {
      x <- NULL

      skip <- 1L

      tryCatch({
        flit <- readLines(p)
        flit <- flit[trimws(flit) != ""]
        flit <- split_at(flit, which(duplicated(flit)))[[1]]
        x <- read.table(header=FALSE, skip=skip, text=flit, check.names=FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.matrix(x[, -(1:2)])
      ## There are no missing values in these data sets, and no missing-value value is given.
      #is.na(flit) <- flit == -99.99
      colParts <- list(c("Global", "NH", "SH", "Tropics", "NH Extratropic", "SH Extratropic", "NH Polar", "SH Polar"), c("", " Land", " Ocean"))
      colNames <- as.vector(t(outer(colParts[[1]], colParts[[2]], paste, sep=""))) # 't()' preserves the order.
      colNames <- paste(series, c(colNames, "USA 48", "USA 48 + Alaska", "Australia"))
      colnames(flit) <- colNames
      d <- cbind(d, flit)

      return (d)
    })(path),

    `RATPAC-A` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- readLines(p)
        re <- "^\\s+(\\d{1})"
        pressureLevels <- trimws(grep(re, flit, value=TRUE, perl=TRUE))
        flit <- sub(re, "#\\1", flit)
        flit <- gsub("\t", " ", flit) # Remove some curious tabs in the headers.
        #flit <- sub("^\\s+", "", flit)
        flit <- split_at(flit, grep("^#", flit, perl=TRUE))
        y <- lapply(flit,
          function(x)
          {
            r <- read.fortran(tc <- textConnection(x), format=c("2I4", "7F7.0"), header=TRUE, skip=skip, check.names=FALSE, comment.char="#"); close(tc)
            ## Since the headers are read in badly, replace them outright.
            names(r) <- c("year", "season", "NH", "SH", "Global", "Tropics", "NH Extratropic", "SH Extratropic", "20N-S")
            r
          }
        )
      }, error=Error, warning=Error)

      d <- mapply(pressureLevels, y,
        FUN = function(l, y)
        {
          y$month <- (y$season * 3) - 2 # V. http://www1.ncdc.noaa.gov/pub/data/ratpac/readme.txt
          flit <- expand.grid(month=1:12, year=unique(y$year))
          flit <- merge(flit, y, by=c("year", "month"), all.x=TRUE)
          d <- data.frame(year=flit$year, yr_part=flit$year + (2 * flit$month - 1)/24, month=flit$month, check.names=FALSE, stringsAsFactors=FALSE)
          flit <- data.matrix(flit[, -(1:3)])
          is.na(flit) <- flit == 999.000
          d <- cbind(d, flit)
          names(d)[!(names(d) %in% commonColumns)] <- paste("RATPAC-A", l, names(d)[!(names(d) %in% commonColumns)])

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
            for (i in c("N", "S")) {
              flit <- readLines(paste(p, x, paste(i, y, "area_v2.txt", sep="_"), sep="/"))
              flit <- read.table(text=flit[!grepl("^\\s+", flit)], header=TRUE, check.names=FALSE)
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
        names(y[[i]])[grep(re, n, perl=TRUE)] <- paste(series, capwords(sub(re, i %_% " \\1", n[grep(re, n, perl=TRUE)])))
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

    `TSI Reconstructed` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, comment.char=";", check.names=FALSE)
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
        x <- read.csv(p, header=FALSE, skip=skip, check.names=FALSE, na.strings="#N/A")
      }, error=Error, warning=Error)

      re <- "(\\d{4})\\.(\\d{3})"
      yearMatches <- str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `Antarctica Land Ice Mass Variation` =,
    `Greenland Land Ice Mass Variation` = (function(p) {
      ## N.B. This is very hacky, scraping the JSON from NASA's Web page, but it will eventually be replaced by averaged gridded data.
      parsedUrl <- strsplit(p, "\\?")[[1]]
      p <- parsedUrl[1]; land <- parsedUrl[2]

      curl <- getCurlHandle()
      curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        r <- getURLContent(p, curl=curl)
        x <- grep("createData\\(\"LandIce" %_% land, strsplit(r, "\n")[[1]], value=TRUE, perl=TRUE)
        flit <- fromJSON(sub("^.+?createData\\(\"LandIce" %_% land %_% "\",(\\[.+?\\]).+?$", "\\1", x, perl=TRUE))
      }, error=Error, warning=Error)

      flit <- as.data.frame(data.matrix(flit[, c("year", "month", "y", "y_margin_max", "y_margin_min")]))

      d <- data.frame(year=flit$year, yr_part=flit$year + (2 * flit$month - 1)/24, month=flit$month, temp=flit$y, check.names=FALSE, stringsAsFactors=FALSE)
      d[[series %_% "_uncertainty"]] <- flit$y_margin_max - flit$y_margin_min

      ## For multiple readings in a single month, keep only the first reading. (I have to sacrifice either the value or the error.)
      d <- subset(d, !duplicated(d[, c("year", "month")]))

      return (d)
    })(path),
  )

  if (is.null(d) && verbose) tryCatch(message(""), message=Error)
  else if (verbose) { cat("Done.", fill=TRUE); flush.console() }

  if (!is.null(d)) {
    if (!is.data.frame(d))
      d <- Reduce(merge_fun_factory(by=c(Reduce(intersect, c(list(commonColumns), lapply(d, names))))), d)

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
        anom <- round(x - base, 3L)

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
    climeNames <- names(e[[i]])[!grepl("^yr_|^met_|^year|^month|_uncertainty$|^temp$", names(e[[i]]))]
    if (is.null(d))
      d <- e[[i]][, c(commonColumns, climeNames, uncertainty)]
    else
      d <- merge(d, e[[i]][, c(commonColumns, climeNames, uncertainty)], by=commonColumns, all=TRUE)
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
#' series <- setdiff(names(inst), c(climeseries:::commonColumns, c("CO2 Mauna Loa")))
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
