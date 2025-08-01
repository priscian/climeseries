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
    `GISTEMP v3 SH Land` =,
    `GISTEMP v3 NH Land` =,
    `GISTEMP v3 Global Land` =,
    `GISTEMP v3 SH` =,
    `GISTEMP v3 NH` =,
    `GISTEMP v3 Global` =,
    `GISTEMP v4 SH Land` =,
    `GISTEMP v4 NH Land` =,
    `GISTEMP v4 Global Land` =,
    `GISTEMP v4 SH` =,
    `GISTEMP v4 NH` =,
    `GISTEMP v4 Global` = (function(p) { # GISS .TXT files are weirdly formatted, so use the .CSVs instead.
      x <- NULL

      skip <- 1L

      ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
      ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
      curl <- RCurl::getCurlHandle()
      RCurl::curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        #r <- RCurl::getURL(p, curl=curl)
        #x <- read.csv(text=r, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
        x <- read.csv(p, header=TRUE, as.is=TRUE, na.strings=c("***", "****", "*****"), skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x[, 1L:13L], id.vars="Year", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, Year, month)

      d <- data.frame(year=flit$Year, yr_part=flit$Year + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `GISTEMP v3 Zonal` =,
    `GISTEMP v3 Zonal Land` =,
    `GISTEMP v4 Zonal` =,
    `GISTEMP v4 Zonal Land` = (function(p) {
      x <- NULL

      skip <- 0L

      ## N.B. GISS blocks HTTP/1.0 requests, so use package "RCurl". V. discussion at:
      ## http://wattsupwiththat.com/2014/07/05/giss-is-unique-now-includes-may-data/
      curl <- RCurl::getCurlHandle()
      RCurl::curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        #r <- RCurl::getURL(p, curl=curl)
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
      yearMatches <- stringr::str_match(x[[1]], re) # Column name "Year" or "Date".
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


    `ClimDiv Min` =,
    `USCRN Min` =,
    `ClimDiv Max` =,
    `USCRN Max` =,
    `ClimDiv Avg` =,
    `USCRN Avg` = (function(p) {
      x <- NULL

      skip <- 1L

      tryCatch({
        x <- read.csv(p, header = TRUE, skip = skip, fill = TRUE, check.names = FALSE)
      }, error = Error, warning = Error)

      re <- "(\\d{4})(\\d{2})"
      yearMatches <- stringr::str_match(x$Date, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(yearMatches[, 3L])

      ## Get correct column name
      temp_name <- trimws(strsplit(series, " ")[[1]][1])

      d <- data.frame(year = yearValue, yr_part = yearValue + (2 * monthValue - 1)/24,
        month = monthValue, temp = x[[temp_name]], check.names = FALSE, stringsAsFactors = FALSE)

      is.na(d$temp) <- d$temp == -99.99
      ## Convert °F anomalies to °C
      d$temp <- d$temp/1.8

      return (d)
    })(path),

    `STAR v5.0` = (function(p) {
      skip <- 0L

      httr::set_config(httr::config(ssl_verifypeer = 0L))
      tryCatch({
        #fileNames <- strsplit(RCurl::getURL(p, dirlistonly = TRUE), "\r*\n")[[1L]]# %>% stringr::str_subset("_Merged")
        fileNames <- httr::content(httr::GET(p), as = "text") %>%
          stringr::str_extract_all("NESDIS-STAR.*?\\.txt", simplify = FALSE) %>% `[[`(1) %>% unique
        dd <- sapply(fileNames,
          function(a)
          {
            level <- stringr::str_match(a, "_(TLS|TLT|TMT|TUT)_")[, 2]
            x <- read.table(p %_% a, header = TRUE, skip = skip, fill = TRUE, check.names = FALSE)
            d <- x %>% dplyr::select(year = Year, month = Month, ends_with("_Anomaly")) %>%
              dplyr::rename_with(function(b) { stringr::str_replace_all(b, "(_|Anomaly)", " ") %>% stringr::str_trim() %>%
                sprintf("STAR v5.0 %s %s", level, .) }, .cols = ends_with("_Anomaly"))

            d
          }, simplify = FALSE) %>%
          purrr::reduce(dplyr::full_join, by = c("year", "month")) %>% dplyr::arrange(year, month) -> d
      }, error = Error, warning = Error)

      ## Make TTT series
      a24 <- 1.15 # TTT calculations given in Zou &al 2023 dx.doi.org/10.1029/2022JD037472
      TMT <- dd[, stringr::str_detect(colnames(dd), " TMT ")] %>% data.matrix
      TLS <- dd[, stringr::str_detect(colnames(dd), " TLS ")] %>% data.matrix
      TTT <- (a24 * TMT + (1 - a24) * TLS) %>% as.data.frame %>%
        `names<-`(stringr::str_replace(names(.), " (TMT|TLS) ", " TTT "))

      d %<>% dplyr::bind_cols(TTT) %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24, .after = "month")

      return (d)
    })(path),

    #`ERSSTv5` =,
    `NCEI v4` = (function(p) {
      skip <- 0L

      tryCatch({
        #fileNames <- strsplit(RCurl::getURL(p, dirlistonly=TRUE), "\r*\n")[[1L]]
        fileNames <- httr::content(httr::GET(p), as = "text") %>%
          stringr::str_extract_all("aravg.*?\\.asc", simplify = FALSE) %>% `[[`(1) %>% unique
        ## Keep only monthly data files.
        re <- "^.*?aravg\\.mon\\.(?<type>.+?)\\.(?<lat1>.+?)\\.(?<lat2>.+?)\\..*?\\.asc$"
        fileNames <- grep(re, fileNames, value=TRUE, perl=TRUE)
        m <- regexpr(re, fileNames, perl=TRUE)
        namedMatches <- cbind(file=fileNames, parse_one(fileNames, m))
        namedMatches[, "type"] <- capwords(sub("_", " + ", namedMatches[, "type"]))
        cat(fill=TRUE)
        l <- apply(namedMatches, 1,
          function(y)
          {
            seriesName <- paste("NCEI v4", y["type"], y["lat1"] %_% "-" %_% y["lat2"])
            cat("    Processing file", y["file"], fill=TRUE); flush.console()
            x <- read.table(p %_% y["file"], header=FALSE, skip=skip, fill=TRUE, check.names=FALSE)
            #d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, conf_int=2 * 1.96 * sqrt(x$V4), check.names=FALSE, stringsAsFactors=FALSE)
            ## N.B. See ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/timeseries/readme.timeseries for "total error variance."
            d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
            names(d)[names(d) == "temp"] <- seriesName
            #names(d)[names(d) == "conf_int"] <- seriesName %_% "_uncertainty"

            d
          }
        )
      }, error=Error, warning=Error)

      return (l)
    })(path),

    `CRUTEM3 Global` =,
    `CRUTEM3 NH` =,
    `CRUTEM3 SH` =,
    `CRUTEM3v Global` =,
    `CRUTEM3v NH` =,
    `CRUTEM3v SH` =,
    `HadCRUT3 Global` =,
    `HadCRUT3 NH` =,
    `HadCRUT3 SH` =,
    `HadCRUT3v Global` =,
    `HadCRUT3v NH` =,
    `HadCRUT3v SH` =,
    `HadSST2 Global` =,
    `HadSST2 NH` =,
    `HadSST2 SH` =,
    `CRUTEM4 Global` =,
    `CRUTEM4 NH` =,
    `CRUTEM4 SH` =,
    `CRUTEM4v Global` =,
    `CRUTEM4v NH` =,
    `CRUTEM4v SH` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read_cru_hemi(p)
      }, error = Error, warning = Error)

      flit <- x %>% dplyr::select(tidyselect::matches("^(year|month)"))
      flit <- reshape2::melt(flit[, 1L:13L], id.vars = "year", variable.name = "month", value.name = "temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      d <- dplyr::arrange(flit, year, month)
      d <- d %>% dplyr::mutate(yr_part = d$year + (2 * d$month - 1)/24)

      d
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
      yearMatches <- stringr::str_match(x$V1, re)
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

    `CRUTEM5 Global` =,
    `CRUTEM5 NH` =,
    `CRUTEM5 SH` =,
    `HadCRUT5 Global` =,
    `HadCRUT5 SH` =,
    `HadCRUT5 NH` =,
    `HadCRUT5 Global (not infilled)` =,
    `HadCRUT5 SH (not infilled)` =,
    `HadCRUT5 NH (not infilled)` = (function(p) {
      x <- NULL

      tryCatch({
        flit <- tempfile()
        download.file(p, flit, mode = "wb", quiet = TRUE)
        n <- ncdf4::nc_open(flit) # 'print(n)' or just 'n' for details.
        tas_var <- "tas"
        if (names(n$var) %>% stringr::str_detect("tas_mean") %>% any)
          tas_var <- "tas_mean"
        a <- ncdf4::ncvar_get(n, tas_var)
        u <- ncdf4::ncvar_get(n, "tas_upper")
        l <- ncdf4::ncvar_get(n, "tas_lower")
        times <- ncdf4::ncvar_get(n, "time")
        tunits <- ncdf4::ncatt_get(n,"time", "units")
        ncdf4::nc_close(n)
      }, error = Error, warning = Error)

      # [1] "days since 1850-1-1 00:00:00"
      dtimes <- as.Date(times, origin = "1850-01-01")
      ## This data set should have the same length as the "time" dimension of 'a':
      d <- dataframe(year = year(dtimes), yr_part = year(dtimes) + (2 * month(dtimes) - 1)/24, month = month(dtimes), temp = a)
      ## See "95% confidence intervals" here: https://crudata.uea.ac.uk/~timo/diag/tempdiag.htm
      ## Divide by 2 to match those Met Office plots (but is this correct for HadCRUT5?):
      d[[series %_% "_uncertainty"]] <- (u - l)/2 # Was: 'u - l'

      return (d)
    })(path),


    `HadSST4 SH` =,
    `HadSST4 NH` =,
    `HadSST4 Tropics` =,
    `HadSST4 Global` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- rio::import(p, fread = FALSE, header = TRUE, as.is = TRUE, na.strings = c("-99.9", "-99.99"), skip = skip, check.names = FALSE, stringsAsFactors = FALSE)
      }, error=Error, warning=Error)

      d <- data.frame(year = x$year, yr_part = x$year + (2 * x$month - 1)/24, month = x$month, temp = x$anomaly, check.names = FALSE, stringsAsFactors = FALSE)

      #d[[series %_% "_uncertainty"]] <- x[[10]] - x[[9]]
      ## Make sure 95% CIs here match those in plots at https://crudata.uea.ac.uk/cru/data/temperature/
      d[[series %_% "_uncertainty"]] <- 2 * 1.96 * x$total_uncertainty

      return (d)
    })(path),

    `HadCET` = (function(p) {
      x <- NULL

      #skip <- 7L
      skip <- 5L

      tryCatch({
        x <- read.table(p, header=FALSE, as.is=TRUE, na.strings=c("-99.9", "-99.99"), skip=skip, check.names=FALSE, stringsAsFactors=FALSE)
      }, error=Error, warning=Error)

      flit <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
      flit <- dplyr::arrange(flit, V1, month)

      d <- data.frame(year=flit$V1, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `Cowtan & Way Krig. Global` =,
    `Cowtan & Way Krig. Global Land` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header=FALSE, skip=skip, check.names=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})\\.(\\d{3})"
      yearMatches <- stringr::str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)
      ## Add value of 95% total uncertainty to data frame.
      ## 'x$V3' is 1 × sigma, so 1.96 × sigma is a 95% CI. V. http://www-users.york.ac.uk/~kdc3/papers/coverage2013/series.html for details, http://www.skepticalscience.com/kevin_cowtan_agu_fall_2014.html for a plot with uncertainty bands.
      if (NCOL(x) > 2)
        d[[series %_% "_uncertainty"]] <- 2 * 1.96 * x$V3

      return (d)
    })(path),

    `BEST Gridded` = (function(p) {
      nh <- create_zonal_data(x = NULL, what = "be", sub_lat = c(0, 90), use_local = TRUE) %>%
        dplyr::select(-yr_part)
      sh <- create_zonal_data(x = NULL, what = "be", sub_lat = c(-90, 0), use_local = TRUE) %>%
        dplyr::select(-yr_part)

      d <- dplyr::full_join(nh, sh, by = c("year", "month")) %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24, .after = "month") %>%
        dplyr::arrange(year, month)

      d
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
        httr::set_config(httr::config(ssl_verifypeer = 0L)) # This is necessary for the BE data on port 4443
        flit <- readLines(tc <- textConnection(httr::content(httr::GET(p), "text", encoding="ISO-8859-1"))); close(tc)
        x <- read.table(text = flit, header=FALSE, skip=skip, check.names=FALSE, comment.char="%")
        dev_null <- sapply(names(x), function(y) is.na(x[[y]]) <<- is.nan(x[[y]]))
      }, error=Error, warning=Error)

      dupIndex <- duplicated(x[, c("V1", "V2")])
      if (any(dupIndex)) {
        if (grepl("Land_and_Ocean_complete", path)) { # The Berkeley Earth Land + Ocean data set.
          ## "with Sea Ice Temperature Inferred from Air Temperatures"
          y <- x[!dupIndex, ]
          d1 <- data.frame(year=y$V1, yr_part=y$V1 + (2 * y$V2 - 1)/24, month=y$V2, `BEST Global (Air Ice Temp.)`=y$V3, check.names=FALSE, stringsAsFactors=FALSE)
          d1Series <- grep("^yr_|^met_|^year|^month|_uncertainty$", names(d1), value=TRUE, invert=TRUE)
          d1[[d1Series %_% "_uncertainty"]] <- 2 * y$V4

          ## "with Sea Ice Temperature Inferred from Water Temperatures"
          y <- x[dupIndex, ]
          d2 <- data.frame(year=y$V1, yr_part=y$V1 + (2 * y$V2 - 1)/24, month=y$V2, `BEST Global (Water Ice Temp.)`=y$V3, check.names=FALSE, stringsAsFactors=FALSE)
          d2Series <- grep("^yr_|^met_|^year|^month|_uncertainty$", names(d2), value=TRUE, invert=TRUE)
          d2[[d2Series %_% "_uncertainty"]] <- 2 * y$V4

          d <- list(d1, d2); names(d) <- c(d1Series, d2Series)
        }
        else
          stop("Unknown data set.")
      }
      else {
        d <- data.frame(year=x$V1, yr_part=x$V1 + (2 * x$V2 - 1)/24, month=x$V2, temp=x$V3, check.names=FALSE, stringsAsFactors=FALSE)
        ## "Uncertainties represent the 95% confidence interval for statistical and spatial undersampling effects as well as ocean biases." From http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt.
        d[[series %_% "_uncertainty"]] <- 2 * x$V4
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
        comments <- stringr::str_extract(readLines(con), "^#.*$")
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
              stemp <- stringr::str_match(y[lineNo], tempRe)[2L]
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
    `RSS TLS 4.0 Land` =,
    `RSS TLS 4.0 Ocean` =,
    `RSS TLS 4.0` =,
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
    `RSS TTS 4.0 Land` =,
    `RSS TTS 4.0 Ocean` =,
    `RSS TTS 4.0` =,
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
        httr::GET(p, httr::write_disk(tempFile, overwrite = TRUE))
        #download.file(p, tempFile, quiet=TRUE)
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
        #download.file(p, tempFile, quiet=TRUE)
        httr::GET(p, httr::write_disk(tempFile, overwrite = TRUE))
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
          flit <- data.matrix(flit[, -(1:2)])
          is.na(flit) <- flit == 999.000
          d <- cbind(d, flit)
          names(d)[!(names(d) %in% common_columns)] <- paste("RATPAC-A", names(d)[!(names(d) %in% common_columns)], "mb", l)
          names(d) <- sub("surf mb", "Surface", names(d))

          d
        }, SIMPLIFY = FALSE
      )

      return (d)
    })(path),

    `NCEP/NCAR Surface Air SH` =,
    `NCEP/NCAR Surface Air SH Polar` =,
    `NCEP/NCAR Surface Air NH` =,
    `NCEP/NCAR Surface Air NH Polar` =,
    `NCEP/NCAR Surface Air Tropics` =,
    `NCEP/NCAR Surface Air USA 48` =,
    `NCEP/NCAR Surface Air Global` = (function(p) {
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

    `AIRS Surface Skin Global` = (function(p) {
      d <- create_combined_airs_series(baseline = 2003:2018, series = series)

      return (d)
    })(path),

    `AIRS SH` =,
    `AIRS NH` =,
    `AIRS Global` = (function(p) {
      x <- NULL

      skip <- 1L

      tryCatch({
        r <- httr::GET(p) %>% httr::content(as = "text")
        coverage <- (r %>% stringr::str_split("\\n"))[[1]][1] %>% stringr::str_match("^(.*?)\\s+.*$") %>% `[`(, 2)
        r <- r %>%
          stringr::str_split(stringr::regex(sprintf("^%s\\s+.*$", rex::escape(coverage)), multiline = TRUE)) %>%
          unlist %>%
          `[`(. != "") %>%
          structure(.Names = paste(c("AIRS v6", "AIRS v7", "GISTEMP v4"), coverage))
        x <- r %>% sapply(
          function(a)
          {
            read.csv(text = a, skip = 1L, as.is = TRUE, na.strings = c("*******"), check.names = FALSE)
          }, simplify = FALSE)
      }, error = Error, warning = Error)

      flit <- sapply(seq_along(x),
        function(a)
        {
          flit <- reshape2::melt(x[[a]][, 1L:13L], id.vars = "Year", variable.name = "month", value.name = names(x)[a])
          for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
          flit <- dplyr::arrange(flit, Year, month)
        }, simplify = FALSE)

      d <- flit %>%
        purrr::reduce(dplyr::full_join, by = c("Year", "month")) %>%
        dplyr::rename(year = "Year") %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24) %>%
        dplyr::relocate(yr_part, .after = month) %>%
        dplyr::select(!tidyselect::starts_with("GISTEMP"))

      return (d)
    })(path),

    `AIRS Zonal` = (function(p) {
      x <- NULL

      skip <- 1L

      tryCatch({
        r <- httr::GET(p) %>% httr::content(as = "text")
        coverage <- (r %>% stringr::str_split("\\n"))[[1]][1] %>% stringr::str_match("^(.*?)\\s+.*$") %>% `[`(, 2)
        r <- r %>%
          stringr::str_split(stringr::regex(sprintf("^%s\\s+.*$", rex::escape(coverage)), multiline = TRUE)) %>%
          unlist %>%
          `[`(. != "") %>%
          structure(.Names = paste(c("AIRS v6", "AIRS v7", "GISTEMP v4"), "Zonal"))
        x <- r %>% sapply(
          function(a)
          {
            read.csv(text = a, skip = 1L, as.is = TRUE, na.strings = c("*******"), check.names = FALSE)
          }, simplify = FALSE)
      }, error = Error, warning = Error)

      flit <- sapply(seq_along(x),
        function(a)
        {
          flit <- x[[a]][, -1]
          colnames(flit) <- paste(names(x)[a], colnames(flit))
          d <- cbind(dataframe(year = x[[a]]$Year, month = 6), flit)
          d <- base::merge(expand.grid(month = 1:12, year = d$year), d, by = c("year", "month"), all = TRUE)
          #d$yr_part <- d$year + (2 * d$month - 1)/24

          d
        }, simplify = FALSE)

      d <- flit %>%
        purrr::reduce(dplyr::full_join, by = c("year", "month")) %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24) %>%
        dplyr::relocate(yr_part, .after = month) %>%
        dplyr::select(!tidyselect::starts_with("GISTEMP"))

      return (d)
    })(path),

    `CO2 Mauna Loa` = (function(p) {
      x <- NULL

      skip <- 64L

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

      op <- options()
      ## -k means don't verify cert, -L means follow redirects (important for some servers):
      options(download.file.method = "curl", download.file.extra = "-k -L")
      tryCatch({
        temp <- tempfile()
        download.file(p, temp)
        flit <- readLines(temp)
        #flit <- flit[trimws(flit) != ""]
        #flit <- flit[grep("^\\d{4}", flit, perl=TRUE)] # Not as robust as 'stringr::str_detect()'
        flit <- flit[stringr::str_trim(lapply(flit, iconv, to = "UTF-8") %>% unlist(use.names = FALSE)) != ""] %>% `[`(!is.na(.))
        flit <- flit[stringr::str_detect(flit, "^\\d{4}")]
        x <- read.csv(header = FALSE, skip = 0L, text = flit, fill = TRUE, check.names = FALSE, na.strings = "NaN")
      }, error = Error, warning = Error, finally = options(op))

      d <- data.frame(year = x$V1, yr_part = x$V1 + (2 * x$V2 - 1)/24, month = x$V2, temp = x$V5,
        check.names = FALSE, stringsAsFactors = FALSE)
      ## 'x$V6' is 1 × sigma, so 1.96 × sigma is a 95% CI, I think.
      d[[series %_% "_uncertainty"]] <- 2 * 1.96 * x$V6

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

    `OSI Sea Ice` = (function(p) {
      areas <- strsplit(RCurl::getURL(p %_% "/", dirlistonly = TRUE), "\r*\n")[[1L]]
      uris <- sapply(areas,
        function(a)
        {
          paste(
            paste(p, a, sep = "/"),
            c(sprintf("osisaf_%s_sia_monthly.txt", a), sprintf("osisaf_%s_sie_monthly.txt", a)),
            sep = "/")
        }, simplify = FALSE, USE.NAMES = FALSE) %>% unlist

      flit <- sapply(uris,
        function(a)
        {
          flit <- readLines(a)
          flitHeader <- stringr::str_subset(flit, "^#")
          area <-
            stringr::str_match(flitHeader, stringr::regex("^# Area:\\s+(.*)$", ignore_case = TRUE)) %>%
              `[`(complete.cases(.))[2]
          quantity <-
            stringr::str_match(flitHeader, stringr::regex("^# Quantity:\\s+(.*)$", ignore_case = TRUE)) %>%
              `[`(complete.cases(.))[2]
          colName <- sprintf("EUMETSAT OSI %s %s", area, quantity)

          x <- read.table(tc <- textConnection(flit), comment = "#", na.strings = "-999"); close(tc)

          d <- dataframe(year = x$V2, month = x$V3, temp = x$V5/1e6) %>%
            dplyr::rename(!!colName := temp)
        }, simplify = FALSE)

      d <- purrr::reduce(flit, dplyr::full_join, by = c("year", "month")) %>%
        dplyr::arrange(year, month) %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24, .after = month)

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

      #f <- tempfile(fileext = ".zip")
      f <- tempfile(fileext = ".zip")
      tryCatch({
        utils::download.file(url = p, destfile = f, mode = "wb", quiet = TRUE)
        x0 <- readr::read_table(f, comment = ";", skip = 1L, na = "nan", col_names = FALSE)
        x <- x0 %>%
          dplyr::transmute(
            year = lubridate::year(X1),
            month = lubridate::month(X1),
            `PMOD TSI VIRGO A (orig.)` = X3,
            `PMOD TSI VIRGO A+B (orig.)` = X4,
            `PMOD TSI VIRGO A+B (orig.)_uncertainty` = 1.96 * X5, # Does X5 = 1σ?
            `PMOD TSI VIRGO A (new)` = X6,
            `PMOD TSI VIRGO A+B (new)` = X7
          )
      }, error = Error, warning = Error)

      d <- x %>% dplyr::group_by(year, month) %>%
        dplyr::group_map(
          function(a, b)
          {
            ## Use a better mean estimate for the "_uncertainty" columns.
            ## V. stats.stackexchange.com/questions/25848/how-to-sum-a-standard-deviation/26647#26647
            cnames <- get_climate_series_names(a, conf_int = TRUE)
            l <- list(cnames[stringr::str_ends(cnames, "_uncertainty", negate = TRUE)], cnames[stringr::str_ends(cnames, "_uncertainty", negate = FALSE)])
            r <- list(.vars = tibble::lst(!!l[[1]], !!l[[2]]),
                .funs = tibble::lst(
                  function(a) { r <- NA_real_; if (!all(is.na(a))) r <- mean(a, na.rm = TRUE); r },
                  function(a) { r <- NA_real_; if (!all(is.na(a))) r <- sqrt(mean(a^2, na.rm = TRUE)); r }
                )) %>%
              ## For applying multiple functions to different columns in 'summarize_at()', see:
              ## https://stackoverflow.com/questions/41109403/r-dplyr-summarise-multiple-functions-to-selected-variables/53981812#53981812
              purrr::pmap(~ a %>% dplyr::summarize_at(.x, .y)) %>%
              { purrr::reduce(c(list(b), .), cbind) }

            r
          }) %>% purrr::reduce(dplyr::bind_rows) %>%
          dplyr::arrange(year, month) %>%
          dplyr::mutate(yr_part = year + (2 * month - 1)/24, .after = month)

      return (d)
    })(path),

    `PMOD TSI (old)` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, skip = 1L, comment.char = ";", check.names = FALSE, colClasses = c(V1 = "character"))
      }, error = Error, warning = Error)

      dates <- as.POSIXct(x$V2 * 86400, origin = "1980-01-01")
      d <- data.frame(year(dates), month(dates), check.names = FALSE, stringsAsFactors = FALSE)
      flit <- data.matrix(x[, -(1:2)])
      is.na(flit) <- flit < -98
      d <- cbind(d, flit)
      names(d) <- c("year", "month", "PMOD TSI (new VIRGO)", "PMOD TSI (orig. VIRGO)")

      ## This data is daily, so it needs to be turned into monthly averages.
      d <- cbind(d[!duplicated(d[, 1:2]), 1:2], Reduce(rbind, by(d[, -(1:2)], list(d$month, d$year), colMeans, na.rm = TRUE, simplify = FALSE)))

      d$yr_part <- d$year + (2 * d$month - 1)/24

      return (d)
    })(path),

    `SORCE TSI` = (function(p) {
      x <- NULL

      tryCatch({
        x <- read.table(p, skip=0L, comment.char=";", check.names=FALSE, stringsAsFactors=FALSE)
      }, error=Error, warning=Error)

      re <- "(\\d{4})(\\d{2})(\\d{2})"
      yearMatches <- stringr::str_match(trunc(x$V1), re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, month=monthValue, check.names=FALSE, stringsAsFactors=FALSE)
      flit <- data.frame(x$V5, x$V9, check.names=FALSE, stringsAsFactors=FALSE)
      names(flit) <- c(series, series %_% "_uncertainty")
      flit[[series %_% "_uncertainty"]] <- 2 * 1.96 * flit[[series %_% "_uncertainty"]]
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
        x <- read.table(p, comment.char=";", check.names=FALSE) # N.B. This is currently (19 Aug 2019) offline; use local copy instead.
        #x <- read.table(system.file("extdata/tsi/TIM_TSI_Reconstruction.txt", package = "climeseries"), comment.char = ";", check.names = FALSE)
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
        CSIRO_down <- FALSE # Set to TRUE if the CSIRO FTP site fails.
        alt_p <- system.file("extdata/latest/CSIRO_Alt.csv", package="climeseries")
        if (!CSIRO_down)
          download.file(p, alt_p, mode = "wb", quiet = TRUE)
        x <- read.csv(ifelse(CSIRO_down, alt_p, p), header = FALSE, skip = skip, check.names = FALSE, na.strings = "#N/A")
      }, error = Error, warning = Error)

      re <- "(\\d{4})\\.(\\d{3})"
      yearMatches <- stringr::str_match(x$V1, re)
      yearValue <- as.numeric(yearMatches[, 2L])
      monthValue <- as.numeric(factor(yearMatches[, 3L]))

      d <- data.frame(year=yearValue, yr_part=yearValue + (2 * monthValue - 1)/24, month=monthValue, temp=x$V2, check.names=FALSE, stringsAsFactors=FALSE)

      return (d)
    })(path),

    `NOAA Global Mean Sea Level` = (function(p) {
      x <- NULL

      skip <- 0L

      ## NOAA STAR data temporarily offline. Comment this out after using once:
      #return (get_climate_data(download = FALSE, baseline = FALSE)[, c("year", "month", "yr_part", series)])

      tryCatch({
        x <- read.csv(p, header=TRUE, skip=skip, check.names=FALSE, comment.char="#")
      }, error=Error, warning=Error)

      l <- na.omit(data.table::data.table(melt(x, id="year")), cols="value")
      setnames(l, c("yr_part", "source", "temp"))
      setcolorder(l, c(1, 3, 2))

      r <- range(trunc(l$yr_part))
      flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
      flit$yr_part <- flit$year + (2 * flit$month - 1)/24
      flit <- data.table::data.table(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(l)
      m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
      m <- data.table::as.data.table(dplyr::full_join(flit, m, by=c("year", "month")))

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
      flit <- data.table::data.table(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(x)
      m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
      m <- data.table::as.data.table(dplyr::full_join(flit, m, by=c("year", "month")))
      ## Uncertainties are 1 × σ (Church & White 2011, dx.doi.org/10.1007/s10712-011-9119-1, Fig. 5), & 1.96 × sigma is 95% CI.
      m[, `_uncertainty` := 2 * 1.96 * `_uncertainty`]
      setnames(m, "_uncertainty", series %_% "_uncertainty")
      d <- as.data.frame(m)

      return (d)
    })(path),

    `AVISO Global Mean Sea Level (nonseasonal)` =,
    `AVISO Global Mean Sea Level` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.table(p, header = FALSE, as.is = TRUE, skip = skip, check.names = FALSE)
      }, error = Error, warning = Error)

      colnames(x) <- c("yr_part", series)

      r <- range(trunc(x$yr_part))
      flit <- expand.grid(month = 1:12, year = seq(r[1], r[2], by = 1))
      flit$yr_part <- flit$year + (2 * flit$month - 1)/24
      flit <- data.table::data.table(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(x)
      m <- flit[m, roll = "nearest"]; m[, yr_part := NULL]
      ## Remove year/month duplicates by averaging:
      m <- m[, lapply(.SD, mean, na.rm = TRUE), by = .(year, month), .SDcols = c(series)]
      m <- data.table::as.data.table(dplyr::full_join(flit, m, by = c("year", "month")))
      d <- as.data.frame(m) %>%
        dplyr::mutate(!!series := 1000 * .data[[series]]) # Convert to mm (like other GMSL series here)

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

        x <- read.table(text = rawToChar(as.raw(httr::content(x0))),
          header = FALSE, skip = skip, check.names = FALSE, comment.char = "H")
      }, error = Error, warning = Error)

      x <- x[, 1:3] # Ocean mass has more than 3 columns.
      colnames(x) <- c("yr_part", series, "_uncertainty")

      r <- range(trunc(x$yr_part))
      flit <- expand.grid(month = 1:12, year=seq(r[1], r[2], by = 1))
      flit$yr_part <- flit$year + (2 * flit$month - 1)/24
      flit <- data.table::data.table(flit)
      data.table::setkey(flit, yr_part)

      m <- copy(x)
      m <- flit[m, roll="nearest"]; m[, yr_part := NULL]
      m <- m[, lapply(.SD, mean, na.rm = TRUE), by = .(year, month), .SDcols=c(series, "_uncertainty")] # Remove year/month duplicates by averaging.
      m <- dplyr::full_join(flit, m, by = c("year", "month"))
      m <- data.table::as.data.table(m) # This needs optimization for the future "climeseries" update.
      ## Uncertainties are 1 × sigma, so 1.96 × sigma is a 95% CI.
      m[, `_uncertainty` := 2 * 1.96 * `_uncertainty`]
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
      curl <- RCurl::getCurlHandle()
      RCurl::curlSetOpt(useragent="Mozilla/5.0", followlocation=TRUE, curl=curl)
      tryCatch({
        ## Get new session ID.
        r <- RCurl::getURLContent("http://giovanni.gsfc.nasa.gov/giovanni/daac-bin/service_manager.pl", curl=curl)
        xml_ <- XML::xmlParse(r, useInternalNodes=TRUE)
        sessionId <- XML::xpathSApply(xml_, "/session", xmlAttrs)["id"]
        submitUrl <- sub("@@DATE@@", format(today(), "%Y-%m-%d"), sub("@@SESSIONID@@", sessionId, p))

        ## Set off build of MODIS AOD data set.
        sessionContent <- RCurl::getURLContent(submitUrl, curl=curl)
        sessionJson <- jsonlite::fromJSON(sessionContent, simplifyVector=TRUE, flatten=TRUE)
        resultsetId <- sessionJson$session$resultset$id
        resultId <- sessionJson$session$resultset$result[[1]]$id

        ## Check progress of build of MODIS AOD data set.
        percentComplete <- 0
        cat(fill=TRUE)
        while (percentComplete != 100) {
          cat("    Data build", percentComplete %_% "% complete.", fill=TRUE); flush.console()
          progressUrlBase <- "http://giovanni.gsfc.nasa.gov/giovanni/daac-bin/service_manager.pl?session=@@SESSIONID@@&resultset=@@RESULTSETID@@&result=@@RESULTID@@&portal=GIOVANNI&format=json"
          progressUrl <- sub("@@RESULTID@@", resultId, sub("@@RESULTSETID@@", resultsetId, sub("@@SESSIONID@@", sessionId, progressUrlBase)))
          progressContent <- RCurl::getURLContent(progressUrl, curl=curl)
          progressJson <- jsonlite::fromJSON(progressContent, simplifyVector=TRUE, flatten=TRUE)
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
      dateMatches <- stringr::str_match(x[[1]], re)
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
      yearMatches <- stringr::str_match(x$V1, re)
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
        dirNames <- strsplit(RCurl::getURL(p, dirlistonly=TRUE), "\r*\n")[[1L]]
        x <- list()
        cat(fill=TRUE)
        for (i in dirNames) {
          fileNames <- strsplit(RCurl::getURL(paste0(p, i, "/"), dirlistonly=TRUE), "\r*\n")[[1L]]
          ## Make sure the following regex doesn't change or lead to mixed file versions.
          fileNames <- grep("^OSIRIS-Odin_L2-Aerosol-Limb-MART", fileNames, value=TRUE)
          x[[i]] <- list()
          for (j in fileNames) {
            re <- ".*?_(\\d{4})m(\\d{4})\\..*$"
            dateMatches <- stringr::str_match(j, re)
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

    `ERA5 Sea Ice Extent` =,
    `ERA5 2m Europe` =,
    `ERA5 2m Global` = (function(p) {
      currentMonth <- current_month; currentYear <- current_year
      if (currentMonth == 1) { currentYearLastMonth <- currentYear - 1; currentMonthLastMonth <- 12 }
      else { currentMonthLastMonth <- currentMonth - 1; currentYearLastMonth <- currentYear }

      uri <- gsub("@@MONTHNUM@@", sprintf("%02d", currentMonth), gsub("@@YEARNUM@@", currentYear, p))
      #uri <- gsub("@@MONTHNUM@@", sprintf("%02d", currentMonthLastMonth), gsub("@@YEARNUM@@", currentYear, p))
      uri <- gsub("@@MONTHNUM_LASTMONTH@@", sprintf("%02d", currentMonthLastMonth), gsub("@@YEARNUM_LASTMONTH@@", currentYearLastMonth, uri))
      if (httr::http_error(uri)) { ## Does the previous month's data exist?
        if (currentMonth == 1) { currentYear <- currentYear - 1; currentMonth <- 12 }
        else currentMonth <- currentMonth - 1

        if (currentMonthLastMonth == 1) { currentYearLastMonth <- currentYearLastMonth - 1; currentMonthLastMonth <- 12 }
        else currentMonthLastMonth <- currentMonthLastMonth - 1

        uri <-gsub("@@MONTHNUM@@", sprintf("%02d", currentMonth), gsub("@@YEARNUM@@", currentYear, p))
        uri <- gsub("@@MONTHNUM_LASTMONTH@@", sprintf("%02d", currentMonthLastMonth), gsub("@@YEARNUM_LASTMONTH@@", currentYearLastMonth, uri))

        if (httr::http_error(uri)) {
          uri <- gsub("@@MONTHNUM@@", sprintf("%02d", currentMonthLastMonth), gsub("@@YEARNUM@@", currentYear, p))
          uri <- gsub("@@MONTHNUM_LASTMONTH@@", sprintf("%02d", currentMonthLastMonth), gsub("@@YEARNUM_LASTMONTH@@", currentYearLastMonth, uri))
        }
      }

      x <- NULL

      skip <- 0L

      tryCatch({
        x <- read.csv(uri, header = TRUE, skip = skip, check.names = FALSE, comment.char = "#")
      }, error = Error, warning = Error)

      #re <- "(\\d{4})(\\d{2})"
      re <- "(\\d{4})-(\\d{2})-(\\d{2})"
      dateMatches <- stringr::str_match(x[[1]], re)
      yearValue <- as.numeric(dateMatches[, 2L])
      monthValue <- as.numeric(dateMatches[, 3L])

      d <- dataframe(year = yearValue, yr_part = yearValue + (2 * monthValue - 1)/24, month = monthValue, temp = x$`2t`) %>%
        dplyr::rename(!!series := temp)

      return (d)
    })(path),

    #`ERA5 2m Global` =,
    `ERA-Interim 2m Global` = (function(p) {
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

    `JRA-55 Surface Air SH` =,
    `JRA-55 Surface Air SH Land` =,
    `JRA-55 Surface Air SH Ocean` =,
    `JRA-55 Surface Air SH Polar` =,
    `JRA-55 Surface Air SH Polar Land` =,
    `JRA-55 Surface Air SH Polar Ocean` =,
    `JRA-55 Surface Air NH` =,
    `JRA-55 Surface Air NH Land` =,
    `JRA-55 Surface Air NH Ocean` =,
    `JRA-55 Surface Air NH Polar` =,
    `JRA-55 Surface Air NH Polar Land` =,
    `JRA-55 Surface Air NH Polar Ocean` =,
    `JRA-55 Surface Air Global` =,
    `JRA-55 Surface Air Global Land` =,
    `JRA-55 Surface Air Global Ocean` =,
    `JRA-55 Surface Air Tropics` =,
    `JRA-55 Surface Air Tropics Land` =,
    `JRA-55 Surface Air Tropics Ocean` =,
    `JRA-55 Surface Air USA 48` =,
    `JRA-55 Surface Air USA 48 Land` =,
    `JRA-55 Surface Air USA 48 Ocean` =,
    `JRA-3Q Surface Air SH` =,
    `JRA-3Q Surface Air SH Land` =,
    `JRA-3Q Surface Air SH Ocean` =,
    `JRA-3Q Surface Air SH Polar` =,
    `JRA-3Q Surface Air SH Polar Land` =,
    `JRA-3Q Surface Air SH Polar Ocean` =,
    `JRA-3Q Surface Air NH` =,
    `JRA-3Q Surface Air NH Land` =,
    `JRA-3Q Surface Air NH Ocean` =,
    `JRA-3Q Surface Air NH Polar` =,
    `JRA-3Q Surface Air NH Polar Land` =,
    `JRA-3Q Surface Air NH Polar Ocean` =,
    `JRA-3Q Surface Air Global` =,
    `JRA-3Q Surface Air Global Land` =,
    `JRA-3Q Surface Air Global Ocean` =,
    `JRA-3Q Surface Air Tropics` =,
    `JRA-3Q Surface Air Tropics Land` =,
    `JRA-3Q Surface Air Tropics Ocean` =,
    `JRA-3Q Surface Air USA 48` =,
    `JRA-3Q Surface Air USA 48 Land` =,
    `JRA-3Q Surface Air USA 48 Ocean` =,
    `ERA5 Surface Air SH` =,
    `ERA5 Surface Air SH Land` =,
    `ERA5 Surface Air SH Ocean` =,
    `ERA5 Surface Air SH Polar` =,
    `ERA5 Surface Air SH Polar Land` =,
    `ERA5 Surface Air SH Polar Ocean` =,
    `ERA5 Surface Air NH` =,
    `ERA5 Surface Air NH Land` =,
    `ERA5 Surface Air NH Ocean` =,
    `ERA5 Surface Air NH Polar` =,
    `ERA5 Surface Air NH Polar Land` =,
    `ERA5 Surface Air NH Polar Ocean` =,
    `ERA5 Surface Air Global` =,
    `ERA5 Surface Air Global Land` =,
    `ERA5 Surface Air Global Ocean` =,
    `ERA5 Surface Air Tropics` =,
    `ERA5 Surface Air Tropics Land` =,
    `ERA5 Surface Air Tropics Ocean` =,
    `ERA5 Surface Air USA 48` =,
    `ERA5 Surface Air USA 48 Land` =,
    `ERA5 Surface Air USA 48 Ocean` =,
    `NCEP/NCAR R1 Surface Air SH` =,
    `NCEP/NCAR R1 Surface Air SH Land` =,
    `NCEP/NCAR R1 Surface Air SH Ocean` =,
    `NCEP/NCAR R1 Surface Air SH Polar` =,
    `NCEP/NCAR R1 Surface Air SH Polar Land` =,
    `NCEP/NCAR R1 Surface Air SH Polar Ocean` =,
    `NCEP/NCAR R1 Surface Air NH` =,
    `NCEP/NCAR R1 Surface Air NH Land` =,
    `NCEP/NCAR R1 Surface Air NH Ocean` =,
    `NCEP/NCAR R1 Surface Air NH Polar` =,
    `NCEP/NCAR R1 Surface Air NH Polar Land` =,
    `NCEP/NCAR R1 Surface Air NH Polar Ocean` =,
    `NCEP/NCAR R1 Surface Air Global` =,
    `NCEP/NCAR R1 Surface Air Global Land` =,
    `NCEP/NCAR R1 Surface Air Global Ocean` =,
    `NCEP/NCAR R1 Surface Air Tropics` =,
    `NCEP/NCAR R1 Surface Air Tropics Land` =,
    `NCEP/NCAR R1 Surface Air Tropics Ocean` =,
    `NCEP/NCAR R1 Surface Air USA 48` =,
    `NCEP/NCAR R1 Surface Air USA 48 Land` =,
    `NCEP/NCAR R1 Surface Air USA 48 Ocean` =,
    `NCEP/NCAR R1 Sea Surface SH` =,
    `NCEP/NCAR R1 Sea Surface NH` =,
    `NCEP/NCAR R1 Sea Surface Global` =,
    `NCEP/DOE R2 Surface Air SH` =,
    `NCEP/DOE R2 Surface Air SH Land` =,
    `NCEP/DOE R2 Surface Air SH Ocean` =,
    `NCEP/DOE R2 Surface Air SH Polar` =,
    `NCEP/DOE R2 Surface Air SH Polar Land` =,
    `NCEP/DOE R2 Surface Air SH Polar Ocean` =,
    `NCEP/DOE R2 Surface Air NH` =,
    `NCEP/DOE R2 Surface Air NH Land` =,
    `NCEP/DOE R2 Surface Air NH Ocean` =,
    `NCEP/DOE R2 Surface Air NH Polar` =,
    `NCEP/DOE R2 Surface Air NH Polar Land` =,
    `NCEP/DOE R2 Surface Air NH Polar Ocean` =,
    `NCEP/DOE R2 Surface Air Global` =,
    `NCEP/DOE R2 Surface Air Global Land` =,
    `NCEP/DOE R2 Surface Air Global Ocean` =,
    `NCEP/DOE R2 Surface Air Tropics` =,
    `NCEP/DOE R2 Surface Air Tropics Land` =,
    `NCEP/DOE R2 Surface Air Tropics Ocean` =,
    `NCEP/DOE R2 Surface Air USA 48` =,
    `NCEP/DOE R2 Surface Air USA 48 Land` =,
    `NCEP/DOE R2 Surface Air USA 48 Ocean` =,
    `NCEP/DOE R2 Sea Surface SH` =,
    `NCEP/DOE R2 Sea Surface NH` =,
    `NCEP/DOE R2 Sea Surface Global` =,
    `NCEP/CFSR Surface Air SH` =,
    `NCEP/CFSR Surface Air SH Land` =,
    `NCEP/CFSR Surface Air SH Ocean` =,
    `NCEP/CFSR Surface Air SH Polar` =,
    `NCEP/CFSR Surface Air SH Polar Land` =,
    `NCEP/CFSR Surface Air SH Polar Ocean` =,
    `NCEP/CFSR Surface Air NH` =,
    `NCEP/CFSR Surface Air NH Land` =,
    `NCEP/CFSR Surface Air NH Ocean` =,
    `NCEP/CFSR Surface Air NH Polar` =,
    `NCEP/CFSR Surface Air NH Polar Land` =,
    `NCEP/CFSR Surface Air NH Polar Ocean` =,
    `NCEP/CFSR Surface Air Global` =,
    `NCEP/CFSR Surface Air Global Land` =,
    `NCEP/CFSR Surface Air Global Ocean` =,
    `NCEP/CFSR Surface Air Tropics` =,
    `NCEP/CFSR Surface Air Tropics Land` =,
    `NCEP/CFSR Surface Air Tropics Ocean` =,
    `NCEP/CFSR Surface Air USA 48` =,
    `NCEP/CFSR Surface Air USA 48 Land` =,
    `NCEP/CFSR Surface Air USA 48 Ocean` =,
    `MERRA-2 Surface Air SH` =,
    `MERRA-2 Surface Air SH Land` =,
    `MERRA-2 Surface Air SH Ocean` =,
    `MERRA-2 Surface Air SH Polar` =,
    `MERRA-2 Surface Air SH Polar Land` =,
    `MERRA-2 Surface Air SH Polar Ocean` =,
    `MERRA-2 Surface Air NH` =,
    `MERRA-2 Surface Air NH Land` =,
    `MERRA-2 Surface Air NH Ocean` =,
    `MERRA-2 Surface Air NH Polar` =,
    `MERRA-2 Surface Air NH Polar Land` =,
    `MERRA-2 Surface Air NH Polar Ocean` =,
    `MERRA-2 Surface Air Global` =,
    `MERRA-2 Surface Air Global Land` =,
    `MERRA-2 Surface Air Global Ocean` =,
    `MERRA-2 Surface Air Tropics` =,
    `MERRA-2 Surface Air Tropics Land` =,
    `MERRA-2 Surface Air Tropics Ocean` =,
    `MERRA-2 Surface Air USA 48` =,
    `MERRA-2 Surface Air USA 48 Land` =,
    `MERRA-2 Surface Air USA 48 Ocean` =,
    `JMA Surface Air SH` =,
    `JMA Surface Air SH Land` =,
    `JMA Surface Air SH Ocean` =,
    `JMA Surface Air SH Polar` =,
    `JMA Surface Air SH Polar Land` =,
    `JMA Surface Air SH Polar Ocean` =,
    `JMA Surface Air NH` =,
    `JMA Surface Air NH Land` =,
    `JMA Surface Air NH Ocean` =,
    `JMA Surface Air NH Polar` =,
    `JMA Surface Air NH Polar Land` =,
    `JMA Surface Air NH Polar Ocean` =,
    `JMA Surface Air Global` =,
    `JMA Surface Air Global Land` =,
    `JMA Surface Air Global Ocean` =,
    `JMA Surface Air Tropics` =,
    `JMA Surface Air Tropics Land` =,
    `JMA Surface Air Tropics Ocean` =,
    `JMA Surface Air USA 48` =,
    `JMA Surface Air USA 48 Land` =,
    `JMA Surface Air USA 48 Ocean` =,
    `ERA-20C Surface Air SH` =,
    `ERA-20C Surface Air SH Land` =,
    `ERA-20C Surface Air SH Ocean` =,
    `ERA-20C Surface Air SH Polar` =,
    `ERA-20C Surface Air SH Polar Land` =,
    `ERA-20C Surface Air SH Polar Ocean` =,
    `ERA-20C Surface Air NH` =,
    `ERA-20C Surface Air NH Land` =,
    `ERA-20C Surface Air NH Ocean` =,
    `ERA-20C Surface Air NH Polar` =,
    `ERA-20C Surface Air NH Polar Land` =,
    `ERA-20C Surface Air NH Polar Ocean` =,
    `ERA-20C Surface Air Global` =,
    `ERA-20C Surface Air Global Land` =,
    `ERA-20C Surface Air Global Ocean` =,
    `ERA-20C Surface Air Tropics` =,
    `ERA-20C Surface Air Tropics Land` =,
    `ERA-20C Surface Air Tropics Ocean` =,
    `ERA-20C Surface Air USA 48` =,
    `ERA-20C Surface Air USA 48 Land` =,
    `ERA-20C Surface Air USA 48 Ocean` =,
    `20th C. Reanalysis V3 Surface Air SH` =,
    `20th C. Reanalysis V3 Surface Air SH Land` =,
    `20th C. Reanalysis V3 Surface Air SH Ocean` =,
    `20th C. Reanalysis V3 Surface Air SH Polar` =,
    `20th C. Reanalysis V3 Surface Air SH Polar Land` =,
    `20th C. Reanalysis V3 Surface Air SH Polar Ocean` =,
    `20th C. Reanalysis V3 Surface Air NH` =,
    `20th C. Reanalysis V3 Surface Air NH Land` =,
    `20th C. Reanalysis V3 Surface Air NH Ocean` =,
    `20th C. Reanalysis V3 Surface Air NH Polar` =,
    `20th C. Reanalysis V3 Surface Air NH Polar Land` =,
    `20th C. Reanalysis V3 Surface Air NH Polar Ocean` =,
    `20th C. Reanalysis V3 Surface Air Global` =,
    `20th C. Reanalysis V3 Surface Air Global Land` =,
    `20th C. Reanalysis V3 Surface Air Global Ocean` =,
    `20th C. Reanalysis V3 Surface Air Tropics` =,
    `20th C. Reanalysis V3 Surface Air Tropics Land` =,
    `20th C. Reanalysis V3 Surface Air Tropics Ocean` =,
    `20th C. Reanalysis V3 Surface Air USA 48` =,
    `20th C. Reanalysis V3 Surface Air USA 48 Land` =,
    `20th C. Reanalysis V3 Surface Air USA 48 Ocean` =,
    `20th C. Reanalysis V3 Sea Surface SH` =,
    `20th C. Reanalysis V3 Sea Surface NH` =,
    `20th C. Reanalysis V3 Sea Surface Global` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- httr::GET(p)
        localPath <- drop(stringr::str_match(httr::content(flit, "text"), stringr::regex("tmp/.*?\\.txt", ignore_case = TRUE)))
        if (length(localPath) > 1)
          warning("Web scrape found multiple hits for local data path (should only find one).")
        purl <- httr::parse_url(p)
        dataPath <- paste(purl$scheme %_% "://" %_% purl$hostname, localPath, sep = "/")
        flit1 <- stringr::str_extract(readLines(dataPath), "^\\d{4}\\s+.*$") %>% stringi::stri_remove_na()
        x <- read.table(text = flit1, header = FALSE, skip = skip, check.names = FALSE)
      }, error = Error, warning = Error)

      flit2 <- reshape2::melt(x[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
      for (i in names(flit2)) flit2[[i]] <- as.numeric(flit2[[i]])
      flit2 <- dplyr::arrange(flit2, V1, month)

      d <- dataframe(year = flit2$V1, yr_part = flit2$V1 + (2 * flit2$month - 1)/24, month = flit2$month, temp = flit2$temp)
      ## Missing values are given as "-9999".
      is.na(d$temp) <- d$temp == -9999

      return (d)
    })(path),

    `GRACE-FO Greenland Ice Mass` =,
    `GRACE-FO Antarctic Ice Mass` = (function(p) {
      x <- NULL

      skip <- 0L

      tryCatch({
        flit <- tempfile(fileext = ".zip")
        download.file(p, flit, mode = "wb", quiet = TRUE)
        x <- rio::import(flit, format = "csv", skip = skip)
      }, error = Error, warning = Error)

      matches <- stringr::str_match(x$`time [yyyy_doy]`, "^(\\d{4})_(\\d+)$")
      yearTemp <- lubridate::date_decimal(as.numeric(matches[, 2]))
      lubridate::day(yearTemp) <- as.numeric(matches[, 3])
      y <- lubridate::year(yearTemp)
      m <- lubridate::month(yearTemp)

      flit <- x[, -(1:2), drop = FALSE]
      flitNames <- names(flit)
      names(flit) <- paste(series, stringr::str_replace(flitNames, "^std_(.*?)$", "\\1_uncertainty"))

      r <- range(y)
      flit2 <- expand.grid(month = 1:12, year = seq(r[1], r[2], by = 1))

      d0 <- data.table::data.table(dataframe(year = y, month = m, flit))
      d0 <- d0[, lapply(.SD, mean, na.rm = TRUE), by = .(year, month), .SDcols = names(flit)] # Remove year/month duplicates by averaging.
      d0 <- dplyr::full_join(flit2, d0, by = c("year", "month"))
      m <- data.table::as.data.table(m) # This needs optimization for the future "climeseries" update.

      ## Uncertainties are 1 × sigma, so 1.96 × sigma is a 95% CI.
      d <- d0 %>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_uncertainty")), function(a) { a * 2 * 1.96 }) %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24)

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
                #x <- read.table(aa$url, header=TRUE, skip=skip, check.names=FALSE)
                x <- read.table(text = httr::content(httr::GET(aa$url), "text"), header=TRUE, skip=skip, check.names=FALSE)
              }, error=Error, warning=Error)

              x
            }
          )

          l <- plyr::arrange(Reduce(rbind, l), YEAR)
          ## Columns 3, 5, 7 are 1 × sigma, so 1.96 × sigma is a 95% CI.
          l_ply(c(3, 5, 7), function(a) l[[a]] <<- 2 * 1.96 * l[[a]])
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
                #x <- read.table(aa$url, header=TRUE, skip=skip, check.names=FALSE)
                x <- read.table(text = httr::content(httr::GET(aa$url), "text"), header=TRUE, skip=skip, check.names=FALSE)
              }, error=Error, warning=Error)

              x
            }
          )

          l <- plyr::arrange(Reduce(rbind, l), YEAR)
          ## Columns 3, 5, 7 are 1 × sigma, so 1.96 × sigma is a 95% CI.
          l_ply(c(3, 5, 7), function(a) l[[a]] <<- 2 * 1.96 * l[[a]])
          flit1 <- paste("NCEI", a$basin[1], "Ocean Heat Content", a$depth[1]) %_% c("", " NH", " SH") %_% " (Pentadal)"
          flit2 <- flit1 %_% "_uncertainty"
          colNames <- c("yr_part", c(flit1, flit2)[order(c(seq_along(flit1), seq_along(flit2) + 0.5))])
          colnames(l) <- colNames

          r <- range(trunc(l$yr_part))
          flit <- expand.grid(month=1:12, year=seq(r[1], r[2], by=1))
          flit$yr_part <- flit$year + (2 * flit$month - 1)/24
          flit <- data.table::data.table(flit)
          data.table::setkey(flit, yr_part)

          m <- data.table::data.table(l)
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
    cat(sprintf("Merging series %s...", i))
    if (is.null(d)) {
      d <- env[[i]][, c(common_columns, climeNames, uncertainty)]
    }
    else {
      d <- merge(d, env[[i]][, c(common_columns, climeNames, uncertainty)], by=common_columns, all=TRUE)
    }
    cat(". Done.", fill = TRUE); utils::flush.console()
  }

  # attr(d, "baseline") <- NULL
  # if (length(env) > 0L)
  #   attr(d, "baseline") <- attr(env[[ls(env)[1L]]], "baseline")

  d <- make_met_year(d)
  if (FALSE) { # Add series from an earlier data set (e.g. in case of current failure)
    env <- new.env()
    load("C:/common/data/climate/climeseries/climate-series_raw_20241117.RData", envir = env)
    flit <- env$d %>% dplyr::select(c(month, year, starts_with(c("JRA-55", "NCEP/CFSR")))) %>%
      dplyr::left_join(d, ., by = c("month", "year"))
    d <- flit
  }

  suffix <- format(Sys.Date(), "%Y%m%d")

  if (!is.null(dataDir)) {
    tempPath <- paste(dataDir, filenameBase %_% "raw_" %_% suffix, sep = "/")
    save(d, file = tempPath %_% ".RData")
    write.csv(d, file = tempPath %_% ".csv", row.names = FALSE)
  }

  e <- d
  d <- recenter_anomalies(e, defaultBaseline)

  if (!is.null(dataDir)) {
    tempPath <- paste(dataDir, filenameBase %_% suffix, sep = "/")
    save(d, file = tempPath %_% ".RData")
    write.csv(d, file = tempPath %_% ".csv", row.names = FALSE)
  }

  return (list(raw = e, baselined = d))
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
get_climate_data <- function(download = FALSE, data_dir, filename_base, urls=climeseries::data_urls, omit=omitUrlNames, only=NULL, baseline=TRUE, annual_mean=FALSE, verbose=TRUE)
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
# cat("1. " %_% names(e$raw), sep="\n")
## Get count of individual data sets:
# length(get_climate_series_names(e$raw))


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
get_climate_series_names <- function(x, conf_int = FALSE, invert = TRUE, keep = NULL, skip = NULL)
{
  colNames <- colnames(x)

  if (!invert) conf_int <- !conf_int

  r <- colNames[grep("(^yr_|^met_|^year|^month" %_% ifelse(conf_int, "", "|_uncertainty$") %_% ")", colNames, invert = invert)]
  if (!is.null(keep)) {
    keep <- sprintf("^(%s)$", paste(keep %>% rex::escape(), collapse = "|"))
    r <- stringr::str_subset(r, keep)
  }
  if (!is.null(skip)) {
    skip <- sprintf("^(%s)$", paste(skip %>% rex::escape(), collapse = "|"))
    r <- stringr::str_subset(r, skip, negate = TRUE)
  }

  return (r)
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
recenter_anomalies <- function(
  x,
  baseline = defaultBaseline,
  digits =  Inf, # Was '4L'
  by_month = TRUE,
  return_baselines_only = FALSE,
  ...
)
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

  flit <- subset(x, x[, "year", drop = TRUE] %in% baseline)

  for (i in tempSeries) {
    if (by_month) {
      bma <- tapply(flit[, i, drop = TRUE], flit[, "month", drop = TRUE], mean, na.rm = TRUE)
      base <- rep(NA_real_, nrow(x))
      ## N.B. This next step is both a time & memory sink, the latter being more problematic; optimize it!
      dev_null <- sapply(names(bma),
        function(s) { v <- bma[s]; if (is.nan(v)) v <- 0.0; base[x[, "month", drop = TRUE] == s] <<- v })
      dev_null <- NULL
    }
    else { # By year.
      bma <- tapply(flit[, i, drop = TRUE], flit[, "year", drop = TRUE], mean, na.rm = TRUE)
      base <- mean(bma)
    }

    ## Center anomalies on average baseline-period temperatures.
    #x[[i]] <- trunc((x[[i]] - base) * 10^digits) / 10^digits
    x[, i] <- round(x[, i] - base, digits)
  }
  attr(x, "baseline") <- baseline

  if (return_baselines_only)
    return (base)
  else
    return (x)
}


## N.B. This one is slower!
#' @export
recenter_anomalies_test <- function(
  x,
  baseline = defaultBaseline,
  digits = Inf, # Was '4L'
  by_month = TRUE,
  return_baselines_only = FALSE,
  ...
)
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

  base <- NULL
  if (by_month) {
    l <- keystone::psapply(get_climate_series_names(x, ...),
      function(series)
      {
        r <- x %>% dplyr::select(year, month, !!series) %>% dplyr::group_by(month) %>%
          dplyr::group_modify(
            function(f, g)
            {
              b <- f %>% dplyr::filter(year %in% baseline) %>% dplyr::pull(series)
              bavg <- b %>% mean(na.rm = TRUE)
              base <<- c(base, bavg)
              if (keystone::is_invalid(bavg))
                bavg <- 0

              r <- f %>% dplyr::mutate(!!series := round(.data[[series]] - bavg, digits)) %>%
                dplyr::select(-month)

              r
            }, .keep = TRUE) %>%
          dplyr::arrange(year, month) %>% dplyr::pull(series)

        r
      }, simplify = FALSE, .parallel = FALSE)
  } else {
    l <- keystone::psapply(get_climate_series_names(x, ...),
      function(series)
      {
        f <- x %>% dplyr::select(year, !!series)
        b <- f %>% dplyr::filter(year %in% baseline) %>% dplyr::pull(series)
        bavg <- b %>% mean(na.rm = TRUE)
        base <<- bavg
        if (keystone::is_invalid(bavg))
          bavg <- 0

        r <- f %>% dplyr::mutate(!!series := round(.data[[series]] - bavg, digits)) %>%
          dplyr::pull(series)

        r
      }, simplify = FALSE, .parallel = FALSE)
  }

  d <- x %>%
    dplyr::select(get_climate_series_names(x, invert = FALSE, ...)) %>% tibble::as_tibble() %>%
    dplyr::bind_cols(tibble::as_tibble(l))
  attr(d, "baseline") <- baseline

  if (return_baselines_only)
    return (base)
  else
    return (d)
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
make_time_series_from_anomalies <- function(
  x,
  frequency = 12L,
  offset = 0.5,
  add_missing_dates = TRUE,
  ...
)
{
  if (is.ts(x))
    return (x)

  startTime <- switch(as.character(frequency),
    `1` = (function() {
      r <- min(x[, "year"], na.rm = TRUE)

      structure(r, complete_dates = dataframe(year = seq(r,  max(x[, "year"], na.rm = TRUE))))
    })(),

    `12` = (function() {
      r <- unlist(x[1L, c("year", "month")])
      r[2] <- r[2] + offset

      x_yr_part <- x[, "year"] + (2 * x[, "month"] - 1)/24
      complete_dates <-
        expand.grid(month = 1:12, year = seq(min(x[, "year"], na.rm = TRUE), max(x[, "year"], na.rm = TRUE))) %>%
        dplyr::mutate(yr_part = year + (2 * month - 1)/24) %>%
        dplyr::filter(yr_part >= min(x_yr_part, na.rm = TRUE) & yr_part <= max(x_yr_part, na.rm = TRUE)) %>%
        dplyr::select(month, year)
      structure(r, complete_dates = complete_dates)
    })()
  )

  complete_dates <- attr(startTime, "complete_dates")
  if (add_missing_dates) {
    ## Suppress message "Joining with 'by = join_by(...)'";
    ## 'copy = TRUE' possibly needed for joining, say, a matrix & a data frame.
    x <- suppressMessages(dplyr::full_join(complete_dates, x, copy = TRUE))
    #x <- merge(complete_dates, x, all = TRUE) # Doesn't work as is

    if (frequency == 12L)
      x[, "yr_part"] <- x[, "year"] + (2 * x[, "month"] - 1)/24
  }

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
