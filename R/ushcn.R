get_ushcn_data <- function(
  archive_type = c(Average = "avg", Minimum = "min", Maximum = "max")
)
{
  tic("USHCN raw v. final")

  ## N.B. Pick your desired temp type here by index or name!
  archiveTempType <- c(Average = "avg", Minimum = "min", Maximum = "max")[1]

  dataDir <- getOption("climeseries_data_dir") # Put your favorite data directory here!
  dataDir <- "C:/Users/james/Downloads/climate/data/USHCN"
  archiveNames <- c(sprintf("ushcn.t%s.latest.raw.tar.gz", archiveTempType), sprintf("ushcn.t%s.latest.FLs.52j.tar.gz", archiveTempType))
  temp_type <- names(archiveTempType)
  ## ftp://ftp.ncdc.noaa.gov/pub/data/ushcn/v2.5/readme.txt
  #archivePaths <- paste("ftp://ftp.ncdc.noaa.gov/pub/data/ushcn/v2.5", archiveNames, sep = "/")
  archivePaths <- paste("https://www.ncei.noaa.gov/pub/data/ushcn/v2.5", archiveNames, sep = "/")

  fileList <- sapply(archivePaths,
    function(x, download)
    {
      archiveName <- paste(dataDir, basename(x), sep = "/")

      if (download) {
        download.file(x, archiveName, mode = "wb", quiet = TRUE)
        untar(archiveName, compressed = NA, exdir = dataDir, list = FALSE) # extras = "--overwrite" not supported
      }

      untar(archiveName, compressed = NA, list = TRUE)
    }, download = TRUE, simplify = FALSE) # 'download = FALSE' to use existing archives.

  temp <- sapply(fileList, function(x) paste(dataDir, x, sep = "/"), simplify = FALSE)
  ## https://stackoverflow.com/questions/46147901/r-test-if-a-file-exists-and-is-not-a-directory
  fl <- sapply(temp, function(a) a[Vectorize(file_test, vectorize.args = "x")("-f", a)], simplify = FALSE)

  surl <- "https://www.ncei.noaa.gov/pub/data/ushcn/v2.5/ushcn-v2.5-stations.txt"
  stations <- read.fwf(surl, widths = c(11, 9, 10, 7, 3, 32, 7, 6, 7, 3), comment.char = "", stringsAsFactors = FALSE)

  ## Make list of raw/final data frames for each station in same wide format as original text file.
  d <- sapply(fl,
    function(x)
    {
      sapply(x,
        function(y)
        {
          z <- read.fwf(y, c(11, 1, 4, rep(9, 12)), stringsAsFactors = FALSE); z <- z[, -2] # Some archives have an extra no. after the year!
          flit <- (gsub("\\s+\\d+", "", trimws(sapply(z[, -(1:2)], function(a) gsub("([A-Za-z]\\d*)", "", a)))))
          is.na(flit) <- flit == -9999

          #r <- dataframe(z[, 1:2], fahr_to_celsius(matrix(as.numeric(flit), ncol = NCOL(flit)) / 100.0))
          ## N.B. Conversion is unnecessary; temps are already in °C. V. the README file.
          r <- dataframe(z[, 1:2], matrix(as.numeric(flit), ncol = NCOL(flit)) / 100.0)
          flit <- stations[stations$V1 == unique(r[[1]])[1], ]
          attr(r, "location") <- list(lat = flit$V2, long = flit$V3, place = paste(trimws(flit$V6), trimws(flit$V5), sep = ", "))

          r
        }, simplify = FALSE)
    }, simplify = FALSE)

  ## Make list of raw/final data tables for each station in long format (year, month, temperature).
  e <- sapply(d,
    function(x)
    {
      sapply(x,
        function(y)
        {
          siteId <- unique(y$V1)
          z <- reshape2::melt(y[, 2L:14L], id.vars = "V3", variable.name = "month", value.name = siteId)
          z$month <- as.numeric(z$month)
          names(z)[names(z) == "V3"] <- "year"
          z <- data.table::data.table(dplyr::arrange(z, year, month))

          attr(z[[siteId]], "location") <- attr(y, "location")

          z
        }, simplify = FALSE)
    }, simplify = FALSE)

  ## Make list of raw/final wide data tables of all long individual stations by merging them on month & year.
  g <- sapply(names(e),
  function(a)
  {
    r <- range(e[[a]][[1]]$year)
    flit <- expand.grid(month = 1:12, year = seq(r[1], r[2], by = 1))
    plyr::l_ply(e[[a]], function(b) { flit <<- dplyr::full_join(flit, b, by = c("year", "month")) })

    dplyr::arrange(flit, year, month)
  }, simplify = FALSE)

  ## Calculate anomalies from some baseline for all stations.
  baseline <- 1951:1980
  #baseline <- 1981:2010
  h <- sapply(g, function(a) recenter_anomalies(a, baseline = baseline), simplify = FALSE)

  ## Create raw/final list of regional/planetary grids of latitude-weighted cells & populate them with station data.
  o <- sapply(h,
    function(x)
    {
      ## Create global grid of 2.5° × 3.5° squares and bin temp values in the correct square.
      p <- make_planetary_grid(grid_size = c(2.5, 3.5))

      y <- data.table::data.table(x)[, get_climate_series_names(x), with = FALSE]

      dev_null <- sapply(seq(NCOL(y)),
      function(z)
      {
        #cat(z, fill = TRUE)

        elms <- y[, z, with = FALSE]
        coords <- attr(elms[[1]], "location")
        lat <- coords[["lat"]]; long <- coords[["long"]]
        rc <- find_planetary_grid_square(p, lat, long)
        if (any(is.na(rc))) return ()
        sq <- p[[rc["row"], rc["col"]]][[1]]
        if (!is.data.frame(sq))
          p[[rc["row"], rc["col"]]][[1]] <<- elms
        else
          p[[rc["row"], rc["col"]]][[1]] <<- cbind(sq, elms)

        nop()
      }, simplify = FALSE)

      ## Check grid-cell contents before averaging:
      ## p[sapply(p, function(x) is.data.frame(x[[1]]))] # Which grid cells are populated?
      ## as.data.frame(p[<element_number>][[1]][[1]]) # To look at the data for cell <element_number>

      ## Create weighted average for each month for each grid cell.
      dev_null <- sapply(seq(length(p)),
      function(z)
      {
        pDT <- data.table::copy(p[z][[1]][[1]])
        if (is.data.frame(pDT)) {
          ## https://stackoverflow.com/questions/31258547/data-table-row-wise-sum-mean-min-max-like-dplyr
          p[z][[1]][[1]] <<- pDT[, `:=`(mean = rowMeans(.SD, na.rm = TRUE))][, .(mean)]
        }

        nop()
      }, simplify = FALSE)

      weights <- NULL
      r <- data.matrix(Reduce(cbind, sapply(seq(length(p)),
        function(z)
        {
          if (!is.data.frame(p[z][[1]][[1]])) return (NULL)

          weights <<- c(weights, attr(p[z][[1]], "weight"))

          p[z][[1]][[1]][[1]]
        }, simplify = FALSE)))

      rr <- plyr::aaply(r, 1, stats::weighted.mean, w = weights, na.rm = TRUE)
      is.na(rr) <- is.nan(rr)

      data.table::data.table(x)[, 1:2][, temp := rr]
    }, simplify = FALSE)

  ## Create raw/final list of long data frames of average temps for the entire region.
  r <- sapply(o,
    function(x) {
      yearRange <- range(x$year)
      r <- base::merge(expand.grid(month = 1:12, year = seq(yearRange[1], yearRange[2])), x, by = c("year", "month"), all = TRUE)
      #r$yr_part <- r$year + (r$month - 0.5)/12; r$met_year <- NA

      as.data.frame(r)
    }, simplify = FALSE)

  series <- c("USHCN " %_% temp_type %_% " Raw", "USHCN " %_% temp_type %_% " Final")
  for (i in seq(length(r))) {
    names(r[[i]])[names(r[[i]]) == "temp"] <- series[i]
  }

  ## Finally, merge raw/final data frames on year & month to produce a single data frame of the regional raw/final temps.
  s <- Reduce(merge_fun_factory(all = TRUE, by = c("year", "month")), r)
  s <- recenter_anomalies(s, baseline)
  diffSeries <- "USHCN Final minus Raw"
  s[[diffSeries]] <- s[[series[2]]] - s[[series[1]]]
  s$yr_part <- s$year + (s$month - 0.5)/12; s$met_year <- NA

  ## How does this compare to the NCEI US temps?

  u <- get_climate_data(download = FALSE, baseline = FALSE)
  #u <- merge(u, s[, c("year", "month", get_climate_series_names(s))], all = TRUE) # Or:
  u <- Reduce(merge_fun_factory(by = c("year", "month"), all = TRUE), list(u, s[, c("year", "month", get_climate_series_names(s))]))
  compSeries <- c(sprintf("NCEI US %s. Temp.", capitalize(archiveTempType)), sprintf("USHCN %s Final", names(archiveTempType)))

  toc()
}
