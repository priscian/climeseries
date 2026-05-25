#' @export
get_yearly_gistemp <- function(series="GISTEMP Met. Stations Oct. 2005", uri="https://web.archive.org/web/20051029130103/http://data.giss.nasa.gov/gistemp/graphs/Fig_A.txt", skip=0L){
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  x <- NULL

  gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

  tryCatch(
{
    #r <- httr::content(httr::GET(uri, httr::timeout(300)), "text", encoding="ISO-8859-1") # Gives timeout errors ≪ 300 s
    curl <- RCurl::getCurlHandle()
    RCurl::curlSetOpt(useragent="Mozilla/5.0", followlocation = TRUE, curl = curl)
    r <- RCurl::getURLContent(uri, curl = curl)
    tab <- gsub("^(?!\\s*\\d{4}\\s+).*$", "", strsplit(r, '\n')[[1L]], perl=TRUE)
    x <- read.table(text=tab, header=FALSE, as.is=TRUE, na.strings=c("*", "**", "***", "****"), skip=skip, check.names=FALSE)
  },
 error=Error,
 warning=Error
)

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
## N.B. The following won't work any more since changes to NASA's Web site:
#   get_yearly_gistemp("GISTEMP Global Met. Stations Current", "http://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A.txt")
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries::common_columns), lapply(allSeries, names))))), allSeries)
# d <- recenter_anomalies(d, 1951:1980) # Should be the same baseline, but make sure.
# series <- sapply(allSeries[-1], get_climate_series_names)
# plot_climate_data(d, series=series, start=1994, ma=NULL, lwd=2, conf_int=FALSE, show_trend=TRUE)
## N.B 'show_trend=TRUE' requires that the column 'met_year' be in the data frame 'x'. Fix this!


#' @export
get_old_monthly_gistemp <- function(series="GISTEMP Global Nov. 2015", uri="http://web.archive.org/web/20151218065405/http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.txt", skip=0L){
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  x <- NULL

  skip <- skip # Skip over notes at start of data.
  gissGlobalMean <- 14.0 # GISS absolute global mean for 1951–1980.

  #tryCatch({
    #r <- httr::content(httr::GET(uri, httr::timeout(300)), "text", encoding="ISO-8859-1") # Gives timeout errors ≪ 300 s
    curl <- RCurl::getCurlHandle()
    RCurl::curlSetOpt(useragent="Mozilla/5.0", followlocation = TRUE, curl = curl)
    r <- RCurl::getURLContent(uri, curl = curl)
    r <- gsub("*****", " ****", r, fixed=TRUE)
    r <- gsub("****", " ***", r, fixed=TRUE)
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
#   env$d[, c(climeseries::common_columns, "GISTEMP Global May 2016")],
#   get_old_monthly_gistemp()
# )
# d <- Reduce(merge_fun_factory(all=TRUE, by=c(Reduce(intersect, c(list(climeseries::common_columns), lapply(allSeries, names))))), allSeries)
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

  tryCatch(
{
    ## Scrape Web page for data.
    webPage <- httr::content(httr::GET(uri), "text", encoding="ISO-8859-1")
    webPage <- readLines(tc <- textConnection(webPage)); close(tc)
    pageTree <- htmlTreeParse(webPage, useInternalNodes=TRUE)
    ## The data table is in a PRE node (the only one, hopefully).
    pre <- XML::xpathSApply(pageTree, "//*/pre", xmlValue)
    ## Prepare the table text.
    tab <- strsplit(pre, '\n')[[1L]]
    tab <- tab[tab != ""]
    x <- read.table(text=tab, header=FALSE, skip=skip, fill=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
  },
 error=Error,
 warning=Error
)

  x$V2 <- as.numeric(x$V2) * 10 # Convert to mm.

  names(x) <- c("Year", "Sea Level (mm)")

  x
}

## usage:
# d <- get_satellite_slr(lat=26, lon=-80) # Off the coast of Miami, FL.


tidegaugeSlrBaseUrl <- "http://www.psmsl.org/data/obtaining/rlr.monthly.data/@@STATION_ID@@.rlrdata"
## List of IDs: http://www.psmsl.org/data/obtaining/
## Other long records not in the PSMSL data set: https://psmsl.org/data/longrecords/

#' @export
get_tidegauge_slr <- function(station_id){
  Error <- function(e) {
    cat(series %_% " series not available.", fill=TRUE)
  }

  skip <- 1L

  x <- NULL

  uri <- sub("@@STATION_ID@@", station_id, tidegaugeSlrBaseUrl)

  tryCatch(
{
    x <- read.csv2(uri, header=FALSE, fill=FALSE, check.names=FALSE, stringsAsFactors=FALSE)
  },
 error=Error,
 warning=Error
)

  x <- x[, 1:2]
  x$V2 <- as.numeric(x$V2)
  is.na(x$V2) <- x$V2 == -99999

  names(x) <- c("Year", "Sea Level (mm)")

  x
}

## usage:
# d <- get_tidegauge_slr(station_id=1858) # E.g. Virginia Key, E of Miami, FL.


#' @export
convert_hdf4_to_h5 <- function(
  hdf4_path = ".", # Can be a single directory or vector of file paths.
  h5_path = NULL, # If a list of file paths, must be same length as no. files given/listed by 'hdf4_path'.
  re = "^.*?\\.hdf$", # Case ignored unless overridden in 'list.files...'.
  list.files... = list(),
  converter_path = "h4toh5convert.exe",
  overwrite = FALSE,
  verbose = TRUE
){
  hdf4Path <- hdf4_path
  ## Is 'hdf4_path' a directory?
  if (utils::file_test("-d", hdf4_path[1])) { # Keep only 1st element; 'Vectorize()' if needed.
    list.filesArgs <- list(
      path = hdf4_path[1],
      pattern = re,
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
    list.filesArgs <- utils::modifyList(list.filesArgs, list.files..., keep.null = TRUE)
    hdf4Path <- do.call(list.files, list.filesArgs)
  }

  h5Path <- h5_path
  if (!is.null(h5_path) && utils::file_test("-d", h5_path[1])) { # N.B. Directory must already exist.
    h5Path <- paste(h5_path[1], basename(tools::file_path_sans_ext(hdf4Path)) %_% ".h5", sep = "/")
  }

  r <- sapply(seq_along(hdf4Path),
    function (i)    {
      if (verbose) {
        cat(sprintf("Converting file %s to HD5...", basename(hdf4Path[i]))); utils::flush.console()
      }

      hdf4File <- hdf4Path[i]
      if (is.null(h5_path))
        h5File <- "" # Convert in same directory w/ ext "h5".
      else
        h5File <- h5Path[i]

      ## Conversion software here: https://portal.hdfgroup.org/display/support/Download+h4h5tools
      cmd <- trimws(sprintf("\"%s\" \"%s\" \"%s\"", converter_path, hdf4File, h5File))
      rv <- NA
      if (!file.exists(h5File) || overwrite)
        rv <- system(cmd, intern = TRUE)

      if (verbose) {
        cat(". Done.", fill = TRUE); utils::flush.console()
      }

      rv
    },
 simplify = TRUE
)

  invisible(r)
}

## usage:
## V. https://disc.gsfc.nasa.gov/data-access
## Update AIRS gridded data. From the WSL Bash shell:
# sudo mount -t drvfs v: /mnt/v
# wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies -N -np -r --accept='*.hdf' -P /mnt/v/data/climate/AIRS-Level3 --content-disposition https://acdisc.gesdisc.eosdis.nasa.gov/data/Aqua_AIRS_Level3/AIRS3STM.006/
## Now convert HDF4 files to HD5 in R:
# r <- convert_hdf4_to_h5("V:/data/climate/AIRS-Level3/acdisc.gesdisc.eosdis.nasa.gov/data/Aqua_AIRS_Level3/AIRS3STM.006", "V:/data/climate/AIRS-Level3/h5")
## Or a single file, e.g.
# r <- convert_hdf4_to_h5("V:/data/climate/AIRS-Level3/acdisc.gesdisc.eosdis.nasa.gov/data/Aqua_AIRS_Level3/AIRS3STM.006/2020/AIRS.2020.09.01.L3.RetStd_IR030.v6.0.31.1.G20281103846.hdf", "V:/data/climate/AIRS-Level3/h5/AIRS.2020.09.01.L3.RetStd_IR030.v6.0.31.1.G20281103846.h5")
## Bash shell:
# sudo umount /mnt/v
#####
## New! V7: https://acdisc.gesdisc.eosdis.nasa.gov/data/Aqua_AIRS_Level3/AIRS3STM.7.0/
## Also, AIRS tables! https://data.giss.nasa.gov/gistemp/#tabledata


#' @export
create_airs_monthly_data <- function(
  data_path = ".",
  files_re = "^.*?\\.h5$", # Case ignored unless overridden in 'list.files...'.
  list.files... = list(),
  group_name = "/ascending/Data Fields/SurfSkinTemp_A", # Or "/descending/Data Fields/SurfSkinTemp_D"
  baseline = 2003:2018,
  apply_lat_weights = TRUE,
  series = "AIRS Surface Skin Global",
  save_rdata = FALSE
){
  list.filesArgs <- list(
    path = data_path,
    pattern = files_re,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  list.filesArgs <- utils::modifyList(list.filesArgs, list.files..., keep.null = TRUE)
  files <- do.call(list.files, list.filesArgs)

  l <- sapply(files,
    function(i)    {
      latitude <- t(rhdf5::h5read(i, "/location/Data Fields/Latitude"))
      longitude <- t(rhdf5::h5read(i, "/location/Data Fields/Longitude"))
      m <- rhdf5::h5read(i, "/location/Grid Attributes/Month")[1, 1]
      y <- rhdf5::h5read(i, "/location/Grid Attributes/Year")[1, 1]
      value <- t(rhdf5::h5read(i, group_name))

      attr(value, "metadata") <- list(year = y, month = m, lat = latitude[, 1], lon = longitude[1, ])

      value
    },
 simplify = FALSE
)

  d <- Reduce(rbind, sapply(l, function(x) {
 m <- attr(x, "metadata"); dataframe(year = m$year, month = m$month)
 }, simplify = FALSE))
  g <- Reduce(function(x, y) abind::abind(x, y, along = 3), l) # lat × lon × month

  p <- make_planetary_grid(grid_size = c(1, 1))

  i <- get_index_from_element(seq_along(g[, , 1]), g[, , 1])
  ## Put full time series into each grid cell.
  utils::flush.console()
  plyr::a_ply(i, 1,
    function(x)    {
      d$temp <- g[x[1], x[2], ]
      is.na(d$temp) <- d$temp == -9999

      p[[x[1], x[2]]][[1]] <<- d
    },
 .progress = "text"
)

  p0 <- rlang::duplicate(p, shallow = FALSE)

  ## Calculate time-series anomalies for each cell.
  flit <- expand.grid(month = 1:12, year = seq(min(d$year), max(d$year)))

  utils::flush.console()
  plyr::a_ply(i, 1,
    function(x)    {
      e <- p[[x[1], x[2]]][[1]]
      e <- merge(flit, e, by = c("year", "month"), all.x = TRUE)

      p[[x[1], x[2]]][[1]] <<- recenter_anomalies(e, baseline)
    },
 .progress = "text"
)

  ## Now weight the means zonally (i.e. by latitude grid).
  get_mean_series <- function(p)  {
    l <- sapply(seq(NROW(p)),
      function(x)      {
        y <- p[x, ]
        w <- attr(y[[1]], "weight")
        l <- sapply(names(y), function(i) {
 r <- y[[i]][[1]]; names(r)[names(r) == "temp"] <- i; r
 }, simplify = FALSE) # List of time series for this latitude
        ## Now merge all the series together.
        d <- dplyr::arrange(Reduce(function(i, j) merge(i, j, by = c("year", "month"), all = TRUE), l), year, month)

        m <- apply(data.matrix(d[, -(1:2)]), 1, function(i) {
 r <- NA; if(!all(is.na(i))) r <- mean(i, na.rm = TRUE); r
 })
        attr(m, "weight") <- w
        attr(m, "time") <- d[, c("year", "month")]

        m
      },
 simplify = FALSE
)

    w <- sapply(l, attr, which = "weight")
    ll <- Reduce(cbind, l)
    m <- apply(ll, 1, function(i) {
 r <- NA; if(!all(is.na(i))) r <- weighted.mean(i, w, na.rm = TRUE); r
 })

    d <- dplyr::arrange(Reduce(function(i, j) merge(i, j, by = c("year", "month"), all = TRUE), sapply(l, attr, which = "time", simplify = FALSE)), year, month)
    d[[series]] <- m

    d
  }
  r <- get_mean_series(p)

  ## This will be fairly inflexible, but it's mostly for debugging.
  if (save_rdata) {
    airsSaveDirBase <- "."
    if (!is.null(getOption("climeseries_data_dir")))
      airsSaveDirBase <- getOption("climeseries_data_dir")
    airsSaveDir <- paste(airsSaveDirBase, "AIRS", sep = "/")
    if (!dir.exists(airsSaveDir))
      dir.create(airsSaveDir, recursive = TRUE)

    fileName <- paste(stringr::str_replace_all(group_name, "/", "_"), make_current_timestamp(use_seconds = TRUE), sep = "_") %_% ".RData"
    save(list = c("l", "d", "g", "p0", "p", "i", "r"), file = paste(airsSaveDir, fileName, sep = "/"))
  }

  r
}

## usage:
#create_airs_monthly_data("E:/Users/priscian/my_documents/oversize/data/climate/AIRS-Level3/acdisc.gesdisc.eosdis.nasa.gov/data/Aqua_AIRS_Level3/AIRS3STM.006/2002/hd5")
#create_airs_monthly_data("V:/data/climate/AIRS-Level3/h5")


#' @export
create_combined_airs_series <- function(
  data_path = NULL,
  ascending = "/ascending/Data Fields/SurfSkinTemp_A",
  descending = "/descending/Data Fields/SurfSkinTemp_D",
  series = "AIRS Surface Skin Global",
  node_weights = 1,
  multiplier = 0.5,
  ...
){
  if (is.null(data_path)) {
    if (!is.null(getOption("climeseries_airs_data_dir")))
      data_path <- getOption("climeseries_airs_data_dir")
    else
      data_path <- "."
  }

  a <- create_airs_monthly_data(data_path = data_path, group_name = ascending, series = series, ...)
  d <- create_airs_monthly_data(data_path = data_path, group_name = descending, series = series, ...)

  w <- rep(node_weights, length.out = 2)

  ad <- a; ad[[series]] <- (w[1] * a[[series]] + w[2] * d[[series]]) * multiplier
  ad$yr_part <- ad$year + (2 * ad$month - 1)/24

  dplyr::arrange(ad, year, month)
}


## Linearly interpolate a climate series backwards for approximate baselining.
## I'll use this mostly for AIRS, but it might be otherwise applicable.
#' @export
create_cmip5_taz_data <- function(
  data_path = ".",
  rdata_path = paste(data_path, "cmip5-taz_all-members_lats-all.RData", sep = "/"),
  files_re = "^taz_Amon_ens_rcp(26|45|60|85)_.*?\\.nc$", # Case ignored unless overridden in 'list.files...'.
  list.files... = list(),
  filter_expr = NULL,
  verbose = TRUE
){
  list.filesArgs <- list(
    path = data_path,
    pattern = files_re,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  list.filesArgs <- utils::modifyList(list.filesArgs, list.files..., keep.null = TRUE)
  files <- do.call(list.files, list.filesArgs)

  cmip5_taz <- sapply(basename(files),
    function(i)    {
      f <- paste(data_path, i, sep = "/")

      if (verbose) {
        cat(sprintf("Processing file %s...", i)); utils::flush.console()
      }

      nc <- RNetCDF::open.nc(f)
      #RNetCDF::print.nc(nc)
      institute_id <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "institute_id")
      model_id <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "model_id")
      scenario <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "experiment")
      forcing <- RNetCDF::att.get.nc(nc, "NC_GLOBAL", "forcing")
      origin <- sub("^\\s*days since\\s*", "", RNetCDF::att.get.nc(nc, "time", "units"), ignore.case = TRUE)
      RNetCDF::close.nc(nc)

      x0 <- tidync::tidync(f)
      if (!is.null(filter_expr))
        x0 <- x0 %>%
 {
 eval(filter_expr)
 }
      x <- x0 %>%
 tidync::hyper_array() %>%
 `[[`(1L, drop = FALSE)

      latitude <- x0$transforms$lat %>%
 dplyr::filter(selected) %>%
 dplyr::pull(lat)
      air_pressure <- x0$transforms$plev %>%
 dplyr::filter(selected) %>%
 dplyr::pull(plev)
      dates <- x0$transforms$time %>%
 dplyr::filter(selected) %>%
 dplyr::pull(time) %>%
        as.Date(origin = origin)

      dimnames(x) <- list(latitude = latitude, air_pressure = air_pressure, dates = as.character(dates))

      attr(x, "latitude") <- latitude
      attr(x, "air_pressure") <- air_pressure
      attr(x, "dates") <- dates

      attr(x, "institute_id") <- institute_id
      attr(x, "model_id") <- model_id
      attr(x, "scenario") <- scenario

      if (verbose) {
        cat(". Done.", fill = TRUE); utils::flush.console()
      }

      x
    },
 simplify = FALSE
)

  if (!is.null(rdata_path))
    save(list = c("cmip5_taz"), file = rdata_path)

  cmip5_taz
}

## usage:
# cmip5_taz <- create_cmip5_taz_data("V:/data/climate/CMIP5-taz")
#
# cmip5_taz <- create_cmip5_taz_data(
#   data_path = "V:/data/climate/CMIP5-taz",
#   rdata_path = paste("V:/data/climate/CMIP5-taz", "cmip5-taz_all-members_lats-tropics.RData", sep = "/"),
#   filter_expr = expression(tidync::hyper_filter(., lat = lat < 24 & lat > -24))
# )


## V. ftp://ftp.remss.com/msu/weighting_functions
#' @export
get_rss_msu_weights <- function(
  weights_path,
  air_pressure, # Vector of air pressures to base interpolations on
  skip = 7
){
  ## These reads are very specific, but seem to work for all the RSS weighting functions:
  colNames <- unlist(read.table(weights_path, skip = skip - 2, header = FALSE, nrows = 1, check.names = FALSE, stringsAsFactors = FALSE))
  surface_weight <- read.table(weights_path, skip = skip - 4, header = FALSE, nrows = 1, check.names = FALSE, stringsAsFactors = FALSE)$V3
  w <- read.table(weights_path, skip = skip, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
  colnames(w) <- sub("^weight$", "Weight", colNames, ignore.case = TRUE, perl = TRUE)

  a <- air_pressure[air_pressure %nin% w$`P(pa)`]
  z <- merge(w, dataframe(`P(pa)` = a), by = "P(pa)", all = TRUE) %>%
    dplyr::arrange(desc(`P(pa)`))
  zz <- z %>%
 dplyr::select(`P(pa)`, `h(m)`, Weight) %>%
    interpNA(method = "linear", unwrap = FALSE) %>%
 dataframe()

  zzz <- zz %>% dplyr::filter(`P(pa)` %in% air_pressure)
  attr(zzz, "surface_weight") <- surface_weight
  attr(zzz, "original_data") <- w

  zzz
}

## usage:
# weights_path <- system.file("inst/extdata/misc/RSS/std_atmosphere_wt_function_chan_tmt_land.txt", package = "climeseries")
# air_pressure <- c(1e+05, 92500, 85000, 70000, 60000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, 7000, 5000, 3000, 2000, 1000)
# w <- get_rss_msu_weights(weights_path, air_pressure)


## V. http://www.realclimate.org/index.php/archives/2017/03/the-true-meaning-of-numbers/
#' @export
create_cmip5_atmosphere_temps <- function(
  taz_archive,
  channel,
  rdata_path = NULL,
  weighting_domain = c("_land", "_ocean"), # These will be blanks for TLS & TTS.
  column_integrate = FALSE,
  ...
){
  if (is.character(taz_archive)) {
    load(taz_archive)
    taz <- cmip5_taz
    cmip5_taz <- NULL
  } else
    taz <- taz_archive

  data(etopo5, package = "esd")
  land_ocean_weights <- list(land = sum(etopo5 >= 0)/length(etopo5), ocean = sum(etopo5 < 0)/length(etopo5))

  ## Estimate area-weighted mean temperature
  area_mean <- function(x, lat_weights) {
    d <- dim(lat_weights)
    y <- x * c(lat_weights)
    dim(y) <- d
    z <- colSums(y, na.rm = TRUE) / sum(lat_weights[, 1], na.rm = TRUE)

    z
  }


  total_weight_between_levels <- function(x) # Where 'x' = data frame from 'get_rss_msu_weights()'.
  {
    ## RSS pseudocode:
    # total_wt_between_level_minus_one_and_level_one =
    #   0.5 * (wt_function(level) + wt_function(level-1)) * (h(level) - h(level-1))

    w <- c(attr(x, "surface_weight"), x$Weight)
    h <- c(0.0, x$`h(m)`)

    0.5 * (x$Weight + head(w, -1)) * diff(h)
  }


  l <- sapply(names(taz),
    function(i)    {
      tazi <- taz[[i]]
      latitude <- attr(tazi, "latitude")
      d <- dim(tazi)

      lat_weights <- matrix(rep(cos(pi * latitude/180), d[2]), d[1], d[2])
      x <- tazi; dim(x) <- c(d[1] * d[2], d[3])
      z <- apply(x, 2, area_mean, lat_weights)

      air_pressure <- attr(tazi, "air_pressure")
      channel <- tolower(channel)
      ocean_msu_weights <- get_rss_msu_weights(system.file(sprintf("inst/extdata/misc/RSS/std_atmosphere_wt_function_chan_%s%s.txt", channel, weighting_domain[1]), package = "climeseries"), air_pressure, ...)
      land_msu_weights <- get_rss_msu_weights(system.file(sprintf("inst/extdata/misc/RSS/std_atmosphere_wt_function_chan_%s%s.txt", channel, weighting_domain[2]), package = "climeseries"), air_pressure, ...)
      msu_weights <-
        land_ocean_weights$ocean * ocean_msu_weights$Weight +
        land_ocean_weights$land * land_msu_weights$Weight

      if (!column_integrate)
        tt <- apply(z, 2, function(x, w) {
 sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
 }, w = msu_weights)

      ## Also test out vertical integration of weighted temps.
      # ocean_msu_weight_surface <- attr(ocean_msu_weights, "original_data") %>% dplyr::slice(1) %>% dplyr::select(`P(pa)`, `h(m)`, Weight)
      # land_msu_weight_surface <- attr(land_msu_weights, "original_data") %>% dplyr::slice(1) %>% dplyr::select(`P(pa)`, `h(m)`, Weight)
      total_weights_combined <-
        land_ocean_weights$ocean * total_weight_between_levels(ocean_msu_weights) +
        land_ocean_weights$land * total_weight_between_levels(land_msu_weights)
      total_msu_weights <- ocean_msu_weights %>% dplyr::mutate(Weight = total_weights_combined)
      layer_heights <- diff(c(0, total_msu_weights$`h(m)`)) # Unnecessary

      # tth <- apply(z, 2, function(x, w) { sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE) }, w = total_msu_weights$Weight) # ?
      if (column_integrate) {
        tt <- apply(z, 2,
          function(x, w, h)          {
            ## V. https://en.wikipedia.org/wiki/Weight_function#Weighted_average
            integratex(h, x * w)$value / integratex(h, w)$value
          },
 w = total_msu_weights$Weight, h = total_msu_weights$`h(m)`
)
      }

      dates <- attr(tazi, "dates")
      model_id <- attr(tazi, "model_id")
      model_name <- tools::file_path_sans_ext(i)

      r0 <- dataframe(year = lubridate::year(dates), month = lubridate::month(dates)) %>%
        dplyr::mutate(!!model_name := tt)
      r <- data.table::data.table(r0)
      r <- as.data.frame(r[, lapply(.SD, mean, na.rm = TRUE), by = .(year, month), .SDcols = names(r0)[3:NCOL(r0)]]) # Remove year/month duplicates by averaging.

      attr(r, "institute_id") <- attr(tazi, "institute_id")
      attr(r, "model_id") <- attr(tazi, "model_id")
      attr(r, "scenario") <- attr(tazi, "scenario")

      r
    },
 simplify = FALSE
)

  r <- range(c(sapply(l, function(x) range(x$year))))
  flit <- expand.grid(month = 1:12, year = seq(r[1], r[2], by = 1))

  m <- sapply(l,
    function(i)    {
      merge(flit, i, by = c("year", "month"), all = TRUE)[[3]]
    },
 simplify = TRUE
)
  colnames(m) <- sapply(l, function(x) names(x)[3])
  y <- flit %>%
    dplyr::mutate(yr_part = year + (2 * month - 1)/24, met_year = NA)
  m <- cbind(y, m, stringsAsFactors = FALSE)

  attr(m, "ensemble") <- "CMIP5"
  attr(m, "model_type") <- "taz"
  attr(m, "model") <- as.vector(sapply(l, attr, which = "model_id"))
  attr(m, "scenario") <- paste("RCP", sprintf("%.1f", as.numeric(sub("^(rcp)(.*)$", "\\2", sapply(l, attr, which = "scenario"), ignore.case = TRUE))))

  cmip5 <- m

  if (!is.null(rdata_path))
    save(list = c("cmip5"), file = rdata_path)

  m
}


create_osiris_daily_saod_data_orig <- function(
  data_path = ".",
  rdata_path = ".",
  daily_filename = "OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm.RData",
  planetary_grid = NULL,
  extract = FALSE
){
  if (extract) {
    fileNames <- list.files(data_path, pattern = "^AEROSOL-L2-LP-OSIRIS_ODIN-SASK_v7_4-", full.names = TRUE)
    fileDates <- tools::file_path_sans_ext(basename(fileNames)) %>% stringr::str_extract("\\d{6}$")
    x <- sapply(fileNames,
      function(i)      {
        cat("    Processing file", basename(i), fill = TRUE); flush.console()

        nc0 <- RNetCDF::open.nc(i)
        origin <- sub("^\\s*days since\\s*", "", RNetCDF::att.get.nc(nc0, "time", "units"), ignore.case = TRUE)
        dates <- RNetCDF::var.get.nc(nc0, "time") %>% as.Date(origin = origin)
        RNetCDF::close.nc(nc0)
        datesC <- dates %>% as.character

        nc <- tidync::tidync(i)
        varNames <- c("extinction", "altitude", "latitude", "longitude")
        x <- sapply(varNames,
          function(a)          {
            substitute(tidync::hyper_tbl_cube(nc %>% tidync::activate(A))$mets[[B]], list(A = as.name(a), B = a)) %>% eval
          },
 simplify = FALSE
)

        xx <- by(seq_along(datesC), datesC,
          function(a)          {
            list(
              extinction = x$extinction[, a, drop = FALSE] %>% unclass %>%
 {
 `[<-`(., is.nan(.), NA)
 },
              alt = x$altitude,
              lat = x$latitude[a],
              long = x$longitude[a]
            )
          },
 simplify = FALSE
) %>% unclass

        xx
      },
 simplify = FALSE
)

    names(x) <- fileDates

    save(x, file = paste(rdata_path, "AEROSOL-L2-LP-OSIRIS_ODIN-SASK_v7_3.RData", sep = "/"))
  } else
    load(paste(rdata_path, "AEROSOL-L2-LP-OSIRIS_ODIN-SASK_v7_3.RData", sep = "/"))

  ### Process the extinction data to calculate monthly SAOD.

  saodDaily <- NULL

  cat(fill = TRUE)
  for (i in names(x)) {
    re <- "(\\d{4})(\\d{2})"
    yearMatches <- stringr::str_match(i, re)
    yearValue <- as.numeric(yearMatches[, 2L])
    monthValue <- as.numeric(yearMatches[, 3L])
    saodDailyTemplate <-
      data.frame(year = yearValue, month = monthValue, day = NA, saod = NA, check.names = FALSE, stringsAsFactors = FALSE)

    for (j in seq_along(x[[i]])) {
      #dayValue <- as.numeric(stringr::str_match(names(x[[i]])[j], ".*?_\\d{4}m\\d{2}(\\d{2})\\..*$")[, 2])
      #cat("    Processing object", paste(i, tools::file_path_sans_ext(names(x[[i]])[j]), sep = "/"), fill = TRUE); flush.console()
      dayValue <- as.numeric(stringr::str_match(names(x[[i]])[j], "\\d{4}-\\d{2}-(\\d{2})")[, 2])
      cat("    Processing object", names(x[[i]])[j], fill = TRUE); flush.console()
      extinction <- x[[i]][[j]]$extinction
      ## Missing values are given as -9999.
      #is.na(extinction) <- extinction == -9999
      alt <- x[[i]][[j]]$alt
      extinction <- data.frame(extinction, check.names = FALSE, stringsAsFactors = FALSE)
      rownames(extinction) <- alt
      lat <- x[[i]][[j]]$lat
      long <- x[[i]][[j]]$long
      coords <- mapply(function(x, y) c(lat = x, long = y), lat, long, SIMPLIFY = FALSE)

      ## Get stratospheric subset of extinction values from 15–35 km. (After Sato et al. 1993 and Rieger et al. 2015;
      ##   but v. Ridley et al. 2014 for including some aerosol effects below 15 km.)
      keepRows <- alt >= 15 & alt <= 35
      e <- subset(extinction, keepRows)
      for (k in seq_along(coords)) {
        attr(e[[k]], "alt") <- alt[keepRows]
        attr(e[[k]], "coords") <- coords[[k]]
      }

      gridSaod <- sapply(e,
        function(y)        {
          r <- NA

          ## Calculate vertical column integral of aerosol extinction.
          if (!all(is.na(y))) {
            r <- integratex(attr(y, "alt"), y)$value
            ## Boucher - Atmospheric Aerosols--Properties and Climate Impacts (2015), p. 44 (Eq. 3.31):
            ## τ = τ_r × (λ / λ_r)^-α; λ = 550 nm, λ_r = 750 nm, τ_r is OSIRIS value, α = 2.3 (v. Rieger et al. 2015)
            ##   = τ_r × 2.04, where τ_r is aerosol extinction integrated from 15–35 km
            r <- r * 2.04
          }

          attr(r, "coords") <- attr(y, "coords")

          r
        },
 simplify = FALSE
      )

      ## Create global grid of 5° × 5° squares and bin each SAOD value in the correct square.
      if (is.null(planetary_grid))
        g <- make_planetary_grid()
      else
        g <- planetary_grid
      dev_null <- sapply(
gridSaod,
        function(y)        {
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
        function(y)        {
          r <- c(value = NA, weight = attr(y, "weight"))
          if (all(is.na(y[[1]]))) return (r)
          r["value"] <- mean(y[[1]], na.rm = TRUE)

          r
        },
 simplify = FALSE
      )

      d <- data.matrix(Reduce(rbind, d))
      saodToday <- stats::weighted.mean(d[, "value"], d[, "weight"], na.rm = TRUE)
      is.na(saodToday) <- is.nan(saodToday)
      saodTodayDf <- saodDailyTemplate
      saodTodayDf$day <- dayValue
      saodTodayDf$saod <- saodToday

      saodDaily <- rbind(saodDaily, saodTodayDf, make.row.names = FALSE, stringsAsFactors = FALSE)
    }
  }

  saod_daily <- saodDaily
  save(saod_daily, file = paste(rdata_path, daily_filename, sep = "/"))
}


#' @export
create_osiris_daily_saod_data <- function(
  data_path = ".",
  rdata_path = ".",
  daily_filename = "OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm.RData",
  planetary_grid = NULL,
  extract = FALSE,
  parallel = TRUE
){
  if (extract) {
    fileNames <- list.files(data_path, pattern = "^AEROSOL-L2-LP-OSIRIS_ODIN-SASK_v7_4-", full.names = TRUE)
    fileDates <- tools::file_path_sans_ext(basename(fileNames)) %>% stringr::str_extract("\\d{6}$")

    # Function to process a single file
    process_file <- function(i) {
      cat("    Processing file", basename(i), fill = TRUE); flush.console()

      nc0 <- RNetCDF::open.nc(i)
      origin <- sub("^\\s*days since\\s*", "", RNetCDF::att.get.nc(nc0, "time", "units"), ignore.case = TRUE)
      dates <- RNetCDF::var.get.nc(nc0, "time") %>% as.Date(origin = origin)
      RNetCDF::close.nc(nc0)
      datesC <- dates %>% as.character

      nc <- tidync::tidync(i)
      varNames <- c("extinction", "altitude", "latitude", "longitude")
      x <- sapply(varNames,
        function(a)        {
          substitute(tidync::hyper_tbl_cube(nc %>% tidync::activate(A))$mets[[B]], list(A = as.name(a), B = a)) %>% eval
        },
 simplify = FALSE
)

      xx <- by(seq_along(datesC), datesC,
        function(a)        {
          list(
            extinction = x$extinction[, a, drop = FALSE] %>% unclass %>%
 {
 `[<-`(., is.nan(.), NA)
 },
            alt = x$altitude,
            lat = x$latitude[a],
            long = x$longitude[a]
          )
        },
 simplify = FALSE
) %>% unclass

      xx
    }

    # Parallelize file processing with proper package loading
    if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
      x <- future.apply::future_lapply(fileNames, process_file,
                                        future.packages = c("RNetCDF", "tidync", "magrittr"),
                                        future.seed = TRUE
)
    } else {
      x <- lapply(fileNames, process_file)
    }

    names(x) <- fileDates

    save(x, file = paste(rdata_path, "AEROSOL-L2-LP-OSIRIS_ODIN-SASK_v7_3.RData", sep = "/"))
  } else
    load(paste(rdata_path, "AEROSOL-L2-LP-OSIRIS_ODIN-SASK_v7_3.RData", sep = "/"))

  ### Process the extinction data to calculate daily SAOD.

  # Pre-create planetary grid once
  if (is.null(planetary_grid))
    planetary_grid <- make_planetary_grid()

  # Get package namespace for exporting functions
  pkg_env <- parent.env(environment())

  # Function to process a single month's data
  process_month <- function(i, x_data, pg, integratex_fn, find_grid_fn) {
    re <- "(\\d{4})(\\d{2})"
    yearMatches <- stringr::str_match(i, re)
    yearValue <- as.numeric(yearMatches[, 2L])
    monthValue <- as.numeric(yearMatches[, 3L])
    saodDailyTemplate <-
      data.frame(year = yearValue, month = monthValue, day = NA, saod = NA, check.names = FALSE, stringsAsFactors = FALSE)

    results <- list()

    for (j in seq_along(x_data[[i]])) {
      dayValue <- as.numeric(stringr::str_match(names(x_data[[i]])[j], "\\d{4}-\\d{2}-(\\d{2})")[, 2])
      cat("    Processing object", names(x_data[[i]])[j], fill = TRUE); flush.console()

      extinction <- x_data[[i]][[j]]$extinction
      alt <- x_data[[i]][[j]]$alt
      extinction <- data.frame(extinction, check.names = FALSE, stringsAsFactors = FALSE)
      rownames(extinction) <- alt
      lat <- x_data[[i]][[j]]$lat
      long <- x_data[[i]][[j]]$long
      coords <- mapply(function(x, y) c(lat = x, long = y), lat, long, SIMPLIFY = FALSE)

      ## Get stratospheric subset of extinction values from 15–35 km.
      keepRows <- alt >= 15 & alt <= 35
      e <- subset(extinction, keepRows)
      for (k in seq_along(coords)) {
        attr(e[[k]], "alt") <- alt[keepRows]
        attr(e[[k]], "coords") <- coords[[k]]
      }

      gridSaod <- sapply(e,
        function(y)        {
          r <- NA

          ## Calculate vertical column integral of aerosol extinction.
          if (!all(is.na(y))) {
            r <- integratex_fn(attr(y, "alt"), y)$value
            ## Convert to 550 nm wavelength
            r <- r * 2.04
          }

          attr(r, "coords") <- attr(y, "coords")

          r
        },
 simplify = FALSE
      )

      ## Create a fresh grid for this day
      g <- pg

      dev_null <- sapply(
gridSaod,
        function(y)        {
          coords <- attr(y, "coords")
          lat <- coords["lat"]; long <- coords["long"]
          rc <- find_grid_fn(g, lat, long)
          if (any(is.na(rc))) return ()
          sq <- g[[rc["row"], rc["col"]]][[1]]
          if (all(is.na(sq)))
            g[[rc["row"], rc["col"]]][[1]] <<- as.vector(y)
          else
            g[[rc["row"], rc["col"]]][[1]] <<- c(sq, as.vector(y))
        }
      )

      ## From the global grid, create a data frame of mean values
      d <- sapply(g,
        function(y)        {
          r <- c(value = NA, weight = attr(y, "weight"))
          if (all(is.na(y[[1]]))) return (r)
          r["value"] <- mean(y[[1]], na.rm = TRUE)

          r
        },
 simplify = FALSE
      )

      d <- data.matrix(Reduce(rbind, d))
      saodToday <- stats::weighted.mean(d[, "value"], d[, "weight"], na.rm = TRUE)
      is.na(saodToday) <- is.nan(saodToday)
      saodTodayDf <- saodDailyTemplate
      saodTodayDf$day <- dayValue
      saodTodayDf$saod <- saodToday

      results[[j]] <- saodTodayDf
    }

    do.call(rbind, results)
  }

  cat(fill = TRUE)

  # Parallelize month processing with explicit function export
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    saodDailyList <- future.apply::future_lapply(
      names(x),
      process_month,
      x_data = x,
      pg = planetary_grid,
      integratex_fn = integratex,
      find_grid_fn = find_planetary_grid_square,
      future.packages = c("stringr", "stats"),
      future.seed = TRUE
    )
  } else {
    saodDailyList <- lapply(names(x), process_month,
                           x_data = x,
                           pg = planetary_grid,
                           integratex_fn = integratex,
                           find_grid_fn = find_planetary_grid_square
)
  }

  saodDaily <- do.call(rbind, saodDailyList)

  saod_daily <- saodDaily
  save(saod_daily, file = paste(rdata_path, daily_filename, sep = "/"))
}


#' @export
create_osiris_saod_data <- function(
  path = NULL,
  filename = "OSIRIS-Odin_Stratospheric-Aerosol-Optical_550nm.RData",
  series_name = "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global",
  create_daily = FALSE,
  ...
){
  if (is.null(path)) {
    if (!is.null(getOption("climeseries_saod_data_dir")))
      path <- getOption("climeseries_saod_data_dir")
    else
      path <- "."
  }
  if (create_daily)
    create_osiris_daily_saod_data(rdata_path = path, daily_filename = filename, ...)

  load(paste(path, filename, sep = "/"), envir = environment())

  r <- plyr::arrange(Reduce(
rbind,
    by(saod_daily, list(saod_daily$year, saod_daily$month),
      function(x) data.frame(
year = x$year[1], month = x$month[1], flit = mean(x$saod, na.rm = TRUE),
        check.names = FALSE, stringsAsFactors = FALSE
),
      simplify = FALSE
)
), year, month)
  r$yr_part <- r$year + (2 * r$month - 1)/24

  names(r)[names(r) %in% "flit"] <- series_name

  r
}

## usage:
# inst <- get_climate_data(download = FALSE, baseline = FALSE)
# allSeries <- list(
#   inst,
#   create_osiris_saod_data()
# )
# d <- Reduce(merge_fun_factory(all = TRUE, by = c(Reduce(intersect, c(list(climeseries::common_columns), lapply(allSeries, names))))), allSeries)
# series <- c("GISS Stratospheric Aerosol Optical Depth (550 nm) Global", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global")
# plot_climate_data(d, series, start = 1985, ylab = "SAOD", main = "Global Mean Stratospheric Aerosol Optical Depth")
## Save only OSIRIS data as CSV file.
# keepRows <- na_unwrap(d$`OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global`)
# write.csv(d[keepRows, c("year", "month", "yr_part", "OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global")], "./OSIRIS-SAOD_2001.11-2016.7.csv", row.names = FALSE)


## Crudely based on Cowtan et al. 2015, dx.doi.org/10.1002/2015GL064888.
#' @export
create_cmip5_tas_tos_data <- function(baseline=defaultBaseline, save_to_package=FALSE){
  data_dir <- system.file("extdata", package="climeseries")
  ensemble <- "cmip5"
  subdir <- "tas + tos"
  path <- paste(data_dir, ensemble, subdir, sep="/")

  ff <- list.files(path, "^.*?\\.dat$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
  modelSummary <- sapply(ff,
    function(a) {
      modelVariable <- stringr::str_match(a, "\\S(tas|tas land|tos)\\S")[2L]
      pathway <- stringr::str_match(a, "rcp(\\d{2})")[2L]
      modelType <- stringr::str_match(a, "\\S(all models|all members|one member per model)\\S")[2L]
      l <- readLines(a)
      re <- "^#.*?from\\s+(.*?),?\\s+(RCP|experiment|model).*$"
      modelLine <- grep(re, l, value=TRUE)
      model <- stringr::str_match(modelLine, re)[2L]

      r <- data.frame(
model=model, type=tolower(modelType), variable=tolower(modelVariable), RCP=as.numeric(pathway)/10, path=a,
        check.rows=FALSE, check.names=FALSE, fix.empty.names=FALSE, stringsAsFactors=FALSE
)

      r
    },
 simplify = FALSE
  )
  modelSummary <- Reduce(rbind, modelSummary)
  modelSummary <- subset(modelSummary, modelSummary$variable %in% c("tas land", "tos")) # Not necessary here, but for genericness.
  #xtabs(~ model + variable + type + RCP, modelSummary)

  l <- dlply(
modelSummary, ~ model + type + RCP,
    function(a)    {
      if (is.null(a) || length(unique(a$variable)) < 2)
        return (NULL)

      ## Earth is approx. 72% water, 28% land.
      weightMap <- c(`tas land`=0.28, tos=0.72)
      w <- weightMap[a$variable]
      w <- w / table(a$variable)[names(w)]

      ## Now read in the files to be averaged together. Returns a list of the separate time series.
      weightedValues <- mapply(a$model, w, a$path, seq(nrow(a)),
        FUN = function (model, weight, path, n)        {
          tab <- read.table(path)
          flit <- melt(tab[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
          for (i in names(flit)) flit[[i]] <- as.numeric(flit[[i]])
          flit <- arrange(flit, V1, month)
          x <- data.frame(year=flit$V1, met_year=NA, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=flit$temp, check.names=FALSE, stringsAsFactors=FALSE)

          modelDesignation <- "m" %_% sprintf("%04d", n)
          x[[modelDesignation]] <- x$temp

          if (!is.null(baseline)) {
            flit <- subset(x, x$year %in% baseline)
            bma <- tapply(flit$temp, flit$month, mean, na.rm=TRUE)
            x$base <- NA
            l_ply(names(bma), function(s) {
 v <- bma[s]; if (is.nan(v)) v <- 0.0; x$base[x$month == s] <<- v
 })

            ## Center anomalies on average baseline-period temperatures.
            x[[modelDesignation]] <- round(x$temp - x$base, 3L)
          }
          x <- x[, c(common_columns, modelDesignation)]
          ## Weight the series.
          x[[modelDesignation]] <- x[[modelDesignation]] * weight
          attr(x, "weight") <- weight
          attr(x, "model") <- model

          x
        }, SIMPLIFY = FALSE
      )

      m <- Reduce(merge_fun_factory(all=TRUE, by=common_columns), weightedValues)
      modelDesignation <- paste0(unique(a$model), "_", unique(a$RCP))
      m[[modelDesignation]] <- rowSums(m[, colnames(m) %nin% common_columns])

      m[, c(names(m)[names(m) %in% common_columns], modelDesignation)]
    }
  )

  keepElements <- !sapply(l, is.null)
  modelDetails <- subset(attr(l,"split_labels"), keepElements)
  m <- l[keepElements]

  m <- Reduce(merge_fun_factory(all=TRUE, by=common_columns), m)
  m <- recenter_anomalies(m, baseline=baseline) # Is this necessary?

  ## Make similar to previously made model objects.
  colNames <- colnames(m)
  colnames(m)[colNames %nin% common_columns] <- "m" %_% sprintf("%04d", seq(sum(colNames %nin% common_columns)))

  attr(m, "ensemble") <- "CMIP5"

  attr(m, "model_type") <- subdir

  attr(m, "model") <- modelDetails$model

  scenario <- "RCP " %_% sprintf(modelDetails$RCP, fmt="%.1f")
  names(scenario) <- colnames(m)[colNames %nin% common_columns]
  attr(m, "scenario") <- factor(scenario)

  cmip5 <- m
  if (save_to_package) {
    save(cmip5, file=paste(path, "cmip5.RData", sep="/"))
    save(cmip5, file=paste(path, "cmip5_raw.RData", sep="/")) # Not really "raw," but oh well.
  }
  ## To create the package data set:
  # m <- create_cmip5_tas_tos_data(save_to_package=TRUE)

  cmip5
}

## usage:
# inst <- get_climate_data(download=FALSE, baseline=TRUE)
# cmip5 <- get_models_data(ensemble="cmip5", subdir="tas + tos")
# series <- c("HadCRUT4 Global")
## Like Fig. 4(b) of Cowtan et al. 2015:
# plot_models_and_climate_data(inst, cmip5, series=series, scenario="RCP 8.5", start=1880, end=2020, ma=12, ma_i=12, baseline=1961:1990, center_fun="mean", smooth_envelope=FALSE, envelope_type="range", envelope_text="range", ylim=c(-1.0, 1.5), conf_int_i=FALSE, col_i_fun="topo.colors", col_i_fun...=list())


## https://crudata.uea.ac.uk/cru/data/temperature/read_cru_hemi.r
read_cru_hemi <- function(filename){
  # read in whole file as table
  tab <- read.table(filename, fill = TRUE)
  nrows <- nrow(tab)
  # create frame
  hemi <- data.frame(
    year = tab[seq(1, nrows, 2), 1],
    annual = tab[seq(1, nrows, 2), 14],
    month = tab[seq(1, nrows, 2), 2:13] %>% `colnames<-`(seq(NCOL(.))),
    cover = tab[seq(2, nrows, 2), 2:13] %>% `colnames<-`(seq(NCOL(.)))
  )
  # mask out months with 0 coverage
  hemi$month.1[which(hemi$cover.1 == 0)] <- NA
  hemi$month.2[which(hemi$cover.2 == 0)] <- NA
  hemi$month.3[which(hemi$cover.3 == 0)] <- NA
  hemi$month.4[which(hemi$cover.4 == 0)] <- NA
  hemi$month.5[which(hemi$cover.5 == 0)] <- NA
  hemi$month.6[which(hemi$cover.6 == 0)] <- NA
  hemi$month.7[which(hemi$cover.7 == 0)] <- NA
  hemi$month.8[which(hemi$cover.8 == 0)] <- NA
  hemi$month.9[which(hemi$cover.9 == 0)] <- NA
  hemi$month.10[which(hemi$cover.10 == 0)] <- NA
  hemi$month.11[which(hemi$cover.11 == 0)] <- NA
  hemi$month.12[which(hemi$cover.12 == 0)] <- NA
  #
  return(hemi)
}


