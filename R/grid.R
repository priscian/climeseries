## Aided by the description at http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/map/download.html.
#' @export
#' @import plyr
make_planetary_grid <- function(
  lat_range = c(90, -90),
  long_range = c(0, 0),
  grid_size = c(5, 5),
  clockwise = FALSE, reverse_long = FALSE,
  container = list(structure(list(c(NA)), weight = 1.0)),
  digits = 3,
  use_lat_weights = TRUE,
  use_lat_zonal_weights = TRUE,
  lat_zonal_weights =
    list(
      # list(range = c(90, 23.6), weight = 0.3),
      # list(range = c(23.6, -23.6), weight = 0.4),
      # list(range = c(-23.6, -90), weight = 0.3)
      list(range = c(90, 0.1), weight = 0.68),
      list(range = c(-0.1, -90), weight = 0.32)
    )
){
  ## N.B. 90° N = +90° lat; 90° S = -90° lat; 180° W = -180° long; 180° E = +180° long.

  AllSame <- function(x, tol = .Machine$double.eps ^ 0.5) abs(max(x) - min(x)) < tol

  if (length(grid_size) == 1L)
    grid_size <- rep(grid_size[1], 2L)
  latSize <- grid_size[1]; longSize <- grid_size[2]

  GetShortArcMidpointValues <- function(r, g)  {
    r <- sort(r)
    signr <- sign(r)
    if (AllSame(signr)) {
      if (AllSame(r)) mr <- r
      else
        mr <- r + c(1, -1) * (g / 2)
    } else {
      if (any(signr == 0)) signr[signr == 0] <- -sum(signr)
      mr <- r - signr * (g / 2)
    }

    mv <- seq(mr[1], mr[2], by = g)

    mv
  }

  latValues <- GetShortArcMidpointValues(lat_range, latSize)
  if (diff(lat_range) < 0)
    latValues <- sort(latValues, decreasing = TRUE)

  GetLongMidpointValues <- function(r, g, clockwise)  {
    if ((diff(r) > 0 && !clockwise) || (diff(r) <= 0 && clockwise)) {
      mv <- GetShortArcMidpointValues(r, g)
      if (diff(r) < 0)
        mv <- sort(mv, decreasing = TRUE)
    } else {
      mv <- c(
        sort(GetShortArcMidpointValues(c(r[1], ((2 * !clockwise) - 1) * 180), g), decreasing = clockwise),
        sort(GetShortArcMidpointValues(c((2 * clockwise - 1) * 180, r[2]), g), decreasing = clockwise)
      )
    }

    ## Reversing the order of long. values might sometimes be necessary for complete arcs, i.e. same start and end values.
    if (reverse_long) mv <- rev(mv)

    mv
  }

  longValues <- GetLongMidpointValues(long_range, longSize, clockwise)

  g <- matrix(container, length(latValues), length(longValues),
    dimnames = list(round(latValues, digits), round(longValues, digits))
)

  ## Add latitude-weight attributes to row elements.
  ## V. https://stackoverflow.com/questions/58881607/calculating-the-cosine-of-latitude-as-weights-for-gridded-data/58883457#58883457
  if (use_lat_weights) {
    w <- cos(matrix(rep(latValues, NCOL(g)), ncol = NCOL(g), byrow = FALSE) * (pi / 180)) # Latitude weights
    plyr::m_ply(expand.grid(r_ = seq(NROW(g)), c_ = seq(NCOL(g))), function(r_, c_) attr(g[[r_, c_]], "weight") <<- w[r_, c_])
  }

  ## Create weights for latitude zones.
  ## V. http://rankexploits.com/musings/2010/the-great-gistemp-mystery/
  if (use_lat_zonal_weights) {
    grid_lats <- as.numeric(dimnames(g)[[1]])
    zone_weights <- rep(1.0, length(grid_lats))

    plyr::l_ply(
seq_along(lat_zonal_weights),
      function(a)      {
        zone_weight <- lat_zonal_weights[[a]]$weight
        lat_range_i <- sapply(lat_zonal_weights[[a]]$range, function(b) nearest(grid_lats, b))

        zone_weights[Reduce(`:`, lat_range_i)] <<- zone_weight
        #zone_weights[Reduce(`:`, lat_range_i)] <<- zone_weight/length(Reduce(`:`, lat_range_i))
      }
)

    ## Also create a cell attribute.
    zw <- matrix(rep(zone_weights, each = NCOL(g)), ncol = NCOL(g), byrow = TRUE)
    plyr::m_ply(
expand.grid(r_ = seq(NROW(g)), c_ = seq(NCOL(g))),
      function(r_, c_) attr(g[[r_, c_]], "zone_weight") <<- zw[r_, c_]
)

    names(zone_weights) <- dimnames(g)[[1]]
    attr(g, "zone_weights") <- zone_weights
  }

  attr(g, "grid_size") <- grid_size; names(attr(g, "grid_size")) <- c("lat", "long")
  attr(g, "lat_range") <- lat_range
  attr(g, "long_range") <- long_range
  class(g) <- "PlanetaryGrid"

  g
}

## usage:
# g <- make_planetary_grid() # Default complete globe after JMA, 90N–90S, 0W–0E.

#' @export
find_planetary_grid_square <- function(p, lat, long){
  if (!inherits(p, "PlanetaryGrid"))
    stop("'p' must be a \"PlanetaryGrid\" object.")

  gridLatValues <- as.numeric(rownames(p)); gridLongValues <- as.numeric(colnames(p))
  gridRow <- which.min(abs(lat - gridLatValues))
  gridCol <- which.min(abs(long - gridLongValues))

  gridSize <- attr(p, "grid_size")
  if (abs(gridLatValues[gridRow] - lat) > gridSize[1] / 2) {
 gridRow <- NA
 }
  if (abs(gridLongValues[gridCol] - long) > gridSize[2] / 2) {
 gridCol <- NA
 }

  c(row = gridRow, col = gridCol)
}

## usage:
# g <- make_planetary_grid()
# find_planetary_grid_square(g, 60.15, 110.82)


#' @export
make_coverage_filter <- function(
  ghcn,
  coverage_years = NULL,
  min_nonmissing_months = 12, # 12 for no missings
  min_nonmissing_years_prop = 0.9 # 1.0 for no missings
){
  ## Default:
  meets_filter_criteria <-
    structure(rep(TRUE, length(get_climate_series_names(ghcn))),
      .Names = get_climate_series_names(ghcn)
)

  if (!is.null(coverage_years)) {
    min_nonmissing_years <- round(length(coverage_years) * min_nonmissing_years_prop)

    flit <- ghcn %>%
      dplyr::filter(year %in% coverage_years)

    meets_filter_criteria <- Reduce(rbind, by(
flit[, get_climate_series_names(flit)], flit$year,
      function(i)      {
        (!(is.na(i))) %>% colSums(na.rm = TRUE)
      }
)) %>%
 (function(x) {
 rownames(x) <- NULL; x
 }) %>%
      (function(x)      {
        if (!is.matrix(x)) x <- t(x)
        (x >= min_nonmissing_months) %>%
          colSums(na.rm = TRUE) >= min_nonmissing_years
      })
  }

  meets_filter_criteria
}


## Return a subset of metadata based on search criteria.
#' @export
metadata_select <- function(
  x, # GHCN station metadata object
  m # Search expression()
){}
## This function is probably unnecessary; it's more flexible to use dplyr filtering/selecting.


## Returns station counts & related data to simplify plotting
#' @export
get_station_counts <- function(
  x, # Temp series created by 'make_ghcn_temperature_series()'
  env = globalenv(), # Environment of 'x' & its metadata
  baseline = 1951:1980,
  region_name = "Regional",
  make_plot = TRUE,
  start_year = NULL, end_year = NULL, # Can take fractions of a year
  unwrap = TRUE,
  suffix = "",
  save_png = FALSE, png... = list(),
  plot_climate_data... = list()
){
  station_names <- sapply(
attr(x, "planetary_grid"),
    function(a) if (is.data.frame(a[[1]])) names(a[[1]])
) %>%
 purrr::flatten() %>%
 unlist
  station_names_re <- stringr::str_flatten(rex::escape(station_names), "|")
  m <- env$station_metadata %>%
    dplyr::filter(stringr::str_detect(id, stringr::regex(station_names_re, ignore_case = TRUE), negate = FALSE))
  #m <- attr(x, "filtered_metadata")
  g0 <- env$ghcn[, c(get_climate_series_names(env$ghcn, invert = FALSE), m$id)]

  N <- g0 %>%
 dplyr::select(c(get_climate_series_names(g0, invert = TRUE))) %>%
 is.na %>%
 `!` %>%
 rowSums
  ss <- g0 %>%
 dplyr::select(c(get_climate_series_names(g0, invert = FALSE))) %>%
 dplyr::mutate(`station count` = N)
  if (unwrap)
    ss %<>% dplyr::filter(na_unwrap(dplyr::pull(naniar::replace_with_na_at(.,
 .vars = "station count",
      .condition ~ .x == 0
), `station count`)))

  if (make_plot) {
    plot_climate_dataArgs <- list(
      x = ss,
      series = "station count",
      start = start_year, end = end_year,
      type = "p", col = "blue", pch = 1,
      main = sprintf("GHCN %s Station Counts", region_name),
      ylab = "Number of stations",
      legend... = list(lty = 0, pch = 1),
      make_standardized_plot_filename... =
        list(suffix = sprintf("_%s_%s%s", tolower(region_name), make_current_timestamp(use_seconds = TRUE), suffix)),
      png... = png..., save_png = save_png
    )
    plot_climate_dataArgs <- utils::modifyList(plot_climate_dataArgs, plot_climate_data..., keep.null = TRUE)

    do.call(plot_climate_data, plot_climate_dataArgs)
  }

  list(
    metadata = m,
    station_series = g0,
    station_counts_series = ss
  )
}


## Starting w/ a random station, select n total that are maximally separated on the globe.
#' @export
get_random_stations <- function(
  n = 30, # No. total stations to be selected
  starting_station = NULL, # Leave NULL for random selection.
  rng_seed = 666
){
  ## https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates
}


## Informal look into gridded data used to make temp series 'x'.
#' @export
grid_info <- function(
  x, # Temp series created by 'make_ghcn_temperature_series()'
  env = globalenv(), # Environment of 'x' & its metadata
  info = c(
    "counts", # Station count/cell, dimnames or not depending on 'label'
    "temps", # Show data for cell 'elm' (which can be 2-D)
    "metadata", # Metadata for stations in cell 'elm' (can be 2-D)
    "coords" # lat,lon of cell 'elm'
  ),
  elm = NULL,
  labels = TRUE
){
  info <- match.arg(info)

  p <- attr(x, "planetary_grid")
  m <- env$station_metadata

  w <- switch(info,
    counts = {
      if (labels)
        structure(sapply(
p,
          function(x) {
 r <- x[[1]]; if (is.data.frame(r)) r <- NCOL(r); r
 }
), .Dim = dim(p), .Dimnames = dimnames(p))
      else
        structure(sapply(p, function(x) {
 r <- x[[1]]; if (is.data.frame(r)) r <- NCOL(r); r
 }), .Dim = dim(p))
    },
    temps = {
      eval_js(sprintf("p[%s][[1]][[1]]", paste(elm, collapse = ", ")))
    },
    metadata = {
      eval_js(sprintf("m[m$id %%in%% colnames(p[%s][[1]][[1]]), ]", paste(elm, collapse = ", ")))
    },
    coords = {
      eval_js(sprintf(
"structure(apply(expand.grid(dimnames(p)), 1, paste, collapse = ','), .Dim = dim(p))[%s]",
        paste(elm, collapse = ", ")
))
    }
  )

  w
}

## usage:
# grid_info(adj, ghcn_v3_avg_a, "counts", labels = TRUE)
# grid_info(adj, ghcn_v3_avg_a, "temps", elm = c(10, 21)) # Can also be 1-D, i.e. 21.
# grid_info(adj, ghcn_v3_avg_a, "metadata", elm = c(10, 21)) # Can also be 1-D.
# grid_info(adj, ghcn_v3_avg_a, "coords", elm = c(10, 21)) # Can also be 1-D.


#' @export
plot_stations_map <- function(
  metadata,
  region_name = "global",
  title_text = sprintf("GHCN-m %s station distribution", region_name),
  save_png = FALSE, save_png_dir, png... = list(),
  ...
){
  ## Station distribution
  world <- ggplot2::map_data("world")
  station_map <- ggplot2::ggplot() +
    ggplot2::geom_map(
      data = world, map = world,
      ggplot2::aes(long, lat, map_id = region),
      color = "white", fill = "lightgray", size = 0.1
    ) +
    ggplot2::geom_point(
      data = metadata,
      ggplot2::aes(longitude, latitude, color = "red"),
      alpha = 0.7
    ) +
    ggplot2::ggtitle(title_text) +
    #ggplot2::ggtitle(sprintf("GHCN-m station distribution %s–%s", min(coverage_years), max(coverage_years))) +
    ggplot2::theme(legend.position = "none")

  if (missing(save_png_dir)) {
    if (!is.null(getOption("climeseries_image_dir")))
      imageDir <- getOption("climeseries_image_dir")
    else
      imageDir <- "."
  } else
    imageDir <- save_png_dir

  filename <- sprintf("%s-stations_%s.png", region_name, make_current_timestamp(use_seconds = TRUE)) %>%
    fs::path_sanitize(replacement = "#")

  if (save_png) {
    pngArgs <- list(
      filename = paste(imageDir, filename, sep = "/"),
      width = 12.5,
      height = 7.3,
      units = "in",
      res = 600
    )
    pngArgs <- utils::modifyList(pngArgs, png..., keep.null = TRUE)
    do.call("png", pngArgs)
  }

  if (dev.cur() == 1L) # If a graphics device is active, plot there instead of opening a new device.
    dev.new(width = 12.5, height = 7.3) # New default device of 1200 × 700 px at 96 DPI.
  station_map %>% print

  if (save_png)
    dev.off()

  cat("Standardized file name:", stringr::str_replace_all(filename, "%%", "%"), fill = TRUE); flush.console()

  return (invisible(station_map))
}

#' @export
create_timeseries_from_gridded <- function(
  x,
  sub_lat = c(-90, 90), sub_long = c(-180, 180),
  data_dir = getOption("climeseries_data_dir"),
  series_suffix = NULL
){
  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)

  if (is.null(data_dir)) data_dir <- getwd()

  ## To be continued!
}


## Can I add these?
## UAH gridded: https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00961
## RSS gridded: https://www.remss.com/measurements/upper-air-temperature/
#' @export
create_zonal_data <- function(
  x, # series from call to 'get_climate_data()'
  sub_lat = c(-90, 90), sub_long = c(-180, 180),
  what = c("hadcrut", "hadcrut4", "hadsst", "crutem", "cw", "be"),
  data_dir = getOption("climeseries_data_dir"),
  metadata = list( # Names should be same as 'what' options
    ## HadCRUT4 url: https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.4.6.0.0.median.nc
    hadcrut = list(
      url = "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc",
      tempvar = "tas_mean",
      series = "HadCRUT5"
    ),
    hadcrut4 = list(
      url = "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.4.6.0.0.median.nc",
      tempvar = "temperature_anomaly",
      series = "HadCRUT4"
    ),
    hadsst = list(
      url = "https://www.metoffice.gov.uk/hadobs/hadsst4/data/netcdf/HadSST.4.1.0.0_median.nc",
      tempvar = "tos",
      series = "HadSST4"
    ),
    crutem = list(
      url = "https://crudata.uea.ac.uk/cru/data/temperature/CRUTEM.5.0.2.0.anomalies.nc",
      tempvar = "tas",
      series = "CRUTEM5"
    ),
    cw = list(
      url = "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/had4_krig_v2_0_0.nc.gz",
      tempvar = "temperature_anomaly",
      series = "Cowtan & Way Krig. Land+SST"
    ),
    be = list(
      url = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Gridded/Land_and_Ocean_LatLong1.nc",
      tempvar = "temperature",
      series = "Berkeley Earth Land+SST (Air Ice Temp.)"
    ),
    be_land = list(
      url = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Gridded/Complete_TAVG_LatLong1.nc",
      tempvar = "temperature",
      series = "Berkeley Earth Land"
    ),
    be_land_tmax = list(
      url = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Gridded/Complete_TMAX_LatLong1.nc",
      tempvar = "temperature",
      series = "Berkeley Earth Land TMAX"
    ),
    be_land_tmin = list(
      url = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Gridded/Complete_TMIN_LatLong1.nc",
      tempvar = "temperature",
      series = "Berkeley Earth Land TMIN"
    )
    ## TODO:
    # https://www.ncei.noaa.gov/data/noaa-global-surface-temperature/v6/access/gridded/
    # https://data.giss.nasa.gov/gistemp/
  ),
  series_suffix = NULL,
  use_local = FALSE
){
  what <- match.arg(what)

  if (missing(x))
    x <- get_climate_data(download = FALSE, baseline = FALSE)
  mergeWithOtherData <- TRUE
  if (is.null(x))
    mergeWithOtherData <- FALSE

  if (is.null(data_dir)) data_dir <- getwd()

  gurl <- metadata[[what]]$url
  tempVar <- metadata[[what]]$tempvar
  series <- metadata[[what]]$series

  filePathTemplate <- sprintf("%s/%%s", data_dir)
  filePath <- sprintf(filePathTemplate, basename(gurl))

  filePath <- switch(what,
    cw = {
      if (!use_local || !file.exists(filePath))
        download.file(gurl, filePath, mode = "wb", quiet = TRUE)
      R.utils::gunzip(filePath, overwrite = TRUE, remove = FALSE)
      flit <- basename(tools::file_path_sans_ext(gurl))
      filePath <- sprintf(filePathTemplate, flit)

      filePath
    },
    hadcrut =,
    hadcrut4 =,
    hadsst =,
    crutem =,
    be = {
      if (!use_local || !file.exists(filePath))
        download.file(gurl, filePath, mode = "wb", quiet = FALSE)

      filePath
    }
  )

  n <- ncdf4::nc_open(filePath) # 'print(n)' or just 'n' for details.
  a <- ncdf4::ncvar_get(n, tempVar)
  ## Structure of 'a' is temperature_anomaly[longitude, latitude, time], 72 × 36 × Inf (monthly since Jan. 1850)
  lat <- ncdf4::ncvar_get(n, "latitude")
  long <- ncdf4::ncvar_get(n, "longitude")
  times <- ncdf4::ncvar_get(n, "time")
  tunits <- ncdf4::ncatt_get(n,"time", "units")
  ncdf4::nc_close(n)

  if (what == "be") {
    tunits
    # $value
    # [1] "year A.D."
    r <- range(trunc(times))
    flit <- expand.grid(month = 1:12, year = seq(r[1], r[2], by = 1))
    flit$yr_part <- flit$year + (2 * flit$month - 1)/24
    flit <- data.table::data.table(flit)
    ## This data set should have the same NROW as the "time" dimension of 'a':
    h <- flit[data.table::data.table(yr_part = times), roll = "nearest", on = "yr_part"] %>%
      as.data.frame %>%
 dplyr::select(year, month)
  } else {
    tunits
    # $value
    # [1] "days since 1850-1-1 00:00:00"
    dtimes <- as.Date(times, origin = "1850-01-01")
    ## This data set should have the same NROW as the "time" dimension of 'a':
    h <- dataframe(year = year(dtimes), month = month(dtimes))
  }

  flit <- apply(
a, 3,
    function(y)    {
      x <- t(y)
      w <- cos(matrix(rep(lat, NCOL(x)), ncol = NCOL(x), byrow = FALSE) * (pi / 180)) # Latitude weights.

      ## Use only subgrid for calculations.
      keepSubGrid <- list(
        lat = lat >= sub_lat[1] & lat <= sub_lat[2],
        long = long >= sub_long[1] & long <= sub_long[2]
      )
      x1 <- x[keepSubGrid$lat, keepSubGrid$long, drop = FALSE]
      w1 <- w[keepSubGrid$lat, keepSubGrid$long, drop = FALSE]

      nlat <- length(lat[keepSubGrid$lat])
      temp <- NULL
      for (i in seq(1L, nrow(x1), by = nlat)) {
        xi <- data.matrix(x1[i:(i + nlat - 1L), ])
        tempi <- stats::weighted.mean(xi, w1, na.rm = TRUE)

        temp <- c(temp, tempi)
      }

      temp
    }
)
  is.na(flit) <- is.nan(flit)

  lat_long_to_text <- function(x, sufs) {
 suf <- sufs[2]; r <- abs(x); if (x < 0) suf <- sufs[1]; r %_% suf
 }
  subLatText <- sapply(sub_lat, lat_long_to_text, sufs = c("S", "N"), simplify = TRUE)
  subLongText <- sapply(sub_long, lat_long_to_text, sufs = c("W", "E"), simplify = TRUE)

  if (is.null(series_suffix))
    seriesOut <- paste0(series, " (", paste(subLatText, collapse = "-"), ", ", paste(subLongText, collapse = "-"), ")")
  else
    seriesOut <- paste0(series, series_suffix)

  h[[seriesOut]] <- flit
  if (mergeWithOtherData)
    x <- merge(x, h, by = c("year", "month"), all = TRUE)
  else
    x <- h %>% dplyr::mutate(yr_part = year + (2 * month - 1)/24, .after = "month")

  x
}

## usage:
# g <- create_zonal_data(what = "cw", use_local = FALSE)
# series <- c("HadCRUT5 Global", "HadCRUT5 (90S-90N, 180W-180E)")
# series <- c("BEST Global (Air Ice Temp.)", "BEST Global (Water Ice Temp.)", "BE Land+SST (Air Ice Temp.) (90S-90N, 180W-180E)")
# series <- c("Cowtan & Way Krig. Global", "Cowtan & Way Krig. Land+SST (90S-90N, 180W-180E)")
# plot_climate_data(g, series, yearly = TRUE) # These should mostly overlap.
#
# g <- create_zonal_data(what = "be", sub_lat = c(0, 90), use_local = TRUE)
# series <- c("BEST Global (Air Ice Temp.)", "BEST Global (Water Ice Temp.)", "BE Land+SST (Air Ice Temp.) (0N-90N, 180W-180E)")
# plot_climate_data(g, series, yearly = TRUE) # These should mostly overlap.
#
# g <- create_zonal_data(what = "be", sub_lat = c(-90, 0), use_local = TRUE)
# series <- c("BEST Global (Air Ice Temp.)", "BEST Global (Water Ice Temp.)", "BE Land+SST (Air Ice Temp.) (90S-0N, 180W-180E)")
# plot_climate_data(g, series, yearly = TRUE) # These should mostly overlap.


