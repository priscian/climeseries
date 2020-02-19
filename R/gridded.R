#' @export
get_series_from_ghcn_gridded <- function(
  ver = 3, # Or 4
  temp = c("avg", "min", "max"),
  quality = "u", # 'u' or 'a' for v3; 'u', 'f', 'e' for v4
  files = list(
    v3 = list(
      base_url = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3",
      archive = "ghcnm.t%s.latest.qc%s.tar.gz",
      countries = "country-codes",
      readme = "README"
    ),
    v4 = list(
      base_url = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v4",
      archive = "ghcnm.t%s.latest.qc%s.tar.gz",
      countries = "ghcnm-countries.txt",
      flags = "ghcnm-flags.txt",
      readme = "readme.txt"
    )
  ),
  download = FALSE,
  data_dir = NULL, subdir = sprintf("ghcn-m/v%s", ver),
  data_files_re = "^ghcnm\\.t%s\\.v%s.*\\.qc%s\\.%s",
  na_strings = -9999,
  divide_by = 100.0,
  load_env = globalenv()
)
{
  archiveTempType <- match.arg(temp)
  v <- as.numeric(ver)[1]

  ## Time the operations.
  tictoc::tic(sprintf("Processing GHCN-M v%s T%s QC%s", ver, toupper(archiveTempType), toupper(quality)))

  dataDir <- ifelse(is.null(data_dir), getOption("climeseries_data_dir"), data_dir)
  if (!is.null(subdir) && trimws(subdir) != "")
    dataDir <- paste(dataDir, trimws(subdir), sep = "/")
  if (!dir.exists(dataDir))
    dir.create(dataDir, recursive = TRUE)

  fileStrings <- files[["v" %_% ver]]
  uris <- list(
    archive = paste(fileStrings$base_url, sprintf(fileStrings$archive, temp, quality), sep = "/"),
    countries = paste(fileStrings$base_url, fileStrings$countries, sep = "/"),
    readme = paste(fileStrings$base_url, fileStrings$readme, sep = "/")
  )

  if (download) {
    fileList <- sapply(uris$archive,
      function(i)
      {
        archiveName <- paste(dataDir, basename(i), sep = "/")

        download.file(i, archiveName, mode = "wb", quiet = TRUE)
        untar(archiveName, exdir = dataDir, list = FALSE) # extras = "--overwrite" not supported

        untar(archiveName, list = TRUE)
      }, simplify = FALSE)

    archiveDir <- paste(dataDir, dirname(fileList[[1]][1]), sep = "/")
    countries <- paste(archiveDir, fileStrings$countries, sep = "/")

    ## Download countries file.
    download.file(uris$countries, countries, mode = "wb", quiet = TRUE)

    if (!is.null(fileStrings$flags))
      download.file(paste(fileStrings$base_url, fileStrings$flags, sep = "/"), paste(archiveDir, fileStrings$flags, sep = "/"), mode = "wb", quiet = TRUE)

    download.file(uris$readme, paste(archiveDir, fileStrings$readme, sep = "/"), mode = "wb", quiet = TRUE)
  }

  ## Now retrieve the latest files by searching for them.
  flit <- list.files(dataDir, sprintf("^ghcnm.v%s", ver), full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  archiveDirs <- sapply(flit, function(i) i[Vectorize(utils::file_test, vectorize.args = "x")("-d", i)], simplify = FALSE) %>% unlist()

  if (is_invalid(archiveDirs))
    stop("No valid GHCN-M directories found.")

  latest <- sort(archiveDirs, decreasing = TRUE)[1]
  inv <- list.files(latest, sprintf(data_files_re, temp, ver, quality, "inv"), full.names = TRUE, ignore.case = TRUE)[1]
  dat <- list.files(latest, sprintf(data_files_re, temp, ver, quality, "dat"), full.names = TRUE, ignore.case = TRUE)[1]

  rdata_filepath <- paste(dataDir, tools::file_path_sans_ext(sprintf(fileStrings$archive, temp, quality), compression = TRUE) %_% ".RData", sep = "/")

  if (download) {
    ## Station metadata
    stationFormat <- c(11, -1, 8, -1, 9, -1, 6, -1, 30, -1, 4, 1, 5, 2, 2, 2, 2, 1, 2, 16, 1)
    station_metadata <- read.fwf(inv, widths = stationFormat, comment.char = "", stringsAsFactors = FALSE)
    stationCols <- read.table(text = '
      ID
      LATITUDE
      LONGITUDE
      STNELEV
      NAME
      GRELEV
      POPCLS
      POPSIZ
      TOPO
      STVEG
      STLOC
      OCNDIS
      AIRSTN
      TOWNDIS
      GRVEG
      POPCSS
    ', header = FALSE, stringsAsFactors = FALSE)[[1]] %>% tolower()
    colnames(station_metadata) <- stationCols

    if (v == 4)
      station_metadata <- station_metadata %>% dplyr::select(1:5)
    attr(station_metadata, "version") <- v
    attr(station_metadata, "temperature") <- temp
    attr(station_metadata, "quality") <- quality
    attr(station_metadata, "archive") <- uris$archive

    ### Station data

    dataFormat <- c(11, 4, 4, rep(c(5, 1, 1, 1), 12))
    station_data <- read.fwf(dat, widths = dataFormat, comment.char = "", na.strings = na_strings, stringsAsFactors = FALSE, n = -1)
    dataNames <- c("id", "year", "element", apply(expand.grid(c("value", "dmflag", "qcflag", "dsflag"), 1:12), 1, function(i) paste(trimws(i), collapse = "")))
    colnames(station_data) <- dataNames

    station_data_list <- station_data %>%
      dplyr::group_by(id) %>%
      dplyr::group_map(
        function(x, y)
        {
          xx <- x %>% tidyr::pivot_longer(
            cols = dplyr::matches("^(value|dmflag|qcflag|dsflag)"),
            names_to = c(".value", "month"),
            names_pattern = "(.*?)(\\d+)"
          )

          attr(xx, "groups") <- y

          xx
        }, keep = TRUE)

    station_names <- sapply(station_data_list, function(x) attr(x, "groups")[[1, 1]])
    names(station_data_list) <- station_names

    r <- range(c(sapply(station_data_list, function(x) range(x$year))))
    flit <- expand.grid(month = 1:12, year = seq(r[1], r[2], by = 1))

    ghcn <- sapply(station_data_list,
      function(i)
      {
        merge(flit, i[, c("year", "month", "value")], by = c("year", "month"), all = TRUE)[[3]] / divide_by
      }, simplify = TRUE)
    colnames(ghcn) <- station_names
    y <- flit %>%
      dplyr::mutate(yr_part = year + (2 * month - 1)/24, met_year = NA)
    ghcn <- cbind(y, ghcn, stringsAsFactors = FALSE)

    save(list = c("station_metadata", "station_data", "station_data_list", "ghcn"), file = rdata_filepath)
  } else {
    load(rdata_filepath, envir = load_env)
  }

  tictoc::toc()

  rdata_filepath
}


#' @export
make_ghcn_temperature_series <- function(
  ghcn, # GHCN station data in climeseries format
  station_metadata,
  series_name,
  other_filters = rep(TRUE, length(get_climate_series_names(ghcn))), # Other filters for station selection
  grid_size = c(30.0, 30.0),
  baseline = 1951:1980,
  lat_range = c(90, -90), long_range = c(-180, 180), # Default global coverage
  interpolate = FALSE,
  min_nonmissing_months = 10, # Over baseline period
  min_nonmissing_years = round(length(baseline) * 0.5), # Over baseline period
  make_planetary_grid... = list(),
  use_lat_zonal_weights = FALSE,
  lat_zonal_weights =
    list(
      list(range = c(90, 23.6), weight = 0.3),
      list(range = c(23.6, -23.6), weight = 0.4),
      list(range = c(-23.6, -90), weight = 0.3)
    )
)
{
  ver <- attr(station_metadata, "version")
  temp_type <- attr(station_metadata, "temperature")
  quality <- attr(station_metadata, "quality")

  LatLongToText <- function(x, sufs, template = "%.0f%s")
    { suf <- sufs[2]; r <- abs(x); if (x < 0) suf <- sufs[1]; sprintf(template, r, suf) }

  if (missing(series_name)) {
    latRangeText <- sapply(lat_range, LatLongToText, sufs = c("S", "N"), simplify = TRUE)
    longRangeText <- sapply(long_range, LatLongToText, sufs = c("W", "E"), simplify = TRUE)

    series_name <- sprintf("GHCN v%i %s, %s Land %s (%s cells%s)",
      ver,
      paste(latRangeText, collapse = "–"),
      paste(longRangeText, collapse = "–"),
      ifelse(quality %in% c("a", "f", "e"), "Adj.", "Raw"),
      paste(sapply(grid_size, sprintf, fmt = "%.1f°"), collapse = " × "),
      ifelse(use_lat_zonal_weights, " zoned", "")
    )
  }

  ## Which stations have adequate coverage over the baseline period?
  has_baseline_coverage <- sapply(get_climate_series_names(ghcn),
    function(i)
    {
      d <- ghcn[, c(common_columns, i)] %>%
        dplyr::filter(year %in% baseline) %>%
        dplyr::group_by(year) %>%
        dplyr::group_map(
          function(x, y)
          {
            sum(!is.na(x[[i]]))
          }) %>% unlist()

      sum(d >= min_nonmissing_months) >= min_nonmissing_years
    }, simplify = TRUE)

  ## Use only stations that meet the baseline-coverage + other filters criteria.
  filters <- has_baseline_coverage & other_filters
  g <- ghcn[, c(common_columns, names(filters)[filters]), drop = FALSE]
  if (!is.null(baseline))
    g <- recenter_anomalies(g, baseline = baseline)

  ### Create planetary grid of (grid_size[1])° × (grid_size[2])° squares and bin temp values in the correct square.

  make_planetary_gridArgs <- list(
    lat_range = lat_range,
    long_range = long_range,
    grid_size = grid_size,
    use_lat_weights = TRUE
  )
  make_planetary_gridArgs <- utils::modifyList(make_planetary_gridArgs, make_planetary_grid..., keep.null = TRUE)
  p <- do.call(make_planetary_grid, make_planetary_gridArgs)
  attr(p, "time_coverage") <- ghcn[, c("year", "month")]

  g <- data.table::data.table(g)
  y <- g[, get_climate_series_names(g), with = FALSE]

  plyr::l_ply(seq(NCOL(y)),
    function(z)
    {
      id <- names(y)[z]
      coords <- station_metadata[station_metadata$id == id, , drop = FALSE]
      elms <- y[, z, with = FALSE]
      lat <- coords[["latitude"]]; long <- coords[["longitude"]]
      rc <- find_planetary_grid_square(p, lat, long)
      if (any(is.na(rc))) return ()
      sq <- p[[rc["row"], rc["col"]]][[1]]
      if (!is.data.frame(sq))
        p[[rc["row"], rc["col"]]][[1]] <<- elms
      else
        p[[rc["row"], rc["col"]]][[1]] <<- cbind(sq, elms)
    })

  ## Make copy of the grid for possible further analysis.
  p0 <- rlang::duplicate(p, shallow = FALSE)

  ## Create average for each month for each grid cell.
  plyr::l_ply(seq(length(p)),
    function(z)
    {
      pDT <- p[z][[1]][[1]]
      if (is.data.frame(pDT)) {
        ## https://stackoverflow.com/questions/31258547/data-table-row-wise-sum-mean-min-max-like-dplyr
        row_means <- pDT[, `:=`(mean = rowMeans(.SD, na.rm = TRUE))][, .(mean)]
        is.na(row_means$mean) <- is.nan(row_means$mean)
        p[z][[1]][[1]] <<- row_means
      }
    })

  ## Aggregate time series into mean by latitude.
  r <- plyr::alply(p, 1,
    function(z)
    {
      zz <- plyr::llply(z, function(zz) { r <- zz[[1]]; if (!is.data.frame(r)) return (NULL); r })

      l <- purrr::reduce(zz, cbind)
      ll <- NULL
      if (!is.null(l)) {
        ll <- apply(l, 1, mean, na.rm = TRUE)
        is.na(ll) <- is.nan(ll)
      }

      ll
    })

  rr <- Reduce(cbind, r); dimnames(rr) <- NULL

  ### Now handle both latitude weights & (potentially) zonal-latitude weights.

  ## Assume that latitude weights are same for all longitudes.
  weights <- apply(p, 1, function(a) attr(a[[1]], "weight"))[!sapply(r, is.null)]

  ## Create weights for latitude zones.
  ## V. http://rankexploits.com/musings/2010/the-great-gistemp-mystery/
  grid_lats <- as.numeric(dimnames(p)[[1]])

  if (use_lat_zonal_weights) {
    zone_weights <- rep(1.0, length(grid_lats))

    plyr::l_ply(seq_along(lat_zonal_weights),
      function(a)
      {
        zone_weight <- lat_zonal_weights[[a]]$weight
        lat_range_i <- sapply(lat_zonal_weights[[a]]$range, function(b) nearest(grid_lats, b))

        zone_weights[Reduce(`:`, lat_range_i)] <<- zone_weight
      })

    ## V. https://math.stackexchange.com/questions/1910320/weighted-arithmetic-mean-with-2-or-more-weights-per-observation
    weights <- zone_weights[!sapply(r, is.null)] * weights
  }

  rrr <- apply(rr, 1, stats::weighted.mean, w = weights, na.rm = TRUE)
  is.na(rrr) <- is.nan(rrr)

  gg <- g[, c("year", "month")][, (series_name) := rrr] # Use parens around variable name with `:=()`
  attr(gg, "planetary_grid") <- p0
  attr(gg, "filters") <- filters

  gg
}

## usage:
# get_series_from_ghcn_gridded(ver = 3, temp = "avg", quality = "a", download = FALSE)
# ghcn_v3_avg <- make_ghcn_temperature_series(ghcn, station_metadata, "GHCN v3 Global Land Adj. (30° × 30° cells)")


## Informal look into gridded data used to make temp series 'x'.
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
)
{
  info <- match.arg(info)

  p <- attr(x, "planetary_grid")
  m <- env$station_metadata

  w <- switch(info,
    counts = {
      if (labels)
        structure(sapply(p, function(x) { r <- x[[1]]; if (is.data.frame(r)) r <- NCOL(r); r }), .Dim = dim(p), .Dimnames = dimnames(p))
      else
        structure(sapply(p, function(x) { r <- x[[1]]; if (is.data.frame(r)) r <- NCOL(r); r }), .Dim = dim(p))
    },

    temps = {
      eval_js(sprintf("p[%s][[1]][[1]]", paste(elm, collapse = ", ")))
    },

    metadata = {
      eval_js(sprintf("m[m$id %%in%% colnames(p[%s][[1]][[1]]), ]", paste(elm, collapse = ", ")))
    },

    coords = {
      eval_js(sprintf("structure(apply(expand.grid(dimnames(p)), 1, paste, collapse = ','), .Dim = dim(p))[%s]", paste(elm, collapse = ", ")))
    }
  )

  w
}

## usage:
# grid_info(adj, ghcn_v3_avg_a, "counts", labels = TRUE)
# grid_info(adj, ghcn_v3_avg_a, "temps", elm = c(10, 21)) # Can also be 1-D, i.e. 21.
# grid_info(adj, ghcn_v3_avg_a, "metadata", elm = c(10, 21)) # Can also be 1-D.
# grid_info(adj, ghcn_v3_avg_a, "coords", elm = c(10, 21)) # Can also be 1-D.
