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
      list(range = c(90, 23.6), weight = 0.3),
      list(range = c(23.6, -23.6), weight = 0.4),
      list(range = c(-23.6, -90), weight = 0.3)
    )
)
{
  ## N.B. 90° N = +90° lat; 90° S = -90° lat; 180° W = -180° long; 180° E = +180° long.

  AllSame <- function(x, tol = .Machine$double.eps ^ 0.5) abs(max(x) - min(x)) < tol

  if (length(grid_size) == 1L)
    grid_size <- rep(grid_size[1], 2L)
  latSize <- grid_size[1]; longSize <- grid_size[2]

  GetShortArcMidpointValues <- function(r, g)
  {
    r <- sort(r)
    signr <- sign(r)
    if (AllSame(signr)) {
      if (AllSame(r)) mr <- r
      else
        mr <- r + c(1, -1) * (g / 2)
    }
    else {
      if (any(signr == 0)) signr[signr == 0] <- -sum(signr)
      mr <- r - signr * (g / 2)
    }

    mv <- seq(mr[1], mr[2], by = g)

    mv
  }

  latValues <- GetShortArcMidpointValues(lat_range, latSize)
  if (diff(lat_range) < 0)
    latValues <- sort(latValues, decreasing = TRUE)

  GetLongMidpointValues <- function(r, g, clockwise)
  {
    if ((diff(r) > 0 && !clockwise) || (diff(r) <= 0 && clockwise)) {
      mv <- GetShortArcMidpointValues(r, g)
      if (diff(r) < 0)
        mv <- sort(mv, decreasing = TRUE)
    }
    else {
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

  g <- matrix(container, length(latValues), length(longValues), dimnames = list(round(latValues, digits), round(longValues, digits)))

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

    plyr::l_ply(seq_along(lat_zonal_weights),
      function(a)
      {
        zone_weight <- lat_zonal_weights[[a]]$weight
        lat_range_i <- sapply(lat_zonal_weights[[a]]$range, function(b) nearest(grid_lats, b))

        zone_weights[Reduce(`:`, lat_range_i)] <<- zone_weight
        #zone_weights[Reduce(`:`, lat_range_i)] <<- zone_weight/length(Reduce(`:`, lat_range_i))
      })

    ## Also create a cell attribute.
    zw <- matrix(rep(zone_weights, each = NCOL(g)), ncol = NCOL(g), byrow = TRUE)
    plyr::m_ply(expand.grid(r_ = seq(NROW(g)), c_ = seq(NCOL(g))), function(r_, c_) attr(g[[r_, c_]], "zone_weight") <<- zw[r_, c_])

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

  c(row = gridRow, col = gridCol)
}

## usage:
# g <- make_planetary_grid()
# find_planetary_grid_square(g, 60.15, 110.82)


#' @export
get_series_from_ghcn_gridded <- function(
  ver = 3, # Or 2 or 4
  temp = c("avg", "min", "max", "mean"),
  quality = "u", # 'u' or 'a' for v3; 'u', 'f', 'e' for v4
  files = list(
    v2 = list(
      #base_url = "https://web.archive.org/web/19970808013347if_/http://www.ncdc.noaa.gov:80/pub/data/ghcn/v2/", # 14 May 1997
      base_url = "https://web.archive.org/web/20030124203721if_/http://www1.ncdc.noaa.gov:80/pub/data/ghcn/v2", # 10 Jan 2003
      archive = "v2.%s%s.Z",
      countries = "v2.country.codes",
      readme = "readme.temperature.Z",
      inv = "v2.temperature.inv"
    ),
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
  latest_modification_time = TRUE,
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
  if (!is.null(fileStrings$inv))
    uris$inv <- paste(fileStrings$base_url, fileStrings$inv, sep = "/")

  if (download) {
    fileList <- sapply(uris$archive,
      function(i)
      {
        archiveName <- paste(dataDir, basename(i), sep = "/")

        download.file(i, archiveName, mode = "wb", quiet = TRUE)

        if (ver == 2) {
          ghcnmSubdir <- sprintf("ghcnm.v2.0.0.%s", make_current_timestamp(fmt = "%Y%m%d"))
          if (!dir.exists(paste(dataDir, ghcnmSubdir, sep = "/")))
            dir.create(paste(dataDir, ghcnmSubdir, sep = "/"), recursive = TRUE)

          destfile <-
            paste(dataDir, ghcnmSubdir, tools::file_path_sans_ext(basename(archiveName)) %_% ".dat", sep= "/")
          unzip_Z(archiveName, destfile)

          paste(ghcnmSubdir, tools::file_path_sans_ext(basename(archiveName)) %_% ".dat", sep= "/")
        } else {
          untar(archiveName, exdir = dataDir, list = FALSE) # extras = "--overwrite" not supported

          untar(archiveName, list = TRUE)
        }
      }, simplify = FALSE)

    archiveDir <- paste(dataDir, dirname(fileList[[1]][1]), sep = "/")
    countries <- paste(archiveDir, fileStrings$countries, sep = "/")

    ## Download countries file.
    download.file(uris$countries, countries, mode = "wb", quiet = TRUE)

    if (!is.null(fileStrings$flags))
      download.file(paste(fileStrings$base_url, fileStrings$flags, sep = "/"), paste(archiveDir, fileStrings$flags, sep = "/"), mode = "wb", quiet = TRUE)

    readmePath <- paste(archiveDir, fileStrings$readme, sep = "/")
    download.file(uris$readme, readmePath, mode = "wb", quiet = TRUE)
    if (ver == 2) {
      unzip_Z(readmePath, tools::file_path_sans_ext(readmePath))

      ## For v2, download separate INV file
      invPath <- paste(archiveDir, tools::file_path_sans_ext(basename(fileList[[1]][1])) %_% ".inv", sep = "/")
      download.file(uris$inv, invPath, mode = "wb", quiet = TRUE)
    }
  }

  ## Now retrieve the latest files by searching for them.
  flit <- list.files(dataDir, sprintf("^ghcnm.v%s", ver), full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  archiveDirs <- sapply(flit, function(i) i[Vectorize(utils::file_test, vectorize.args = "x")("-d", i)], simplify = FALSE) %>% unlist()

  if (is_invalid(archiveDirs))
    stop("No valid GHCN-M directories found.")

  latest <- sort(archiveDirs, decreasing = TRUE)[1]
  if (latest_modification_time)
    latest <- archiveDirs[order(file.mtime(archiveDirs), decreasing = TRUE)][1]
  ver_ <- ver
  if (ver_ == 2) {
    flit <- temp; temp <- ver; ver <- flit
  }
  inv <- list.files(latest, sprintf(data_files_re, temp, ver, quality, "inv"), full.names = TRUE, ignore.case = TRUE)[1]
  dat <- list.files(latest, sprintf(data_files_re, temp, ver, quality, "dat"), full.names = TRUE, ignore.case = TRUE)[1]
  if (ver_ == 2) {
    flit <- temp; temp <- ver; ver <- flit
  }

  rdata_filepath <- paste(dataDir, tools::file_path_sans_ext(sprintf(fileStrings$archive, temp, quality), compression = TRUE) %_% ".RData", sep = "/")

  #browser()
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
        }, .keep = TRUE)

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
    #ghcn <- dplyr::bind_cols(y, as.data.frame(ghcn))

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
  uncertainty = TRUE, boot_seed = 666, boot... = list(),
  round_to_nearest = NULL, # NULL or ±a, where 'a' describes dist'n U(-a, +a),
  runif_seed = 666, use_runif = TRUE, # Use 'stats::runif()' instead of rounding when SD close to error range
  spreadsheet_path = NULL, # Set equal to a file path to make an Excel spreadsheet from the data
  use_weighted_median = FALSE
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
  has_baseline_coverage <- rep(TRUE, length(get_climate_series_names(ghcn)))
  if (!is.null(baseline)) {
    flit <- ghcn %>%
      dplyr::filter(year %in% baseline)

    has_baseline_coverage <- Reduce(rbind, by(flit[, get_climate_series_names(flit)], flit$year,
      function(i)
      {
        (!(is.na(i))) %>% colSums(na.rm = TRUE)
      })) %>% (function(x) { rownames(x) <- NULL; x }) %>%
      (function(x)
      {
        (x >= min_nonmissing_months) %>%
          colSums(na.rm = TRUE) >= min_nonmissing_years
      })
  }

  ## Use only stations that meet the baseline-coverage + other filters criteria.
  filters <- has_baseline_coverage & other_filters
  g <- ghcn[, c(common_columns, get_climate_series_names(ghcn)[filters]), drop = FALSE]

  ## N.B. Probably best to add uniform random noise here.
  if (!is.null(round_to_nearest)) {
    g <- g %>%
      dplyr::mutate_at(dplyr::vars(get_climate_series_names(.)),
        function(tt) {
          if (use_runif) {
            set.seed(runif_seed)

            tt + stats::runif(length(tt), -abs(round_to_nearest), abs(round_to_nearest))
          } else {
            2 * round_to_nearest * round(tt/(2 * round_to_nearest))
          }
        })
  }

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

  y <- g[, get_climate_series_names(g), drop = FALSE]

  plyr::l_ply(seq(NCOL(y)),
    function(z)
    {
      id <- names(y)[z]
      coords <- station_metadata[station_metadata$id == id, , drop = FALSE]
      elms <- y[, z, drop = FALSE]
      lat <- coords[["latitude"]]; long <- coords[["longitude"]]
      rc <- find_planetary_grid_square(p, lat, long)
      if (any(is.na(rc))) return ()
      sq <- p[[rc["row"], rc["col"]]][[1]]
      if (!is.data.frame(sq))
        p[[rc["row"], rc["col"]]][[1]] <<- elms
      else
        p[[rc["row"], rc["col"]]][[1]] <<- dplyr::bind_cols(sq, elms)
    })

  ## Make copy of the grid for possible further analysis.
  p0 <- rlang::duplicate(p, shallow = FALSE)

  ## Create average for each month for each grid cell.
  plyr::l_ply(seq_along(p),
    function(z)
    {
      #pDF <- p[z][[1]][[1]] # Alternative indexing
      pDF <- p[[z]][[1]]
      if (is.data.frame(pDF)) {
        row_means <- rowMeans(pDF, na.rm = TRUE)
        is.na(row_means) <- is.nan(row_means)
        p[[z]][[1]] <<- dataframe(mean = row_means)
      }
    })

  ## Aggregate time series into mean by latitude.
  r <- plyr::alply(p, 1,
    function(z)
    {
      zz <- plyr::llply(z, function(zz) { if (is.data.frame(zz[[1]])) return (zz[[1]]); NULL })
      zzz <- zz %>% purrr::compact()

      ll <- NULL
      if (!is_invalid(zzz)) {
        ll <- purrr::reduce(zzz, cbind) %>% data.matrix %>% rowMeans(na.rm = TRUE)
        is.na(ll) <- is.nan(ll)
      }

      ll
    })
  plyr::l_ply(seq_along(r), function(i) if (!is.null(r[[i]])) r[[i]] <<- structure(dataframe(r[[i]]), .Names = dimnames(p)[[1]][i]))

  rr <- purrr::reduce(r %>% purrr::compact(), cbind) %>% data.matrix

  ### Now handle both latitude weights & (potentially) zonal-latitude weights.

  ## Assume that latitude weights are same for all longitudes.
  weights <- apply(p, 1, function(a) attr(a[[1]], "weight"))[!sapply(r, is.null)]

  if (use_lat_zonal_weights) {
    zone_weights <- attr(p, "zone_weights")

    ## Is this right?
    flit <- zone_weights[!sapply(r, is.null)]
    lt1 <- table(flit)
    lt2 <- unique(flit)
    plyr::l_ply(seq_along(lt1), function(i) { flit[flit == lt2[i]] <<- lt2[i]/lt1[i] })

    ## This is probably better:
    rles <- zone_weights[!sapply(r, is.null)] %>% seqle(incr = 0)
    lat_zonal_weights <- mapply(rles$values, rles$lengths, FUN = function(a, b) { rep(a/b, b) }, SIMPLIFY = FALSE) %>% unlist

    ## V. https://math.stackexchange.com/questions/1910320/weighted-arithmetic-mean-with-2-or-more-weights-per-observation
    weights <- lat_zonal_weights * weights
  }

  if (!use_weighted_median)
    rrr <- apply(rr, 1, stats::weighted.mean, w = weights, na.rm = TRUE)
  else
    rrr <- apply(rr, 1, matrixStats::weightedMedian, w = weights, na.rm = TRUE)
  is.na(rrr) <- is.nan(rrr)

  gg <- g[, c("year", "month")] %>% dplyr::mutate(!!series_name := rrr)

  station_weights <- weighted_station_data <- unweighted_station_data <- NULL
  if (uncertainty) local({
    d <- sapply(t(p0), function(a) { if (is.data.frame(a[[1]])) return(a[[1]]); NULL }, simplify = FALSE) %>% purrr::compact() %>% purrr::reduce(dplyr::bind_cols) %>% data.matrix

    wc <- apply(p0, 1, function(a) sapply(a, function(b) { if (is.data.frame(b[[1]])) return (rep(1, NCOL(b[[1]]))/NCOL(b[[1]])); NULL }, simplify = FALSE)) %>% unlist %>% as.vector

    flit1 <- apply(p0, 1, function(a) { sapply(a, function(b) { if (is.data.frame(b[[1]])) { apply(b[[1]], 1, function(bb) { r <- bb; r[!is.na(r)] <- 1; r/sum(r, na.rm = TRUE) }) } }, simplify = FALSE) %>% purrr::compact() }) %>% purrr::compact(); names(flit1) <- colnames(rr)
    flit1a <- rapply(flit1, function(a) { if (!is.matrix(a)) return (as.matrix(a)); t(a) }, how = "replace")
    flit1b <- vector("list", length = sapply(flit1a, length) %>% sum)
    i <- 1; dev_null <- rapply(flit1a, function(a) { flit1b[[i]] <<- a; i <<- i + 1; NULL })
    # rapply(flit1b, NCOL) %>% length # No. non-empty cells
    # rapply(flit1b, NCOL) %>% sum # No. stations
    flit1c <- rapply(flit1a, function(a) { a[!is.na(a)] <- 1; a }, how = "replace")
    ## flit1c: List w/ elms for all non-empty cells by lat; each leaf elm contains matrix of all time points × all stations for that cell, 1 for non-missing.

    ####################
    ##### Weights based on the no. of stations in a cell:
    ####################
    wc1 <- purrr::reduce(flit1b, cbind) %>% data.matrix; colnames(wc1) <- NULL
    # apply(wc1, 2, min, na.rm = TRUE) == wc %>% as.vector

    ## Assume that latitude weights are same for all longitudes.
    wl0 <- apply(p0, 1, function(a) attr(a[[1]], "weight"))
    wl <- wl0[!sapply(r, is.null)]
    if (use_lat_zonal_weights)
      wl <- wl * lat_zonal_weights

    ## Aggregate stations by latitude.
    flit2 <- vector("list", length = dim(p0)[1])
    # sapply(flit2, length) %>% sum # No. non-empty cells
    # rapply(flit2, NCOL) %>% sum # No. stations
    i <- 0; plyr::a_ply(p0, 1, function(a) { i <<- i + 1; r <- sapply(a, function(b) { if (!is_invalid(b[[1]])) return (b[[1]]); NULL }, simplify = FALSE) %>% purrr::compact(); if (!is_invalid(r)) flit2[[i]] <<- r })
    flit2 <- flit2 %>% purrr::compact()
    flit2a <- sapply(flit2, function(a) { r <- Reduce(cbind, a) %>% data.matrix; r[!is.na(r)] <- 1; colnames(r) <- NULL; r }, simplify = FALSE)
    ## flit2a: List w/ elms for all non-empty latitudes; each elm contains matrix of all time points × all stations for that lat, 1 for non-missing.
    # sapply(flit2a, NCOL) %>% sum # Total no. of stations
    i <- 0; flit2b <- sapply(flit2a, function(a) { i <<- i + 1; r <- apply(a, 1, function(b) { (wl[i] * b)/sum(b, na.rm = TRUE) }); if (is.null(dim(r))) return (as.matrix(r)); t(r) }, simplify = FALSE)

    ####################
    ##### Weights based on differing no. of non-empty cells for each latitude:
    ####################
    flit2c <- Reduce(cbind, flit2b) %>% data.matrix; colnames(flit2c) <- NULL; wl1 <- flit2c
    # apply(wl1, 2, min, na.rm = TRUE) == wll %>% as.vector

    flit3 <- plyr::aaply(p0, 1, function(a) sapply(a, function(b) if (is.data.frame(b[[1]])) NCOL(b[[1]]) else NULL, simplify = FALSE), .drop = FALSE)
    lat_station_counts <- apply(flit3, 1, function(a) { r <- a %>% purrr::compact(); if (length(r) > 0) return (r %>% unlist %>% sum); NULL }) %>% unlist
    lat_cell_counts <- apply(flit3, 1, function(a) { r <- a %>% purrr::compact(); if (length(r) > 0) return (r %>% unlist %>% length); NULL }) %>% unlist
    lat_observations_weights <- sapply(seq_along(lat_cell_counts), function(i) { rep(lat_cell_counts[i]/sum(lat_cell_counts, na.rm = TRUE), lat_station_counts[i])/lat_station_counts[i] }, simplify = FALSE) %>% unlist

    ## Need to use 'flit1c' & 'flit2a' here.
    lat_station_counts1 <- plyr::llply(flit2a, rowSums, na.rm = TRUE)
    # sapply(lat_station_counts1, max) # Cf. lat_station_counts %>% as.vector
    lat_cell_counts1 <- sapply(seq_along(flit1c), function(i) { r <- sapply(flit1c[[i]], function(b) { r <- rowSums(b, na.rm = TRUE); r[r > 1] <- 1; r }) %>% rowSums }, simplify = FALSE)
    # sapply(lat_cell_counts1, max) # Cf. lat_cell_counts %>% as.vector
    sum_lat_cell_counts1 <- Reduce(cbind, lat_cell_counts1) %>% data.matrix %>% rowSums
    flit3a <- sapply(seq_along(lat_cell_counts1), function(i) { r <- lat_cell_counts1[[i]]/sum_lat_cell_counts1; is.na(r) <- is.nan(r); rr <- matrix(rep(r, lat_station_counts[i]), ncol = lat_station_counts[i], byrow = FALSE)/lat_station_counts1[[i]]; is.na(rr) <- is.nan(rr); rr }, simplify = FALSE)
    lat_observations_weights1 <- Reduce(cbind, flit3a) %>% data.matrix; colnames(lat_observations_weights1) <- NULL

    ####################
    ##### Weights based on differing no. of non-empty cells & stations for each latitude:
    ####################
    wcl1 <- 1/lat_observations_weights1

    w <- wc1 * wl1 * wcl1; colnames(w) <- get_climate_series_names(d) # Use this below to speed things up.
    if (!use_weighted_median)
      { i <- 0; dd <- apply(d, 1, function(a) { i <<- i + 1; stats::weighted.mean(a, w = w[i, ], na.rm = TRUE) }); is.na(dd) <- is.nan(dd) }
    else
      { i <- 0; dd <- apply(d, 1, function(a) { i <<- i + 1; matrixStats::weightedMedian(a, w = w[i, ], na.rm = TRUE) }); is.na(dd) <- is.nan(dd) }

    ## Test to make sure the series resulting from 'd' looks correct.
    ddd <- cbind(attr(p0, "time_coverage"), GHCN_orig = rrr, GHCN = dd)
    e <- get_climate_data(download = FALSE, baseline = FALSE)
    e <- purrr::reduce(list(e, ddd), merge, by = c("year", "month"), all.x = TRUE)
    baseline <- 1951:1980
    plot_climate_data(e, series = c("GHCN_orig", "GHCN"), 1880, yearly = TRUE, baseline = baseline, conf_int = FALSE, lwd = 1, ylim = NULL, save_png = FALSE)

    g <- data.table::data.table(cbind(attr(p0, "time_coverage"), d))
    ggg <- g[, get_climate_series_names(g), with = FALSE]

    ## N.B. This is MUCH faster than using an equivalent 'sapply()' call!
    i <- 0; h <- t(apply(ggg, 1, function(a) { i <<- i + 1; r <- a * (w[i, ]/sum(w[i, !is.na(a)])) * sum(!is.na(a)); is.na(r) <- is.nan(r); r })) %>% data.table::data.table()
    ## all.equal(dd, rowMeans(h, na.rm = TRUE)) # TRUE, only off by very small tolerance
    station_weights <<- w; unweighted_station_data <<- as.data.frame(ggg); weighted_station_data <<- as.data.frame(h)

    ## V. https://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
    bf <- function(x, i)
    {
      r <- mean(x[i], na.rm = TRUE)
      is.na(r) <- is.nan(r)

      r
    }

    bootArgs <- list(
      statistic = bf,
      R = 100
    )
    bootArgs <- utils::modifyList(bootArgs, boot..., keep.null = TRUE)

    set.seed(boot_seed)
    b <- apply(h, 1, function(a) { bootArgs$data <- a; do.call(boot::boot, bootArgs) })

    GHCN_uncertainty <- sapply(b,
      function(a)
      {
        tryCatch({
          #diff(boot::boot.ci(a, type = "norm")$normal[1, ][2:3])
          diff(boot::boot.ci(a, type = "basic")$basic[1, ][4:5])
        }, error = function(e) NA)
      }) %>% as.vector

    ## CIs based on the CLT:
    GHCN_uncertainty_clt <- sapply(b, function(a) sd(a$data, na.rm = TRUE)/sqrt(sum(!is.na(a$data)))) * 2 * 1.96

    gg[[series_name %_% "_uncertainty"]] <<- GHCN_uncertainty_clt
    gg[[series_name %_% "_uncertainty_boot"]] <<- GHCN_uncertainty
  })

  attr(gg, "planetary_grid") <- p0
  attr(gg, "filters") <- filters

  if (!is.null(spreadsheet_path)) {
    if (!dir.exists(dirname(spreadsheet_path)))
      dir.create(dirname(spreadsheet_path), recursive = TRUE)

    ## N.B. This takes a while!
    cat(sprintf("Creating spreadsheet %s...", basename(spreadsheet_path))); utils::flush.console()

    str_baseline <- stringr::str_flatten(range(baseline), collapse = "-")

    l <- list(
      `station-data` = ghcn %>% dplyr::select(c("month", "year", "yr_part", get_climate_series_names(.))),
      `station-metadata` = station_metadata
    )
    l[["stations_regional_base" %_% str_baseline]] <- g %>% dplyr::select(c("month", "year", "yr_part", get_climate_series_names(.)))
    l[["cell-counts" %_% stringr::str_flatten(sprintf("%.1f", attr(p0, "grid_size")), collapse = "x")]] <-
      structure(sapply(p0, function(x) { r <- x[[1]]; if (is.data.frame(r)) r <- NCOL(r); r }), .Dim = dim(p0), .Dimnames = dimnames(p0))
    if (!is.null(unweighted_station_data)) {
      l[["unweight-stations_base" %_% str_baseline]] <-
        dplyr::bind_cols(dplyr::select(g, c("month", "year", "yr_part")), unweighted_station_data[, colnames(unweighted_station_data), drop = FALSE])
    }
    if (!is.null(station_weights))
      l$`all-weights` <- station_weights
    if (!is.null(weighted_station_data)) {
      l[["weighted-stations_base" %_% str_baseline]] <-
        dplyr::bind_cols(dplyr::select(g, c("month", "year", "yr_part")), weighted_station_data[, colnames(station_weights), drop = FALSE])
    }
    l[["timeseries_base" %_% str_baseline]] <- gg %>% tibble::add_column(yr_part = .$year + (2 * .$month - 1)/24, .after = "month")

    rowNames <- rep(FALSE, length(l)); rowNames[4] <- TRUE # Make sure cell-counts grid has row names

    pathTemplate <- paste0(tools::file_path_sans_ext(spreadsheet_path), "_%s.", tools::file_ext(spreadsheet_path))
    ## This is no good; too many columns for Excel:
    # rio::export(head(l, 2), sprintf(pathTemplate, "a"), rowNames = head(rowNames, 2), colNames = TRUE)
    rio::export(l[[1]], sprintf(pathTemplate, "stations") %_% ".csv")
    rio::export(l[2], sprintf(pathTemplate, "metadata"), rowNames = rowNames[2], colNames = TRUE, overwrite = TRUE)
    rio::export(tail(l, -2), sprintf(pathTemplate, "analyzed"), rowNames = tail(rowNames, -2), colNames = TRUE, overwrite = TRUE)

    ## Put this list in the global environment in case I need it for anything.
    assign(series_name, l, envir = .GlobalEnv)

    cat(". Done.", fill = TRUE); utils::flush.console()
  }

  gg
}

## usage:
# get_series_from_ghcn_gridded(ver = 3, temp = "avg", quality = "a", download = FALSE)
# ghcn_v3_avg <- make_ghcn_temperature_series(ghcn, station_metadata, "GHCN v3 Global Land Adj. (30° × 30° cells)")


## Return a subset of metadata based on search criteria.
#' @export
metadata_select <- function(
  x, # GHCN station metadata object
  m # Search expression()
)
{

}
## This function is probably unnecessary; it's more flexible to use dplyr filtering/selecting.


## Returns station counts & related data to simplify plotting
#' @export
get_station_counts <- function(
  x, # Temp series created by 'make_ghcn_temperature_series()'
  env = globalenv(), # Environment of 'x' & its metadata
  baseline = 1951:1980,
  region_name = "Regional",
  spaghetti_sep = 1.0,
  make_plot = TRUE,
  start_year = NULL, end_year = NULL, # Can take fractions of a year
  save_png = FALSE,

  plot_climate_data... = list()
)
{
  station_names <- sapply(attr(x, "planetary_grid"), function(a) if (is.data.frame(a[[1]])) names(a[[1]])) %>% purrr::flatten() %>% unlist
  station_names_re <- stringr::str_flatten(rex::escape(station_names), "|")
  m <- env$station_metadata %>%
    dplyr::filter(stringr::str_detect(id, stringr::regex(station_names_re, ignore_case = TRUE), negate = FALSE))
  g0 <- env$ghcn[, c(get_climate_series_names(env$ghcn, invert = FALSE), m$id)]

  ## This is probably the best one can do in a spaghetti plot:
  g00 <- recenter_anomalies(g0, baseline)
  series00 <- get_climate_series_names(g00)
  m00 <- structure(m$id %>% as.character, .Names = trimws(m$name)) # id-to-name map

  duplicateNames <- names(m00) %>% intersect(.[duplicated(.)])
  for(i in duplicateNames) {
    dupIndex <- which(names(m00) == i)
    ## Replace w/ sequential numbers:
    # names(m00)[dupIndex] <- sapply(seq_along(dupIndex), function(j) sprintf("%s_%04d", names(m00)[dupIndex[j]], j))
    ## Replace w/ station ID:
    names(m00)[dupIndex] <- paste(names(m00)[dupIndex], m00[dupIndex], sep = "_")
  }

  g00 <- dplyr::rename(g00, !!!m00)
  dev_null <- mapply(get_climate_series_names(g00), seq(0, by = spaghetti_sep, length.out = length(series00)), FUN = function(a, b) { g00[[a]] <<- g00[[a]] + b; NULL })
  #plot_climate_data(g00, series = get_climate_series_names(g00), yearly = TRUE, baseline = NULL, conf_int = FALSE, lwd = 1, ylim = NULL, make_standardized_plot_filename... = list(suffix = ""), save_png = FALSE)

  N <- g00 %>% dplyr::select(c(get_climate_series_names(g00, invert = TRUE))) %>% is.na %>% `!` %>% rowSums
  ss <- g00 %>% dplyr::select(c(get_climate_series_names(g00, invert = FALSE))) %>% dplyr::mutate(`station count` = N)

  if (make_plot) {
    plot_climate_dataArgs <- list(
      x = ss,
      series = "station count",
      start = start_year, end = end_year,
      type = "p", col = "blue", pch = 1,
      main = sprintf("GHCN %s Station Counts", region_name),
      ylab = "Number of stations",
      legend... = list(lty = 0, pch = 1),
      make_standardized_plot_filename... = list(suffix = sprintf("_%s", tolower(region_name))),
      save_png = save_png
    )
    plot_climate_dataArgs <- utils::modifyList(plot_climate_dataArgs, plot_climate_data..., keep.null = TRUE)

    do.call(plot_climate_data, plot_climate_dataArgs)
    #plot_climate_data(ss, series = "station count", start_year, end_year, type = "p", col = "blue", pch = 1, main = sprintf("GHCN v4 %s Station Counts", region_name), ylab = "Number of stations", legend... = list(lty = 0, pch = 1), make_standardized_plot_filename... = list(suffix = sprintf("_%s", tolower(region_name))), save_png = save_png)
  }

  list(
    metadata = m,
    station_series = g0,
    station_id_name_map = m00,
    station_spaghetti_series = g00,
    station_counts_series = ss
  )
}


## Starting w/ a random station, select n total that are maximally separated on the globe.
#' @export
get_random_stations <- function(
  n = 30, # No. total stations to be selected
  starting_station = NULL, # Leave NULL for random selection.
  rng_seed = 666
)
{
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
