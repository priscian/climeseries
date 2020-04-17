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

  c(row=gridRow, col=gridCol)
}

## usage:
# g <- make_planetary_grid()
# find_planetary_grid_square(g, 60.15, 110.82)


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
  if (latest_modification_time)
    latest <- archiveDirs[order(file.mtime(archiveDirs), decreasing = TRUE)][1]
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
  use_lat_zonal_weights = TRUE,
  uncertainty = TRUE, boot_seed = 666, boot... = list()
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

  rrr <- apply(rr, 1, stats::weighted.mean, w = weights, na.rm = TRUE)
  is.na(rrr) <- is.nan(rrr)

  gg <- g[, c("year", "month")] %>% dplyr::mutate(!!series_name := rrr)

  if (uncertainty) local({
    d <- sapply(t(p0), function(a) { if (is.data.frame(a[[1]])) return(a[[1]]); NULL }, simplify = FALSE) %>% purrr::compact() %>% purrr::reduce(dplyr::bind_cols) %>% data.matrix

    wz <- apply(p0, 1, function(a) sapply(a, function(b) { if (is.data.frame(b[[1]])) return (rep(1, NCOL(b[[1]]))/NCOL(b[[1]])); NULL }, simplify = FALSE)) %>% unlist %>% as.vector

    v2 <- apply(p0, 1, function(a) { sapply(a, function(b) { if (is.data.frame(b[[1]])) { apply(b[[1]], 1, function(bb) { r <- bb; r[!is.na(r)] <- 1; r/sum(r, na.rm = TRUE) }) } }, simplify = FALSE) %>% purrr::compact() }) %>% purrr::compact(); names(v2) <- colnames(rr)
    v2a <- rapply(v2, function(a) { if (!is.matrix(a)) return (as.matrix(a)); t(a) }, how = "replace")
    v2b <- vector("list", length = sapply(v2a, length) %>% sum)
    i <- 1; dev_null <- rapply(v2a, function(a) { v2b[[i]] <<- a; i <<- i + 1; NULL })
    # rapply(v2b, NCOL) %>% length # No. non-empty cells
    # rapply(v2b, NCOL) %>% sum # No. stations
    v2c <- rapply(v2a, function(a) { a[!is.na(a)] <- 1; a }, how = "replace")
    ## v2c: List w/ elms for all non-empty cells by lat; each leaf elm contains matrix of all time points × all stations for that cell, 1 for non-missing.

    ####################
    ##### Weights based on the no. of stations in a cell:
    ####################
    wz1 <- purrr::reduce(v2b, cbind) %>% data.matrix
    # apply(wz1, 2, min, na.rm = TRUE) == wz %>% as.vector

    ## Assume that latitude weights are same for all longitudes.
    wl0 <- apply(p0, 1, function(a) attr(a[[1]], "weight"))
    wl <- wl0[!sapply(r, is.null)]
    if (use_lat_zonal_weights)
      wl <- wl * lat_zonal_weights

    ## Aggregate stations by latitude.
    v3 <- vector("list", length = dim(p0)[1])
    # sapply(v3, length) %>% sum # No. non-empty cells
    # rapply(v3, NCOL) %>% sum # No. stations
    i <- 0; plyr::a_ply(p0, 1, function(a) { i <<- i + 1; r <- sapply(a, function(b) { if (!is_invalid(b[[1]])) return (b[[1]]); NULL }, simplify = FALSE) %>% purrr::compact(); if (!is_invalid(r)) v3[[i]] <<- r })
    v3 <- v3 %>% purrr::compact()
    v3a <- sapply(v3, function(a) { r <- Reduce(cbind, a) %>% data.matrix; r[!is.na(r)] <- 1; colnames(r) <- NULL; r }, simplify = FALSE)
    ## v3a: List w/ elms for all non-empty latitudes; each elm contains matrix of all time points × all stations for that lat, 1 for non-missing.
    # sapply(v3a, NCOL) %>% sum # Total no. of stations
    i <- 0; v3b <- sapply(v3a, function(a) { i <<- i + 1; r <- apply(a, 1, function(b) { (wl[i] * b)/sum(b, na.rm = TRUE) }); if (is.null(dim(r))) return (as.matrix(r)); t(r) }, simplify = FALSE)

    ####################
    ##### Weights based on differing no. of non-empty cells for each latitude:
    ####################
    wll1 <- v3c <- Reduce(cbind, v3b) %>% data.matrix; colnames(v3c) <- NULL
    # apply(wll1, 2, min, na.rm = TRUE) == wll %>% as.vector

    vv <- plyr::aaply(p0, 1, function(a) sapply(a, function(b) if (is.data.frame(b[[1]])) NCOL(b[[1]]) else NULL, simplify = FALSE), .drop = FALSE)
    lat_station_counts <- apply(vv, 1, function(a) { r <- a %>% purrr::compact(); if (length(r) > 0) return (r %>% unlist %>% sum); NULL }) %>% unlist
    lat_cell_counts <- apply(vv, 1, function(a) { r <- a %>% purrr::compact(); if (length(r) > 0) return (r %>% unlist %>% length); NULL }) %>% unlist
    lat_observations_weights <- sapply(seq_along(lat_cell_counts), function(i) { rep(lat_cell_counts[i]/sum(lat_cell_counts, na.rm = TRUE), lat_station_counts[i])/lat_station_counts[i] }, simplify = FALSE) %>% unlist

    ## Need to use 'v2c' & 'v3a' here.
    lat_station_counts1 <- plyr::llply(v3a, rowSums, na.rm = TRUE)
    # sapply(lat_station_counts1, max) # Cf. lat_station_counts %>% as.vector
    lat_cell_counts1 <- sapply(seq_along(v2c), function(i) { r <- sapply(v2c[[i]], function(b) { r <- rowSums(b, na.rm = TRUE); r[r > 1] <- 1; r }) %>% rowSums }, simplify = FALSE)
    # sapply(lat_cell_counts1, max) # Cf. lat_cell_counts %>% as.vector
    sum_lat_cell_counts1 <- Reduce(cbind, lat_cell_counts1) %>% data.matrix %>% rowSums
    v1 <- sapply(seq_along(lat_cell_counts1), function(i) { r <- lat_cell_counts1[[i]]/sum_lat_cell_counts1; is.na(r) <- is.nan(r); rr <- matrix(rep(r, lat_station_counts[i]), ncol = lat_station_counts[i], byrow = FALSE)/lat_station_counts1[[i]]; is.na(rr) <- is.nan(rr); rr }, simplify = FALSE)
    lat_observations_weights1 <- Reduce(cbind, v1) %>% data.matrix

    ####################
    ##### Weights based on differing no. of non-empty cells & stations for each latitude:
    ####################
    inv_lat_observations_weights1 <- 1/lat_observations_weights1

    w <- wz1 * inv_lat_observations_weights1 * wll1 # Use this below to speed things up.
    i <- 0; dd <- apply(d, 1, function(a) { i <<- i + 1; stats::weighted.mean(a, w = w[i, ], na.rm = TRUE) }); is.na(dd) <- is.nan(dd)

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

  gg
}

## usage:
# get_series_from_ghcn_gridded(ver = 3, temp = "avg", quality = "a", download = FALSE)
# ghcn_v3_avg <- make_ghcn_temperature_series(ghcn, station_metadata, "GHCN v3 Global Land Adj. (30° × 30° cells)")


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
