## Which values of 'v' are closest to the given values of 'x'?
nearest_orig <- function(v, x, value = FALSE){
  d <- data.table::data.table(v, value = v)
  data.table::setattr(d, "sorted", "v")
  data.table::setkey(d, v) # Sort the data

  ## Binary search
  ## N.B. Can't really get at 'J()' without making this package depend on "data.table" --
  ## V. https://stackoverflow.com/questions/22001945/how-is-j-function-implemented-in-data-table
  m <- d[J(x), roll = "nearest"]$value

  l <- which(v == m)

  if (value)
    v[l]
  else
    l
}

## usage:
# nearest(1:10, c(0, 13))


## Same as 'DescTools::IsZero()'.
#' @export
is_zero <- function (x, tol = sqrt(.Machine$double.eps), na.rm = FALSE){
  if (na.rm)
    x <- x[!is.na(x)]
  if (is.numeric(x))
    abs(x) < tol
  else FALSE
}


## Which values of 'x' are closest to the given values of 'v'? I.e. the "fixed" values are 'x'.
## Swiped from 'DescTools::Closest()'.
#' @export
nearest <- function (x, v, value = FALSE, na.rm = FALSE){
  v <- v[1]

  if (na.rm)
    x <- x[!is.na(x)]

  mdist <- min(abs(x - v))
  if (is.na(mdist))
    res <- NA
  else {
    idx <- is_zero(abs(x - v) - mdist)
    if (!value)
      res <- which(idx)
    else res <- x[idx]
  }

  return (res)
}

## usage:
# nearest(-(1:10), 13)


#' @export
merge_fun_factory <- function(FUN=base::merge, SETDIFF=TRUE, ...){
  if (SETDIFF)
    ## N.B. Note how '...' is NOT in 'function(x, y)'.
    function(x, y) FUN(x, y[, c(eval(get_dots(..., evaluate=TRUE)$evaluated$by), setdiff(colnames(y), colnames(x)))], ...)
  else
    function(x, y) FUN(x, y, ...)
}


#' @export
only_selected_series <- function(x, series, range = NULL, x_var = NULL, ...) {
  if (!is.null(range) && is.null(x_var)) {
    cols <- intersect(colnames(x), common_columns)
    x_var <- if ("yr_part" %in% cols) "yr_part" else "year"
  }
  keystone::only_selected_series(x, series, common_columns = common_columns,
    range = range, x_var = x_var, ...)
}

#' @export
oss <- only_selected_series


#' @export
view_only_selected_series <- function(x, series, range = NULL, x_var = NULL, ...) {
  if (!is.null(range) && is.null(x_var)) {
    cols <- intersect(colnames(x), common_columns)
    x_var <- if ("yr_part" %in% cols) "yr_part" else "year"
  }
  keystone::view_only_selected_series(x, series, common_columns = common_columns,
    range = range, x_var = x_var, ...)
}

#' @export
vss <- view_only_selected_series

## usage:
# e <- get_climate_data(download=FALSE, baseline=FALSE) # Or full update: e <- get_climate_data(download = TRUE, omit = NULL)
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global",
#   "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5", "UAH TLT 6.0 Global", "ERA-Interim 2m Global",
#   "RATPAC-A 850-300 mb Global")
# vss(e, series)
# g <- make_yearly_data(e)
# vss(g, series, with=FALSE)


#' @export
add_months <- function(x, m){
  if (length(x) == 1)
    y <- nearest_year_month_from_numeric(x = x)
  else
    y <- x

  y <- as.vector(y)

  r <- c(year = y[1] + (y[2] + m - 1) %/% 12, month = ((y[2] + (m %% 12)) %% 12) %>% (function(z) ifelse(z, z, 12)))

  r
}

## usage:
# add_months(1880.458, -7)
# add_months(c(1880, 6), 8)


## Cf. https://stackoverflow.com/questions/7414657/find-the-corresponding-row-and-column-number-to-an-indexed-element-in-a-matrix/7414764#7414764
#' @export
get_index_from_element <- function(i, m){
  x <- array(seq_along(m), dim = dim(m))
  which(x == i, arr.ind = TRUE)
}

## usage:
# m <- array(1:24, dim = 2:4)
# get_index_from_element(1:24, m)


## https://stevemosher.wordpress.com/2010/09/13/handling-z-files/
#' @export
unzip_Z <- function(
  zfile,
  destfile,
  readBin_n = 99999999,
  remove = FALSE
){
  ## This function is called for the side effect of uncompressing a .Z file
  ## 'zfile' is a path to the Zfile
  ## 'destfile' is the uncompressed file to be written
  ##   N.B. No protection against overwriting!
  ## If 'remove' is TRUE, delete the Z file afterwards

  if (!file.exists(zfile))
    stop(cat(zfile, " does not exist"))

  handle <- file(zfile, "rb")
  data <- readBin(handle, "raw", readBin_n)
  close(handle)

  ## https://cran.r-project.org/src/contrib/Archive/uncompress/
  ## Make v1.34 into v1.35; in "zzz.R", change '.First.lib' to '.onLoad' & install e.g.:
  ## devtools::install("[...]/Downloads/climate/GHCN/v2/uncompress", upgrade = "never")

  uncomp_data <- uncompress::uncompress(data)

  desthandle <- file(destfile, "wb")
  writeBin(uncomp_data, desthandle)
  close(desthandle)

  if (remove == TRUE)
    unlink(zfile)
}


#' @export
fahr_to_kelvin <- function(temp){
  ((temp - 32) * (5/9)) + 273.15
}

## Convert Kelvin temperatures to Celsius.
#' @export
kelvin_to_celsius <- function(temp){
  temp - 273.15
}

## Convert Fahrenheit temperatures to Celsius.
#' @export
fahr_to_celsius <- function(temp){
  kelvin_to_celsius(fahr_to_kelvin(temp))
}

## Convert Celsius temperatures to Fahrenheit.
#' @export
celsius_to_fahr <- function(temp){
  temp * (9/5) + 32
}


