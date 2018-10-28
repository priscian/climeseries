# http://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
#' @export
seqle <- function(x, incr=1)
{
  if (!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + incr
  #y <- abs(x[-1L] - x[-n] - incr) > .Machine$double.eps ^ 0.5 # Possible enhancement for numerics. See Web link above.
  i <- c(which(y | is.na(y)), n)

  list(lengths=diff(c(0L, i)), values=x[head(c(0L, i) +1L, -1L)])
}


## Find leading and trailing NAs in a vector; returns 'FALSE' for leading/trailing NAs, 'TRUE' for NA-enwrapped values.
#' @export
na_unwrap <- function(x, ...)
  UseMethod("na_unwrap")


#' @export
na_unwrap.matrix <- function(x, ...)
{
  apply(apply(x, 2, na_unwrap.default, ...), 1, any)
}


#' @export
na_unwrap.data.frame <- function(x, ...)
{
  na_unwrap.matrix(x, ...)
}


#' @export
na_unwrap.default <- function(x, type=c("both", "head", "tail", "none"), ...)
{
  type <- match.arg(type)

  nai <- stats:::na.omit.default(x) # Changed 14 Jan. 2017 to work with "ts" objects.
  #s <- rle(attr(nai, "na.action")) # See external function definition.
  s <- seqle(attr(nai, "na.action")) # See external function definition.

  leadi <- head(s$values, 1L)
  leadr <- NULL
  if (!is.na(leadi)) {
    if (leadi == 1L)
      leadr <- leadi:(leadi + head(s$lengths, 1L) - 1L)
  }

  traili <- tail(s$values, 1L)
  trailr <- NULL
  if (!is.na(traili)) {
    if (traili + tail(s$lengths, 1L) - 1L == length(x))
      trailr <- traili:(length(x))
  }

  r <- rep(TRUE, length(x))

  switch(type,
    both = r[c(leadr, trailr)] <- FALSE,
    head = r[c(leadr)] <- FALSE,
    tail = r[c(trailr)] <- FALSE
  )

  return (r)
}

## usage:
# na_unwrap(inst$Keeling)
# na_unwrap(inst$GISTEMP[inst$year %in% 1900:2000]) # No leading/trailing NAs.


#' @export
shift <- function(x, ...)
  UseMethod("shift")


#' @export
shift.default <- function (x, i=1L, roll=TRUE, na_rm=FALSE)
{
  if (i == 0L) return (x)

  naRm <- function(x, na_rm)
  {
    if (!na_rm) return (x)

    x[setdiff(seq_along(x), attr(na.omit(x), "na.action"))]
  }

  n  <- length(x)
  if (n == 0L) return (x)

  j <- i %% n

  if (roll && j == 0L) return (naRm(x, na_rm))

  if (!roll && j == 0L) {
    x[seq_along(x)] <- NA

    return (naRm(x, na_rm))
  }

  if (!roll && i > n) {
    rv <- x
    rv[seq_along(rv)] <- NaN
  }
  else {
    shifted <- 1L:(n - j)
    if (i > 0L)
      shifted <- (n - j + 1L):n

    if (!roll) x[shifted] <- NA
    if (na_rm) x[shifted] <- NaN

    rv <- x[c((n - j + 1L):n, shifted)]
    if (i > 0L)
      rv <- x[c(shifted, 1L:(n - j))]
  }

  if (na_rm)
    rv <- rv[!is.nan(rv)]

  return (rv)
}

## usage:
# shift(1:10)
# shift(1:10, roll=FALSE)
# shift(1:10, -1)
# shift(1:10, -1, roll=FALSE)
# shift(1:10, 5)
# shift(1:10, 5, roll=FALSE)
# shift(1:10, -5)
# shift(1:10, -5, roll=FALSE)
# shift(1:10, 5, roll=FALSE, na_rm=TRUE)
# shift(1:10, -5, roll=FALSE, na_rm=TRUE)


#' @export
shift.data.frame <- function(x, i, ...)
{
  if (!is.list(i)) {
    i <- as.list(rep(i, length.out=length(x)))
    names(i) <- names(x)
  }

  for(j in names(i))
    x[[j]] <- shift.default(x[[j]], i[[j]], ...)

  x
}


#' @export
nearest <- function(v, x, value = FALSE)
{
  d <- data.table(v, value = v)
  setattr(d, "sorted", "v")
  setkey(d, v) # Sort the data

  ## Binary search
  m <- d[J(x), roll = "nearest"]$value

  l <- which(v == m)

  if (value)
    v[l]
  else
    l
}

#' @export
nearest_below <- function(v, x, value=FALSE) { l <- which(v == max(v[(v < x)])); if (value) v[l] else l }

#' @export
nearest_above <- function(v, x, value=FALSE) { l <- which(v == min(v[(v > x)])); if (value) v[l] else l }


## Use convolution filter to calculate n-month moving average.
#' @export
moving_average <- function(x, n, sides=1L, ...) { if (is.null(n)) return (x); r <- stats::filter(x, rep(1/n, n), sides=sides, ...); colnames(r) <- colnames(x); return (r) } # 'n' is the window size.

#' @export
MA <- moving_average


#' @export
interpNA <- function (x, method=c("linear", "before", "after", "none"), unwrap=TRUE, skip_all_is_na=TRUE, ...)
{
  if (!inherits(x, "matrix") && !inherits(x, "timeSeries"))
    x <- as(x, "matrix")

  if (method[1] == "none")
    return (x)

  fun <- stats::approx
  if (method[1] %nin% c("linear", "before", "after", "none")) # '?stats::spline' for available "method"s.
    ## The following code removes any unmatched arguments from a call to 'FUN()';
    ## e.g. 'stats::spline()' doesn't have a formal argument 'f', which is nonetheless passed in below.
    fun <- function(...) { FUN <- stats::spline; d <- get_dots(...); a <- d$arguments[trimws(names(d$arguments)) %in% c("", formalArgs(FUN))]; do.call(FUN, a, quote=FALSE, envir=parent.frame()) }
  #else unwrap = FALSE

  interpVectorNA <- function(x, method, f, ...)
  {
    n <- length(x)
    idx <- (1:n)[!is.na(x)]
    y <- fun(x=idx, y=x[idx], xout=1:n, method=method, f=f)$y

    ## If spline interpolation, allow terminal NAs to be interpolated.
    if (!unwrap) return (y)

    ## If any leading/trailing NAs remain, interpolate them from the first/last value.
    y[!na_unwrap(y, "head")] <- y[head(which(!is.na(y)), 1)]
    y[!na_unwrap(y, "tail")] <- y[tail(which(!is.na(y)), 1)]

    r <- x
    r[na_unwrap(x, ...)] <- y[na_unwrap(x, ...)]

    r
  }

  method <- method[1]
  f <- 0
  if (method == "before") {
    method <- "constant"
    f <- 0
  }
  if (method == "after") {
    method <- "constant"
    f <- 1
  }
  for (i in 1:ncol(x)) {
    if (skip_all_is_na) {
      if (all(is.na(x[, i])))
        next
    }
    x[, i] <- interpVectorNA(x[, i], method, f, ...)
  }

  x
}


## http://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
#' @export
split_at <- function(x, pos, split_after=FALSE)
{
  unname(split(x, cumsum(seq_along(x) %in% (pos + as.integer(split_after)))))
}


#' @export
get_dots <- function(..., evaluate=FALSE)
{
  caller <- sys.function(which=-1L)
  formalArguments <- NULL
  if (!is.null(caller)) {
    callerName <- as.list(sys.call(-1L))[[1L]]
    formalArguments <- names(formals(caller))
  }
  #unevaluated <- substitute(...()) # List of '...' name-value pairs.
  unevaluated <- eval(substitute(alist(...)))
  dotsAsCharacter <- unlist(sapply(unevaluated, deparse, simplify=TRUE))
  dotsNames <- names(dotsAsCharacter)
  if (is.null(dotsNames))
    dotsNames <- rep("", length(dotsAsCharacter))

  rv <- list()
  if (!is.null(sys.call(-2L)))
    rv$calling_function <- as.list(sys.call(-2L))[[1L]]
  rv$current_function <- callerName
  rv$current_formals <- formalArguments
  #rv$... <- environment()$`...`
  rv$arguments <- as.list(unevaluated)
  if (evaluate)
    rv$evaluated <- list(...)
  rv$as_character <- dotsAsCharacter
  rv$named_dots <- dotsNames
  whichDots <- which(formalArguments == "...")
  if (length(whichDots) == 0L)
    whichDots <- ifelse(length(formalArguments) == 0L, 1L, length(formalArguments))
  temp <- append(formalArguments, dotsNames[dotsNames != ""], after=whichDots)
  rv$all_named_args <- temp[temp != "..."]

  return (rv)
}


## https://stackoverflow.com/a/47955845/931941
#' @export
get_all_args <- function(defaults = FALSE) {
  ## Get formals of parent function.
  parentFormals <- formals(sys.function(sys.parent(n=1)))

  ## Get names of assumed arguments.
  hasDots <- FALSE
  fnames <- names(parentFormals)
  if (any(fnames == "...")) {
    hasDots <- TRUE
    ## Remove '...' from list of parameter names.
    fnames <- fnames[-which(fnames == "...")]
  }

  ## Get current values for named variables in the parent frame.
  a <- evalq(as.list(environment()), envir=parent.frame())
  a <- a[fnames]

  ## Get the list of variables in '...'.
  if (hasDots)
    # a <- c(a, evalq(list(...), envir=parent.frame()))
    a <- c(a, evalq(get_dots(...)$arguments, envir=parent.frame()))

  if (defaults) {
    ## Get default values.
    defArgs <- as.list(parentFormals)
    defArgs <- defArgs[unlist(lapply(defArgs, FUN=function(x) class(x) != "name"))]
    a[names(defArgs)] <- defArgs
    setArgs <- evalq(as.list(match.call())[-1], envir=parent.frame())
    a[names(setArgs)] <- setArgs
  }

  a
}


#' @export
merge_fun_factory <- function(FUN=base::merge, SETDIFF=TRUE, ...)
{
  if (SETDIFF)
    ## N.B. Note how '...' is NOT in 'function(x, y)'.
    function(x, y) FUN(x, y[, c(eval(get_dots(..., evaluate=TRUE)$evaluated$by), setdiff(colnames(y), colnames(x)))], ...)
  else
    function(x, y) FUN(x, y, ...)
}


#' @export
nop <- function(x=NULL)
{
  return (invisible(x))
}


#' @export
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1L, 1L)), { s <- substring(s, 2L); if (strict) tolower(s) else s }, sep='', collapse=' ')
  sapply(strsplit(s, split=' '), cap, USE.NAMES=!is.null(names(s)))
}


#' @export
`%nin%` <- function(x, table) {
  match(x, table, nomatch=0) == 0
}


## V. '?base::grep' from the R command line.
#' @export
parse_one <- function(res, result)
{
  m <- do.call(rbind, lapply(seq_along(res),
    function(i) {
      if (result[i] == -1) return("")
      st <- attr(result, "capture.start")[i, ]
      substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
    }))
  colnames(m) <- attr(result, "capture.names")

  m
}


## Code "borrowed" from 'MESS::auc()' (https://cran.r-project.org/web/packages/MESS/).
#' @export
integratex <- function (x, y, from=min(x), to=max(x), type=c("spline", "linear"), absolutearea=FALSE, integrate...=list(), ...)
{
  type <- match.arg(type)
  if (length(x) != length(y))
    stop("x and y must have the same length")
  if (length(unique(x)) < 2L)
    return(NA)
  if (type == "linear") {
    if (absolutearea)
      y <- y - min(y)
    values <- approx(x, y, xout=sort(unique(c(from, to, x[x > from & x < to]))), ...)
    res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
    if (absolutearea)
      res <- res - min(y) * (max(x) - min(x))
  }
  else {
    if (absolutearea)
      myfunction <- function(x)
      {
        abs(splinefun(x, y, method="natural"))
      }
    else myfunction <- splinefun(x, y, method="natural")

    integrateArgs <- list(
      f = myfunction,
      lower = from,
      upper = to
    )
    integrateArgs <- modifyList(integrateArgs, integrate...)
    res <- do.call("integrate", integrateArgs)
    #res <- integrate(myfunction, lower=from, upper=to)$value
  }

  res
}


#' @export
backtick <- function(x, ...)
{
  sapply(x, function(a) paste("`", as.character(a), "`", sep=""), ...)
}


#' @export
dataframe <- function (..., row.names=NULL, check.rows=FALSE, check.names=FALSE, fix.empty.names=FALSE, stringsAsFactors=FALSE)
{
  data.frame(..., row.names=row.names, check.rows=check.rows, check.names=check.names, fix.empty.names=fix.empty.names, stringsAsFactors=stringsAsFactors)
}


#' @export
char_sort <- function(x, s)
{
  x[which(x %in% s)] <- s[which(s %in% x)]

  x
}


#' @export
only_selected_series <- function(x, series, sort = FALSE, range = NULL, ...)
{
  colNames <- c(intersect(names(x), c(common_columns, series)))
  if (!sort)
    colNames <- char_sort(colNames, series)
  r <- x[, colNames, ...]

  if (!is.null(range)) {
    if ("yr_part" %in% colNames)
      yearVar <- "yr_part"
    else if ("year" %in% colNames)
      yearVar <- "year"

    r <- r[r[[yearVar]] >= ifelse(is.na(range[1]), min(r[[yearVar]], na.rm = TRUE), range[1]) & r[[yearVar]] <= ifelse(is.na(range[2]), max(r[[yearVar]]), range[2]), ]
  }

  r
}

#' @export
oss <- only_selected_series


#' @export
view_only_selected_series <- function(..., fun = View)
{
  fun(only_selected_series(...))
}

#' @export
vss <- view_only_selected_series

## usage:
# e <- get_climate_data(download=FALSE, baseline=FALSE) # Or full update: e <- get_climate_data(download=TRUE, omit=NULL)
# series <- c("GISTEMP Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global",
#   "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5", "UAH TLT 6.0 Global", "ERA-Interim 2m Global",
#   "RATPAC-A 850-300 mb Global")
# vss(e, series)
# g <- make_yearly_data(e)
# vss(g, series, with=FALSE)


## Compare internal representations of R objects:
## http://stackoverflow.com/questions/9278715/value-reference-equality-for-same-named-function-in-package-namespace-environmen
#' @export
are_same <- function(x, y)
{
  f <- function(x) capture.output(.Internal(inspect(x)))
  all(f(x) == f(y))
}


#' @export
is_invalid <- function(x, ...)
{
  if (missing(x) || is.null(x) || length(x) == 0L)
    return (TRUE)

  if (is.list(x))
    return (all(sapply(x, is_invalid)))
  else if (is.vector(x))
    return (all(is.na(x)))
  else
    return (FALSE)
}
