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


## Which values of 'v' are closest to the given values of 'x'?
nearest_orig <- function(v, x, value = FALSE)
{
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
is_zero <- function (x, tol = sqrt(.Machine$double.eps), na.rm = FALSE)
{
  if (na.rm)
    x <- x[!is.na(x)]
  if (is.numeric(x))
    abs(x) < tol
  else FALSE
}


## Which values of 'x' are closest to the given values of 'v'? I.e. the "fixed" values are 'x'.
## Swiped from 'DescTools::Closest()'.
#' @export
nearest <- function (x, v, value = FALSE, na.rm = FALSE)
{
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
nearest_below <- function(v, x, value = FALSE) { l <- which(v == max(v[(v < x)])); if (value) v[l] else l }

#' @export
nearest_above <- function(v, x, value = FALSE) { l <- which(v == min(v[(v > x)])); if (value) v[l] else l }


## Use convolution filter to calculate n-month moving average.
## N.B. "If sides = 1 the filter coefficients are for past values only; if sides = 2 they are centred around lag 0. In this
##   case the length of the filter should be odd, but if it is even, more of the filter is forward in time than backward."
#' @export
moving_average <- function(x, n, sides = 1L, ...) # 'n' is the window size
{
  if (is.null(n)) return (x)

  r <- stats::filter(x, rep(1/n, n), sides = sides, ...) %>%
    `colnames<-`(colnames(x))

  return (r)
}

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
    integrateArgs <- utils::modifyList(integrateArgs, integrate..., keep.null = TRUE)
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
  if (missing(series))
    series <- NULL

  colNames <- c(intersect(colnames(x), c(common_columns, series)))
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
# e <- get_climate_data(download=FALSE, baseline=FALSE) # Or full update: e <- get_climate_data(download = TRUE, omit = NULL)
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


## These fuzzy equalities are necessary or useful sometimes.
#' @export
all_equal <- function(...) Vectorize(all.equal, "target", SIMPLIFY = FALSE)(...)


#' @export
is_equal <- function(..., simplify = TRUE) sapply(all_equal(...), function(x) is.logical(x) && x, simplify = simplify)


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


#' @export
make_current_timestamp <- function(fmt = "%Y-%m-%d", use_seconds = FALSE, seconds_sep = '+')
{
  sysTime <- Sys.time()
  timestamp <- format(sysTime, fmt)
  if (use_seconds)
    timestamp <- paste(timestamp, sprintf("%05d", lubridate::period_to_seconds(lubridate::hms(format(Sys.time(), "%H:%M:%S")))), sep = seconds_sep)

  return (timestamp)
}


#' @export
add_months <- function(x, m)
{
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


#' @export
eval_js <- function(..., envir = parent.frame(), enclos = if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv())
{
  dots <- get_dots(..., evaluate = TRUE)
  expr <- unlist(dots$evaluated)

  if (is.list(expr)) {
    if (is.function(expr[[1L]])) # If first '...' argument is a function, execute it with other '...' arguments as its own.
      return (do.call(expr[[1L]], tail(expr, -1L)))

    for (i in expr) {
      if (is.expression(i) || is.language(i)) {
        return (eval(i, envir, enclos)) # Returns only the first expression found.
      }
    }
  }

  expr <- paste(expr, collapse = " ")

  if (typeof(expr) != "character")
    return (expr)

  expr <- parse(text = expr)
  eval(expr, envir, enclos)
}


## Cf. https://stackoverflow.com/questions/7414657/find-the-corresponding-row-and-column-number-to-an-indexed-element-in-a-matrix/7414764#7414764
#' @export
get_index_from_element <- function(i, m)
{
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
)
{
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


## Adapted from 'fANCOVA::loess.as()'; automatically choose 'span' parameter for 'stats::loess()'
optimize_span <- function(
  model,
  criterion = c("aicc", "gcv"),
  span_range = c(0.05, 0.95),
  seed = 666
)
{
  as.crit <- function(x)
  {
    span <- x$pars$span
    traceL <- x$trace.hat
    sigma2 <- sum(x$residuals^2)/(x$n - 1)
    aicc <- log(sigma2) + 1 + 2 * (2 * (traceL + 1))/(x$n - traceL - 2)
    gcv <- x$n * sigma2/(x$n - traceL)^2
    result <- list(span = span, aicc = aicc, gcv = gcv)

    return(result)
  }

  criterion <- match.arg(criterion)
  fn <- function(span)
  {
    mod <- update(model, span = span)

    as.crit(mod)[[criterion]]
  }

  if (!is.null(seed))
    set.seed(seed)
  result <- stats::optimize(fn, span_range)

  #return (list(span = result$minimum, criterion = result$objective))
  return (result$minimum)
}


## Drop-in replacement for 'stats::loess()' that chooses optimal span/bandwidth parameter when 'span = NULL';
##   also produces a quick-view viz for 'plot = TRUE'.
#' @export
LOESS <- function(
  formula,
  data,
  span = formals(stats::loess)$span,
  plot = FALSE,
  optimize_span... = list(),
  ...
)
{
  ## See equivalent code in 'stats::loess()' to create data frame from 'formula':
  # mf <- match.call(expand.dots = FALSE)
  # mf$span <- mf$plot <- mf$optimize_span... <- mf$... <- NULL
  # mf[[1L]] <- quote(stats::model.frame)
  # mf <- eval(mf, parent.frame())

  opt.span <- optimize_span %>% `environment<-`(environment()) # Otherwise 'stats::optimize()' fails
  form <- formula
  # form <- formula %>% `environment<-`(environment()) # Allows use of e.g. 'stats::model.frame()'

  if (missing(data))
    data <- NULL

  if (is.null(span)) {
    optimize_spanArgs <- list(
      model = stats::loess(formula = form, data = data, ...)
    )
    optimize_spanArgs <- utils::modifyList(optimize_spanArgs, optimize_span..., keep.null = TRUE)

    span <- do.call(opt.span, optimize_spanArgs)
  }

  ## 'mf' replaces calculations above:
  if (plot)
    mf <- stats::loess(formula = form, data = data, span = span, method = "model.frame", ...)
  mod <- stats::loess(formula = form, data = data, span = span, ...)

  if (plot) { ## Adapted from 'fANCOVA::loess.as()'
    x <- mod$x
    modPlot <- update(mod, control = loess.control(surface = "direct"))

    if (NCOL(x) == 1) {
      m <- 100
      xNew <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = m)
      formVars <- all.vars(form)
      fitNew <- stats::predict(modPlot, dataframe(xNew) %>% `names<-`(tail(formVars, -1)))

      plot(form, mf, col = "grey", xlab = formVars[2], ylab = formVars[1], ...)
      lines(xNew, fitNew, lwd = 1.5, ...)
      mtext(sprintf("span: %1.2f", span), side = 3)
    } else {
      m <- 50
      x1 <- seq(min(x[, 1]), max(x[, 1]), len = m)
      x2 <- seq(min(x[, 2]), max(x[, 2]), len = m)
      formVars <- all.vars(form)
      xNew <- expand.grid(x1 = x1, x2 = x2) %>%
        `names<-`(tail(formVars, -1)) %>% `length<-`(2)
      fitNew <- matrix(stats::predict(modPlot, xNew), m, m)

      graphics::persp(x1, x2, fitNew, theta = 40, phi = 30, ticktype = "detailed",
        xlab = formVars[2], ylab = formVars[3], zlab = formVars[1], col = "lightblue", expand = 0.6)
      mtext(sprintf("span: %1.2f", span), side = 3)
    }
  }

  return (mod)
}

## usage:
# (cars.lo <- LOESS(dist ~ speed, cars, span = NULL, plot = TRUE))
# n2 <- 100
# x21 <- runif(n2, min = 0, max = 3)
# x22 <- runif(n2, min = 0, max = 3)
# sd2 <- 0.25
# e2 <- rnorm(n2, sd = sd2)
# y2 <- sin(2 * x21) + sin(2 * x22) + 1 + e2
# # (y2.fit <- fANCOVA::loess.as(cbind(x21, x22), y2, plot = TRUE))
# dat <- cbind(x21, x22, y2) %>% as.data.frame
# (y2.fit <- LOESS(y2 ~ x21 + x22, dat, span = NULL, plot = TRUE))


#' @export
poly_eval <- function(expr, envir = parent.frame(), env = rlang::caller_env(), ...)
{
  if (is.null(expr))
    return (NULL)

  if (is.function(expr)) {
    #expr(...) # Change to 'do.call()' to include 'envir'
    do.call(what = expr, args = get_dots(...)$arguments, envir = envir)
  } else if (rlang::is_expression(expr)) {
    rlang::eval_tidy(expr, env = env, ...)
  } else if (is.expression(expr)) {
    eval(expr, envir = envir, ...)
  } else {
    expr
  }
}
