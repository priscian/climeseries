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
  apply(apply(x, 2, na_unwrap.default), 1, any)
}


#' @export
na_unwrap.data.frame <- function(x, ...)
{
  na_unwrap.matrix(x, ...)
}


#' @export
na_unwrap.default <- function(x, ...)
{
  nai <- na.omit(x)
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
  r[c(leadr, trailr)] <- FALSE

  return (r)
}

## usage:
# na_unwrap(inst$Keeling)
# na_unwrap(inst$GISTEMP[inst$year %in% 1900:2000]) # No leading/trailing NAs.


#' @export
shift <- function (x, i=1L, roll=TRUE, na_rm=FALSE)
{
  n  <- length(x)
  if (n == 0L) return(x)

  j <- i %% n

  if (j == 0L) return(x)

  shifted <- 1L:(n - j)
  if (i > 0L)
    shifted <- (n - j + 1L):n

  if (!roll) x[shifted] <- NA
  if (na_rm) x[shifted] <- NaN

  rv <- x[c((n - j + 1L):n, shifted)]
  if (i > 0L)
    rv <- x[c(shifted, 1L:(n - j))]

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
nearest_below <- function(v, x, value=FALSE) { l <- which(v == max(v[(v < x)])); if (value) v[l] else l }

#' @export
nearest_above <- function(v, x, value=FALSE) { l <- which(v == min(v[(v > x)])); if (value) v[l] else l }


## Use convolution filter to calculate n-month moving average.
#' @export
moving_average <- function(x, n, sides=1L, ...) { if (is.null(n)) return (x); r <- stats::filter(x, rep(1/n, n), sides=sides, ...); colnames(r) <- colnames(x); return (r) } # 'n' is the window size.

#' @export
MA <- moving_average


#' @export
interpNA <- function (x, method=c("linear", "before", "after"), ...)
{
  if (!inherits(x, "matrix") && !inherits(x, "timeSeries"))
    x <- as(x, "matrix")

  interpVectorNA <- function(x, method, f) {
    n <- length(x)
    idx <- (1:n)[!is.na(x)]
    x <- approx(idx, x[idx], 1:n, method=method, f=f)$y

    x
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
    x[, i] <- interpVectorNA(x[, i], method, f)
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


#' @export
merge_fun_factory <- function(...)
{
  function(x, y) base::merge(x, y[, c(eval(get_dots(...)$arguments$by), setdiff(colnames(y), colnames(x)))], ...)
}


#' @export
nop <- function(x=NULL)
{
  return (invisible(x))
}
