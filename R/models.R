#' Get CMIP Phase 3 or 5 Data from Stored Data
#'
#' Retrieves temperature data of CMIP3 or CMIP5 models from files of individual model runs, with or without saving a combined data set, or from a combined R data set.
#'
#' @param ensemble One of "cmip3" or "cmip5" coresponding to the appropriate IPCC model ensemble.
#' @param baseline An integer year or, more typically, range of years on which the temperature anomalies will be centered. If \code{NULL}, no baseline centering is done, and the string "raw_" will be appended to the \code{ensemble} string before the model data is saved or loaded.
#' @param save If \code{NULL}, retrieve model data without saving it; if \code{TRUE}, retrieve model data and save it as a combined R data set; if \code{FALSE}, load model data from a combined R data set. The default is \code{FALSE}.
#' @param subdir Corresponds to a subdirectory containing a specific set of model runs, with the CMIP3+ default "all members" almost always being preferred; likewise the CMIP5 default "all models".
#' @param cmip3_raw Logical. The CMIP3+ "all models" data contains SRES A2 and SRES B1 scenario runs that typically begin in Jan. 2001, so they need to be centered differently from SRES B1; also, the 20C3M scenario runs typically end in Dec. 2000, which needs to be accounted for. If the \code{ensemble = "cmip3"} and \code{cmip3_raw = FALSE}, then these issues are handled automatically in the returned (NOT saved) data set.
#' @param center_fun The function used to calculate the central tendency of the model runs at each time point; the default of \code{mean} is usually sufficient. It is used here specifically to line up some CMIP3+ scenario runs on a common baseline if \code{ensemble = "cmip3"} and \code{cmip3_raw = FALSE}, and should match the argument given to the parameter of the same name in a call to \code{\link{plot_models_and_climate_data}}, if plotting is done.
#'
#' @return A data frame of CMIP model runs. (Obivously I should give more detail here.)
#'
#' @examples
#' \dontrun{
#' ## The most common uses:
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=FALSE)
#' cmip3 <- get_models_data(baseline=1981:2010, save=FALSE)
#'
#' ## Other common uses:
#' cmip3 <- get_models_data(baseline=1981:2010, save=NULL) # Retrieve CMIP3 data without saving combined data set.
#' cmip3 <- get_models_data(baseline=1981:2010, save=TRUE) # Retrieve CMIP3 data and save as combined data set.
#' cmip3 <- get_models_data(baseline=1981:2010, save=FALSE) # Load CMIP3 data from combined R data set.
#' cmip3 <- get_models_data(baseline=NULL, save=TRUE) # Retrieve "raw" CMIP3 data and save as combined data set.
#' cmip3 <- get_models_data(baseline=NULL, save=FALSE) # Load "raw" CMIP3 data from combined R data set.
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=TRUE, subdir="all members") # Save CMIP5 data from "all members" data set.
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=TRUE, subdir="1 member per model") # Save CMIP5 data from "1 member per model" data set.
#'
#' ## Copy this to the R command line to redo all the data:
#' cmip3 <- get_models_data(baseline=1981:2010, save=TRUE)
#' cmip3 <- get_models_data(baseline=NULL, save=TRUE)
#' cmip3 <- get_models_data(baseline=1981:2010, save=TRUE, subdir="all models")
#' cmip3 <- get_models_data(baseline=NULL, save=TRUE, subdir="all models")
#' cmip3 <- get_models_data(baseline=1981:2010, save=TRUE, subdir="multi-model mean")
#' cmip3 <- get_models_data(baseline=NULL, save=TRUE, subdir="multi-model mean")
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=TRUE)
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=NULL, save=TRUE)
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=TRUE, subdir="1 member per model")
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=NULL, save=TRUE, subdir="1 member per model")
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=1981:2010, save=TRUE, subdir="all members")
#' cmip5 <- get_models_data(ensemble="cmip5", baseline=NULL, save=TRUE, subdir="all members")
#'
#' ## Create "meta" files to match up model runs with emissions scenarios. In each model-set directory, double-click on file "cmip(3|5).RData" and run the following code:
#' x <- "cmip3" # Or:
#' x <- "cmip5"
#' write.csv(eval(substitute(data.frame(model=names(attr(cmip, "scenario")), scenario=attr(cmip, "scenario")),
#'   list(cmip=as.symbol(x)))), file=paste(x, "_meta.csv", sep=""), row.names=FALSE)
#' }
#'
#' @export
get_models_data <- function(ensemble=c("cmip3", "cmip5"), baseline=NULL, save=FALSE, data_dir, subdir=NULL, cmip3_raw=FALSE, center_fun="mean")
{
  if (missing(data_dir)) {
    if (!is.null(getOption("climeseries_models_dir")))
      data_dir <- getOption("climeseries_models_dir")
    else if (is.null(save) || save == FALSE)
      data_dir <- system.file("extdata", package="climeseries")
    else
      data_dir <- dataDir
  }

  ensemble <- match.arg(ensemble)

  if (is.null(subdir)) { # Set up reasonable defaults for each ensemble.
    subdir <- switch(ensemble,
      cmip3 = "all members",
      cmip5 = "all models"
    )
  }

  path <- paste(data_dir, ensemble, subdir, sep="/")

  AlignCmip3Data <- function(x)
  {
    scenario <- attr(x, "scenario")

    ## Clear any 20C3M scenario data after 2000; i.e. make it exclusively historical.
    x[x$year > 2000, names(scenario[scenario %in% "20C3M"])] <- NA

    ## Clear any SRES A1B and SRES A2 scenario data before 2001; i.e. make it exclusively predictive.
    x[x$year < 2001, names(scenario[scenario %in% c("SRES A2", "SRES B1")])] <- NA

    middleDec2000 <- do.call(center_fun, list(x=unlist(x[x$year == 2000 & x$month == 12, names(scenario[scenario %in% "SRES A1B"])]), na.rm=TRUE)) # SRES A1B central tendency for Dec. 2000.

    middleJan2001.a2 <- do.call(center_fun, list(x=unlist(x[x$year == 2001 & x$month == 1, names(scenario[scenario %in% "SRES A2"])]), na.rm=TRUE)) # SRES A2 central tendency for Jan. 2001.
    middleJan2001.b1 <- do.call(center_fun, list(x=unlist(x[x$year == 2001 & x$month == 1, names(scenario[scenario %in% "SRES B1"])]), na.rm=TRUE)) # SRES B1 central tendency for Jan. 2001.

    ## Now calculate the difference between these "threshold" means and recenter the SRES runs to match the means.
    x[, names(scenario[scenario %in% "SRES A2"])] <- x[, names(scenario[scenario %in% "SRES A2"])] - (middleJan2001.a2 - middleDec2000)
    x[, names(scenario[scenario %in% "SRES B1"])] <- x[, names(scenario[scenario %in% "SRES B1"])] - (middleJan2001.b1 - middleDec2000)

    return (x)
  }

  if (!is.null(save) && !save) { # I.e. load the CMIP data set.
    loadEnv <- new.env()
    load(paste(path, ensemble %_% ifelse(is.null(baseline), "_raw", "") %_% ".RData", sep="/"), envir=loadEnv)

    if (!is.null(baseline))
      loadEnv[[ensemble]] <- recenter_anomalies(loadEnv[[ensemble]], baseline) # This step takes several seconds.

    d <- loadEnv[[ensemble]]

    if (ensemble == "cmip3" && !cmip3_raw)
      d <- AlignCmip3Data(d)

    return (d)
  }

  files <- list.files(path, "^.*?\\.dat$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE)

  KtoC <- function(k) k - 273.15

  e <- new.env()

  for (i in seq_along(files)) {
    tab <- read.table(files[i])
    is.na(tab) <- tab < 0

    flit <- melt(tab[, 1L:13L], id.vars="V1", variable.name="month", value.name="temp")
    for (j in names(flit)) flit[[j]] <- as.numeric(flit[[j]])
    flit <- arrange(flit, V1, month)

    x <- data.frame(year=flit$V1, met_year=NA, yr_part=flit$V1 + (2 * flit$month - 1)/24, month=flit$month, temp=KtoC(flit$temp), check.names=FALSE, stringsAsFactors=FALSE)

    modelDesignation <- "m" %_% sprintf("%04d", i)
    x[[modelDesignation]] <- x$temp

    if (!is.null(baseline)) {
      flit <- subset(x, x$year %in% baseline)
      bma <- tapply(flit$temp, flit$month, mean, na.rm=TRUE)
      x$base <- NA
      null <- sapply(names(bma), function(s) { v <- bma[s]; if (is.nan(v)) v <- 0.0; x$base[x$month == s] <<- v }); null <- NULL

      ## Center anomalies on average baseline-period temperatures.
      x[[modelDesignation]] <- round(x$temp - x$base, 3L)
    }
    x <- x[, c(common_columns, modelDesignation)]

    attr(x, "baseline") <- baseline
    attr(x, "member") <- sub("\\.[^.]*$", "", basename(files[i]))

    rm_ <- str_match(attr(x, "member"), "(20c3m|sres|rcp)([ab]\\db?|\\d)?(\\d)?")
    flit <- rm_[1L, 2L:ncol(rm)]; flit <- flit[flit != ""]
    scenario <- toupper(paste(flit, collapse=" "))
    if (flit[1L] == "rcp") # For the CMIP5 scenarios.
      scenario <- toupper(flit[1] %_% " " %_% paste(flit[2:3], collapse="."))
    attr(x, "scenario") <- scenario

    e[[modelDesignation]] <- x
  }

  d <- NULL
  for (i in ls(e)) {
    if (is.null(d))
      d <- e[[i]][, c(common_columns, i)]
    else
      d <- merge(d, e[[i]][, c(common_columns, i)], by=common_columns, all=TRUE)
  }

  met_year <- shift(d$year, -1, roll=FALSE)
  met_year[is.na(met_year)] <- max(d$year, na.rm=TRUE) + 1
  d$met_year <- met_year

  attr(d, "ensemble") <- ifelse(ensemble == "cmip3", "CMIP3+", "CMIP5")

  attr(d, "scenario") <- factor(sapply(ls(e), function(s) { attr(e[[s]], "scenario") }))

  attr(d, "baseline") <- NULL
  if (length(e) > 0L)
    attr(d, "baseline") <- attr(e[[ls(e)[1L]]], "baseline")

  if (!is.null(save) && save) { # I.e. save the CMIP data set.
    local({
      assign(ensemble, d)
      cmipPath <- paste(path, ensemble %_% ifelse(is.null(baseline), "_raw", ""), sep="/")

      eval(substitute(save(o, file=cmipPath %_% ".RData"), list(o=as.symbol(ensemble))))
      dput(get(ensemble), file=cmipPath %_% ".dput")
      write.csv(get(ensemble), file=cmipPath %_% ".csv", row.names=FALSE)
    })
  }

  if (ensemble == "cmip3" && !cmip3_raw && subdir == "all members")
    d <- AlignCmip3Data(d)

  return (d)
}
