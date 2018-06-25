### Code modified from 'stats:::window.default()'.

window_default <- function (x, start = NULL, end = NULL, frequency = NULL, deltat = NULL,
    extend = FALSE, ...)
{
    x <- hasTsp(x)
    xtsp <- tsp(x)
    xfreq <- xtsp[3L]
    xtime <- time(x)
    ts.eps <- getOption("ts.eps")
    if (!is.null(frequency) && !is.null(deltat) && abs(frequency *
        deltat - 1) > ts.eps)
        stop("'frequency' and 'deltat' are both supplied and are inconsistent")
    if (is.null(frequency) && is.null(deltat))
        yfreq <- xfreq
    else if (is.null(deltat))
        yfreq <- frequency
    else if (is.null(frequency))
        yfreq <- 1/deltat
    thin <- round(xfreq/yfreq)
    if (yfreq > 0 && abs(xfreq/yfreq - thin) < ts.eps) {
        yfreq <- xfreq/thin
    }
    else {
        thin <- 1
        yfreq <- xfreq
        warning("'frequency' not changed")
    }
    start <- if (is.null(start))
        xtsp[1L]
    else switch(length(start), start, start[1L] + (start[2L] -
        1)/xfreq, stop("bad value for 'start'"))
    if (start < xtsp[1L] - ts.eps/xfreq && !extend) {
        start <- xtsp[1L]
        warning("'start' value not changed")
    }
    end <- if (is.null(end))
        xtsp[2L]
    else switch(length(end), end, end[1L] + (end[2L] - 1)/xfreq,
        stop("bad value for 'end'"))
    if (end > xtsp[2L] + ts.eps/xfreq && !extend) {
        end <- xtsp[2L]
        warning("'end' value not changed")
    }
    if (start > end)
        stop("'start' cannot be after 'end'")
    if (!extend) {
        if (all(abs(start - xtime) > ts.eps/xfreq))
            start <- xtime[(xtime > start) & ((start + 1/xfreq) >
                xtime)]
        if (all(abs(end - xtime) > ts.eps/xfreq))
            end <- xtime[(xtime < end) & ((end - 1/xfreq) < xtime)]
        i <- seq.int(trunc((start - xtsp[1L]) * xfreq + 1.5),
            trunc((end - xtsp[1L]) * xfreq + 1.5), by = thin)
        y <- if (is.matrix(x))
            x[i, , drop = FALSE]
        else x[i]
        ystart <- xtime[i[1L]]
        yend <- xtime[i[length(i)]]
        #attr(y, "tsp") <- c(ystart, yend, yfreq)
        y <- make_time_series_from_anomalies(y, frequency = yfreq, conf_int=TRUE)
    }
    else {
        stoff <- ceiling((start - xtsp[1L]) * xfreq - ts.eps)
        ystart <- (round(xtsp[1L] * xfreq) + stoff)/xfreq
        enoff <- floor((end - xtsp[2L]) * xfreq + ts.eps)
        yend <- (round(xtsp[2L] * xfreq) + enoff)/xfreq
        nold <- round(xfreq * (xtsp[2L] - xtsp[1L])) + 1
        i <- if (start > xtsp[2L] + ts.eps/xfreq || end < xtsp[1L] -
            ts.eps/xfreq)
            rep(nold + 1, floor(1 + (end - start) * xfreq + ts.eps))
        else {
            i0 <- 1 + max(0, stoff)
            i1 <- nold + min(0, enoff)
            c(rep.int(nold + 1, max(0, -stoff)), if (i0 <= i1) i0:i1,
                rep.int(nold + 1, max(0, enoff)))
        }
        y <- if (is.matrix(x))
            rbind(x, NA)[i, , drop = FALSE]
        else c(x, NA)[i]
        ## N.B. This fails for some time series with non-zero 'tsp' starts, so replace it & return a 'ts' object:
        #attr(y, "tsp") <- c(ystart, yend, xfreq)
        y <- make_time_series_from_anomalies(y, frequency = xfreq, conf_int=TRUE)
        if (yfreq != xfreq)
            y <- Recall(y, frequency = yfreq)
    }
    y
}
