
#' @export
change_luminance <- function(col, lum=1.0) {
 as.vector(apply(sapply(col, col2rgb) / 255, 2, function(x) {
 x <- x + lum; x[x > 1.0] <- 1.0; rgb(x[1], x[2], x[3])
 }))
 } # Also see 'scales::col2hcl()'.


#' @export
vline <- function(mark_years, abline...=list(), text...=list()){
  ablineArgs <- list(
    v = mark_years,
    col = scales::alpha("black", 0.4),
    lty = "dashed"
  )
  ablineArgs <- utils::modifyList(ablineArgs, abline..., keep.null = TRUE)

  do.call("abline", ablineArgs)

  textArgs <- list(
    x = mark_years,
    y = par("yaxp")[2L],
    labels = mark_years,
    cex = 0.8,
    srt = 270,
    adj = c(NA, -0.25)
  )
  textArgs <- utils::modifyList(textArgs, text..., keep.null = TRUE)

  do.call("text", textArgs)

  nop()
}
