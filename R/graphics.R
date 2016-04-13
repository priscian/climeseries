#' @export
vary_brightness <- function(color, len, end=NULL)
{
  # 'color' is a single value of any of the three kinds of R color specifications, i.e. either a color name (as listed by 'colors()'), a hexadecimal string of the form "#rrggbb", or a positive integer i meaning 'palette()[i]'.
  # 'len' is the number of brightness values to be in the palette.
  # 'end' is the ending brightness level in the palette; if NULL, 'end' will be set to half the brightness level of 'color' (i.e. the palette will move from brighter to darker).

  if (length(color) > 1L) color <- color[1L]
  startColorHsv <- rgb2hsv(col2rgb(color))

  BuildHsvMatrix <- function(mat, n) { if (n == 0) return (mat); BuildHsvMatrix(cbind(mat, mat[, 1L]), n - 1L) }
  hsvMat <- BuildHsvMatrix(startColorHsv, len - 1L)

  start <- hsvMat["v", 1L]
  if (is.null(end))
    end <- hsvMat["v", 1L] * 0.5

  hsvMat["v", ] <- seq(start, end, length.out=ncol(hsvMat))

  colorsOut <- apply(hsvMat, 2, function(x) do.call(hsv, as.list(x)))

  return (colorsOut)
}


#' @export
change_luminance <- function(col, lum=1.0) { as.vector(apply(sapply(col, col2rgb) / 255, 2, function(x) { x <- x + lum; x[x > 1.0] <- 1.0; rgb(x[1], x[2], x[3]) })) } # Also see 'scales::col2hcl()'.
