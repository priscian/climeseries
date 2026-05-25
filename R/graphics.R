#' @export
vline <- function(mark_years, abline... = list(), text... = list()) {
  text... <- utils::modifyList(list(y = par("yaxp")[2L]), text...)
  keystone::vline(mark_x = mark_years, abline... = abline..., text... = text...)
}
