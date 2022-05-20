
#' plot_add_lines_maybe
#'
#' Given plot_ly object and a set of table of lines, add the lines to
#' the object. Act as a passthrough if the lines table is omitted or
#' empty.
#'
#' @param p plot_ly object
#' @param lines Lines to add
#' @param ... Additional params for plotly::add_lines
#'
#' @return modified plot_ly object
plot_add_lines_maybe <- function(p, lines = NULL, ...) {
  if (!is.null(lines) && nrow(lines) > 0) {
    data <- plotly::highlight_key(lines, ~LineId, group = "line-highlight")
    plotly::add_lines(p, data = data, ...)
  } else {
    p
  }
}



