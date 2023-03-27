#' Plot histogram of posterior distribution(s)
#' @export hist.demografr_abc
#' @export
hist.demografr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    check_param_presence(colnames(params), param)
    x$numparam <- 1
    x$names$parameter.names <- param
    x$adj.values <- x$adj.values[, param, drop = FALSE]
    x$unadj.values <- x$unadj.values[, param, drop = FALSE]
  }
  abc:::hist.abc(x, ...)
}
