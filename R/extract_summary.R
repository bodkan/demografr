#' Extract table of estimated model parameters
#'
#' @param abc ABC object generated by \code{run_abc}
#' @param param A character vector containing either parameter names to summarize,
#'   or a regex-like matches to be used for subsetting. If \code{NULL} (the default),
#'   all parameters will be extracted.
#'
#' @return A data frame object with posterior summary statistics
#'
#' @examples
#' # read example ABC result with an inferred joint posterior distribution
#' abc_res <- readRDS(system.file("examples/basics_abc.rds", package = "demografr"))
#'
#' extract_summary(abc_res)
#'
#' @import abc
#' @export
extract_summary <- function(abc, param = NULL) {
  # TODO: there is a new warning in a density calculation performed in abc's summary method
  # via built-in density function:
  # https://bugs.r-project.org/show_bug.cgi?id=18490
  posterior_df <- as.data.frame.matrix(quiet(summary(abc)))

  params <- subset_parameters(subset = param, all = colnames(posterior_df))

  posterior_df[, params, drop = FALSE]
}
