#' Extract inferred posterior(s) as a standard data frame
#'
#' @param abc An object produced by \code{run_abc}
#' @param param A character vector containing either parameter names to summarize,
#'   or a regex-like matches to be used for subsetting. If \code{NULL} (the default),
#'   all parameters will be extracted.
#' @param posterior Should an "adj"-usted or "unadj"-usted posterior be extracted?
#'   (Default is "adj").
#'
#' @return A data frame in the long format with posterior values of parameters
#'
#' @export
extract_posterior <- function(abc, param = NULL, posterior = c("adj", "unadj")) {
  posterior <- match.arg(posterior)
  components <- attr(abc, "components")

  param <- subset_parameters(subset = param, all = colnames(components$parameters))

  # TODO check demographr_abc type

  # get the entire posterior sample, convert it to a long format, subset variables
  df <- abc[[paste0(posterior, ".values")]] %>%
    as.data.frame() %>%
    stats::setNames(colnames(components$parameters)) %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "param", values_to = "value") %>%
    dplyr::filter(param %in% param)

  df <- df[df$param %in% param, ]

  df
}
