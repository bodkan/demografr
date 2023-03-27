
#' Extract inferred posterior(s) as a standard data frame
#' @export
extract_posterior <- function(abc, posterior = c("adj", "unadj")) {
  posterior <- match.arg(posterior)
  # TODO check demographr_abc type

  # get the entire posterior sample, convert it to a long format, subset variables
  df <- abc[[paste0(posterior, ".values")]] %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "param", values_to = "value") %>%
    dplyr::filter(param %in% param)

  df
}
