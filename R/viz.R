# custom plotting functions -----------------------------------------------

#' @import ggplot2
#' @export
plot_prior <- function(priors, replicates = 1000, geom = ggplot2::geom_histogram) {
  samples_df <- simulate_priors(priors, replicates)
  ggplot(samples_df) +
    geom(aes(value, fill = param, color = param)) +
    facet_wrap(~ param) +
    guides(fill = guide_legend(""), color = guide_legend(""))
}

#' @import ggplot2
#' @export
plot_posterior <- function(abc, param = NULL, type = c("adj", "unadj"),
                           geom = ggplot2::geom_density, ...) {
  df <- extract_posterior(abc, type)

  if (is.null(param)) param <- colnames(attr(abc, "parameters"))   

  # compute a given summary statistic of the posterior
  summary_df <- quiet(summary(abc))["Weighted Mean:", param]

  ggplot(df[df$param %in% param, ]) +
    geom(aes(value, fill = param, color = param), ...) +
    geom_vline(xintercept = summary_df, linetype = 2, alpha = 0.75) +
    guides(fill = guide_legend(""), color = guide_legend(""))
}

# overloaded functions from the abc package -------------------------------

#' @export hist.demografr_abc
#' @export
hist.demografr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    x$numparam <- 1
    x$names$parameter.names <- param
    params <- params[, param]
  }
  abc:::hist.abc(x, ...)
}

#' @export plot.demografr_abc
#' @export
plot.demografr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    x$numparam <- 1
    params <- params[, param]
  }
  abc:::plot.abc(x, param = params, ...)
}

#' @export
simulate_priors <- function(priors, replicates = 1000) {
  if (!is.list(priors)) priors <- list(priors)

  vars <- prior_variables(priors)

  samples_list <- lapply(seq_along(priors), \(i) data.frame(
    param = vars[i],
    value = replicate(n = replicates, sample_prior(priors[[i]])$value),
    stringsAsFactors = FALSE
  ))

  samples_df <- dplyr::as_tibble(do.call(rbind, samples_list))
  samples_df
}

#' @export
extract_posterior <- function(abc, type = c("adj", "unadj")) {
  type <- match.arg(type)
  # TODO check demographr_abc type

  # get the entire posterior sample, convert it to a long format, subset variables
  df <- abc[[paste0(type, ".values")]] %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "param", values_to = "value") %>%
    dplyr::filter(param %in% param)

  df
}
