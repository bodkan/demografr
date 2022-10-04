# custom plotting functions -----------------------------------------------

#' @export
plot_prior <- function(priors, replicates = 1000, geom = ggplot2::geom_histogram) {
  samples_df <- simulate_priors(priors, replicates)
  ggplot2::ggplot(samples_df) +
    geom(ggplot2::aes(value, fill = param, color = param)) +
    ggplot2::facet_wrap(~ param)
}

#' @export
plot_posterior <- function(x, param = colnames(attr(x, "parameters")), type = c("adj", "unadj"),
                            geom = ggplot2::geom_density) {
  df <- extract_posterior(x, type)

  # compute a given summary statistic of the posterior
  summary_df <- quiet(summary(result))["Weighted Mean:", param]

  ggplot2::ggplot(df) +
    geom(ggplot2::aes(value, fill = param, color = param)) +
    ggplot2::geom_vline(xintercept = summary_df, linetype = 2, alpha = 0.75)
}


# overloaded functions from the abc package -------------------------------

#' @export hist.demographr_abc
hist.demographr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    x$numparam <- 1
    x$names$parameter.names <- param
    params <- params[, param]
  }
  abc:::hist.abc(x, ...)
}

#' @export plot.demographr_abc
plot.demographr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    x$numparam <- 1
    params <- params[, param]
  }
  abc:::plot.abc(x, param = params, ...)
}

# a function to silence the unnecessary summary() output on abc objects
# https://stackoverflow.com/a/54136863
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# used by plot_priors
#' @export
simulate_priors <- function(priors, replicates = 1000) {
  if (!is.list(priors)) priors <- list(priors)

  vars <- prior_variables(priors)

  samples_list <- lapply(seq_along(priors), \(i) data.frame(
    param = vars[i],
    value = replicate(n = replicates, sample_prior(priors[[i]])$value),
    stringsAsFactors = FALSE
  ))

  samples_df <- do.call(rbind, samples_list)
  samples_df
}

#' @export
extract_posterior <- function(x, type = c("adj", "unadj")) {
  type <- match.arg(type)
  # TODO check demographr_abc type

  # get the entire posterior sample, convert it to a long format, subset variables
  df <- x[[paste0(type, ".values")]] %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "param", values_to = "value") %>%
    dplyr::filter(param %in% param)

  df
}