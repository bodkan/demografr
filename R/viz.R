# custom plotting functions -----------------------------------------------

#' @export
plot_priors <- function(priors, replicates = 1000, geom = ggplot2::geom_histogram, ...) {
  samples_df <- simulate_priors(priors, replicates)
  ggplot2::ggplot(samples_df) +
    geom(aes(value, fill = prior), ...) +
    ggplot2::facet_wrap(~ prior)
}

#' @export
plot_posteriors <- function(x, param = colnames(attr(x, "parameters")), type = c("adj", "unadj"),
                            geom = ggplot2::geom_density) {
  type <- match.arg(type)
  # TODO check demographr_abc type

  # get the entire posterior sample, convert it to a long format, subset variables
  df <- x[[paste0(type, ".values")]] %>%
    dplyr::as_tibble() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "parameter", values_to = "value") %>%
    dplyr::filter(parameter %in% param)

  # compute a given summary statistic of the posterior
  summary_df <- quiet(summary(result))["Weighted Mean:", param]

  df %>%
    ggplot2::ggplot(aes(value, color = parameter)) +
    geom() +
    ggplot2::geom_vline(xintercept = summary_df, linetype = 2, alpha = 0.75)
}


# overloaded functions from the abc package -------------------------------

#' @importFrom abc hist.abc
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

#' @importFrom ape plot.abc
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
simulate_priors <- function(priors, replicates = 1000) {
  if (!is.list(priors)) priors <- list(priors)

  vars <- prior_variables(priors)

  samples_list <- lapply(seq_along(priors), \(i) data.frame(
    prior = vars[i],
    value = replicate(n = replicates, sample_prior(priors[[i]])$value),
    stringsAsFactors = FALSE
  ))

  samples_df <- do.call(rbind, samples_list)
  samples_df
}

