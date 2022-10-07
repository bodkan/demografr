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
  posterior_stats <- quiet(summary(abc))["Weighted Mean:", param] %>%
    data.frame(param = param, value = ., stringsAsFactors = FALSE)

  ggplot(df[df$param %in% param, ]) +
    geom(aes(value, fill = param, color = param), ...) +
    geom_vline(data = posterior_stats, aes(xintercept = value, color = param), linetype = 2) +
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
