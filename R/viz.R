# custom plotting functions -----------------------------------------------

#' @import ggplot2
#' @export
plot_prior <- function(x, type = NULL, replicates = 10000, geom = ggplot2::geom_histogram) {
  priors <- if (inherits(x, "demografr_abc")) attr(x, "priors") else x

  if (!is.null(type))
    priors <- subset_priors(priors, type)

  samples_df <- simulate_priors(priors, replicates)
  samples_df$type <- gsub("(Ne|Tgf|Tsplit|gf)_.*", "\\1", samples_df$param)

  ggplot(samples_df) +
    geom(aes(value, fill = type, color = type)) +
    facet_wrap(~ param, scales = "free_x") +
    guides(fill = guide_legend("prior type"), color = guide_legend("prior type") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)))
}

#' @import ggplot2
#' @export
plot_posterior <- function(abc, param = NULL, type = NULL, posterior = c("adj", "unadj"),
                           geom = ggplot2::geom_density, facets = TRUE, ...) {
  df <- extract_posterior(abc, posterior)
  df$type <- gsub("(Ne|Tgf|Tsplit|gf)_.*", "\\1", df$param)

  if (!is.null(type)) {
    type <- match.arg(type, c("Ne", "Tgf", "Tsplit", "gf"))
    df <- df[df$type == type, ]
    param <- unique(df$param)
  }

  if (is.null(param))
    param <- colnames(attr(abc, "parameters"))   

  # compute a given summary statistic of the posterior
  posterior_stats <- quiet(summary(abc))["Weighted Mean:", param] %>%
    data.frame(param = param, value = ., stringsAsFactors = FALSE)

  p <- ggplot(df[df$param %in% param, ]) +
    geom(aes(value, fill = type, color = type), alpha = 0.5, ...) +
    geom_vline(data = posterior_stats, aes(xintercept = value), linetype = 2) +
    guides(fill = guide_legend("parameter\ntype"), color = guide_legend("parameter\ntype")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA))

  if (facets) p <- p + facet_wrap(~ param)

  p
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
