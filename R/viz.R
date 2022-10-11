# custom plotting functions -----------------------------------------------

#' @import ggplot2
#' @export
plot_prior <- function(x, type = NULL, replicates = 10000, geom = ggplot2::geom_histogram) {
  priors <- if (inherits(x, "demografr_abc")) attr(x, "priors") else x

  if (!is.null(type))
    priors <- subset_priors(priors, type)

  samples_df <- simulate_priors(priors, replicates)
  samples_df$type <- gsub("(Ne|Tgf|Tsplit|gf)_.*", "\\1", samples_df$param)

  p <- ggplot(samples_df) +
    geom(aes(value, fill = type, color = type)) +
    facet_wrap(~ param, scales = "free") +
    guides(fill = guide_legend("prior type"), color = guide_legend("prior type")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal()

  scales <- if (length(unique(samples_df$type)) == 1) "fixed" else "free"
  p <- p + facet_wrap(~ param, scales = scales)

  p
}

#' @import ggplot2
#' @export
plot_posterior <- function(abc, param = NULL, type = NULL, posterior = c("adj", "unadj"),
                           summary = c("mode", "mean", "median"),
                           geom = ggplot2::geom_density, facets = TRUE, xlim = NULL, ...) {
  df <- extract_posterior(abc, posterior)
  df$type <- gsub("(Ne|Tgf|Tsplit|gf)_.*", "\\1", df$param)

  # subset to a given type of parameters
  if (!is.null(type)) {
    type <- match.arg(type, c("Ne", "Tgf", "Tsplit", "gf"))
    df <- df[df$type == type, ]
    param <- unique(df$param)
  }

  # if requested, subset to a given set of parameters
  if (is.null(param))
    param <- colnames(attr(abc, "parameters"))   
  else {
    check_param_presence(unique(df$param), param)
    df <- df[df$param %in% param, ]
  }

  # compute a given summary statistic of the posterior which will be added
  # as a vertical line overlaid on top of the distribution plot
  summary_df <- extract_posterior_summary(abc, summary)
  summary_df <- summary_df[summary_df$param %in% param, ]

  p <- ggplot(df[df$param %in% param, ]) +
    geom(aes(value, fill = type, color = type), alpha = 0.5, ...) +
    # geom_vline(data = summary_df, aes(xintercept = value), linetype = 2) +
    guides(fill = guide_legend("parameter\ntype"),
           color = guide_legend("parameter\ntype")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal() +
    theme(strip.text.x = element_text(face = "bold", size = 15)) +
    coord_cartesian(xlim = xlim)

  if (facets) {
    scales <- if (length(unique(df$type)) == 1) "fixed" else "free"
    p <- p + facet_wrap(~ param, scales = scales)
  }

  p
}

# overloaded functions from the abc package -------------------------------

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

#' @export plot.demografr_abc
#' @export
plot.demografr_abc <- function(x, param = NULL, ...) {
  params <- attr(x, "parameters")
  if (!is.null(param)) {
    check_param_presence(colnames(params), param)
    x$numparam <- 1
    x$names$parameter.names <- param
    x$adj.values <- x$adj.values[, param, drop = FALSE]
    x$residuals <- x$residuals[, param, drop = FALSE]
    x$unadj.values <- x$unadj.values[, param, drop = FALSE]
    params <- params[, param, drop = FALSE]
  }
  abc:::plot.abc(x, param = params, ...)
}
