#' Plot prior distribution(s)
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
    guides(fill = guide_legend("type"),
           color = guide_legend("type")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal()

  scales <- if (length(unique(samples_df$type)) == 1) "fixed" else "free"
  p <- p + facet_wrap(~ param, scales = scales)

  p
}
