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

# Subset prior formulas to just those of a given type
subset_priors <- function(priors, type) {
  Filter(function(p) match_prior_type(p, type), priors)
}

# Check if the provided prior formula contains the specified parameter type
# (i.e. "Ne", "T_split", etc.)
match_prior_type <- function(formula, type) {
  if (!length(formula)) return(FALSE)

  variable <- as.list(formula)[[2]]
  grepl(type, variable)
}