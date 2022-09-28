#' @export
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

#' @import ggplot2
#' @export
plot_priors <- function(priors, replicates = 1000, geom = geom_histogram, ...) {
  samples_df <- simulate_priors(priors, replicates)
  ggplot(samples_df) +
    geom(aes(value, fill = prior), ...) +
    facet_wrap(~ prior)
}
