#' Plot prior distribution(s)
#'
#' @param x Either an object of the class \code{demografr_abc}, or a list of prior
#'   sampling statements
#' @param param A character vector containing either parameter names to summarize,
#'   or a regex-like matches to be used for subsetting.
#' @param replicates How many samples to simulate from each prior for plotting?
#' @param geom Either \code{ggplot2::geom_histogram} or \code{ggplot2::geom_density}
#'
#' @import ggplot2
#' @export
plot_prior <- function(x, param = NULL, replicates = 10000, geom = ggplot2::geom_density,
                       facets = FALSE) {
  priors <- if (inherits(x, "demografr_abc")) attr(x, "priors") else x
  all_params <- get_prior_names(priors); names(priors) <- all_params
  subset_params <- subset_parameters(subset = param, all = all_params)
  priors <- priors[subset_params]; unname(priors)

  samples_df <- simulate_priors(priors, replicates)

  p_facet <- if (facets) facet_wrap(~ param, scales = "free") else NULL

  p <- ggplot(samples_df) +
    geom(aes(value, fill = param, color = param), alpha = 0.5) +
    scale_fill_discrete(drop = FALSE) +
    scale_color_discrete(drop = FALSE) +
    theme_minimal() +
    theme(strip.text.x = element_text(face = "bold", size = 13)) +
    p_facet

  p
}

simulate_priors <- function(priors, replicates = 1000) {
  if (!is.list(priors)) priors <- list(priors)

  vars <- get_prior_names(priors)

  samples_list <- lapply(
    seq_along(priors), \(i)
    dplyr::tibble(
      param = vars[i],
      value = replicate(n = replicates, sample_prior(priors[[i]])$value),
      stringsAsFactors = FALSE
    )
  )

  samples_df <- do.call(rbind, samples_list)
  samples_df
}
