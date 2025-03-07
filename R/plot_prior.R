#' Plot prior distribution(s)
#'
#' @param x Either an object of the class \code{demografr_abc}, or a list of prior
#'   sampling statements
#' @param param A character vector containing either parameter names to summarize,
#'   or a regex-like matches to be used for subsetting. If \code{NULL} (the default),
#'   all parameters will be extracted.
#' @param facets Should individual parameters be plotting on a facet each?
#' @param file Output file for a figure saved via \code{ggsave}
#' @param replicates How many samples to simulate from each prior for plotting?
#' @param geom Either \code{ggplot2::geom_histogram} or \code{ggplot2::geom_density}
#' @param ... Optional argument which will be passed to \code{ggsave}
#'
#' @return A ggplot2 plot object
#'
#' @export
plot_prior <- function(x, param = NULL, facets = FALSE, file = NULL,
                       replicates = 10000, geom = ggplot2::geom_density, ...) {
  priors <- if (inherits(x, "demografr_abc.abc")) attr(x, "priors") else x
  all_params <- get_param_names(priors); names(priors) <- all_params
  subset_params <- subset_parameters(subset = param, all = all_params)
  priors <- priors[subset_params]; unname(priors)

  samples_df <- simulate_priors(priors, replicates)

  p_facet <- if (facets) ggplot2::facet_wrap(~ param, scales = "free") else NULL

  p <- ggplot2::ggplot(samples_df) +
    geom(ggplot2::aes(value, fill = param, color = param), alpha = 0.5) +
    ggplot2::scale_fill_discrete(drop = FALSE) +
    ggplot2::scale_color_discrete(drop = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(face = "bold", size = 13)) +
    p_facet

  if (!is.null(file))
    ggplot2::ggsave(file, plot = p, ...)
  else
    p
}

simulate_priors <- function(priors, replicates = 1000) {
  if (!is.list(priors)) priors <- list(priors)

  vars <- get_param_names(priors)

  samples_list <- lapply(
    seq_along(priors), function(i) {
      dplyr::tibble(
        param = vars[i],
        value = replicate(n = replicates, sample_prior(priors[[i]])$value),
        stringsAsFactors = FALSE
      )
    }
  )

  samples_df <- do.call(rbind, samples_list)
  samples_df
}
