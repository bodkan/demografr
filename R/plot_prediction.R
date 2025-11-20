#' Plot the result of a posterior predictive check for a given summary statistic
#'
#' @param data Data frame with predictions generated from ABC posteriors, as generated
#'   by the \code{predict} function
#' @param stat Which tree-sequence summary statistic to extract? If \code{NULL}, all
#'   summary statistics will be visualized.
#' @param file Output path to a PDF figure to be saved
#' @param facets Should each summary statistic be plotted on its own facet (default is
#'   \code{TRUE})?
#' @param ... Optional argument which will be passed to \code{ggsave}
#'
#' @return If \code{file = NULL}, a ggplot2 object (or a list of such objects, if
#'   multiple summary statistics are visualized), otherwise nothing is returned and the
#'   plots are instead saved to a file.
#'
#' @examples
#' \dontrun{
#' # read statistics computed from posterior sample simulations
#' pred <- readRDS(url(paste0(
#' "https://raw.githubusercontent.com/bodkan/demografr/",
#' "refs/heads/main/inst/examples/downstream_predX.rds"
#' )))
#'
#' # note the columns `diversity`, `divergence`, and `f4`
#' pred
#'
#' # extract_prediction() is a convenience function which unpacks the given
#' # summary statistic in a normal data frame column (here `diversity`)
#' extract_prediction(pred, "diversity")
#'
#' # we can also plot the posterior predictive distributions for all statistics
#' plot_prediction(pred, "diversity")
#' }
#'
#' @export
#' @import ggplot2
plot_prediction <- function(data, stat = NULL, file = NULL, facets = TRUE, ...) {
  observed <- attr(data, "components")$observed

  all_stats <- names(observed)
  if (is.null(stat))
    stat <- all_stats
  if (!all(stat %in% all_stats))
    stop("Unknown statistic(s) '", paste(stat, collapse = ", "), "' specified", call. = FALSE)

  p_stats <- list()

  for (s in stat) {
    is_vector_stat <- all(vapply(observed[[s]], is.numeric, FUN.VALUE = logical(1)))

    if (is_vector_stat)
      observed_df <- observed[[s]]
    else {
      x <- observed[[s]]
      value_col <- vapply(names(x), function(i) is.numeric(x[[i]]), FUN.VALUE = logical(1))
      stat_names <- colnames(x)[!value_col]
      value_name <- colnames(x)[value_col]

      observed_df <- dplyr::tibble(
        summary = s,
        stat = paste0(s, "_", apply(x[, stat_names, drop = FALSE], 1, paste, collapse = "_")),
        value = x[, value_col, drop = TRUE]
      )
    }

    predicted_df <- extract_prediction(data, s)

    if (is_vector_stat) {
      x <- as.symbol(colnames(observed_df)[1])
      y <- as.symbol(colnames(observed_df)[2])

      p <-
        ggplot2::ggplot() +
          ggplot2::geom_line(data = predicted_df[[s]], ggplot2::aes(!!x, !!y, color = rep, group = rep), alpha = 0.5) +
          ggplot2::geom_line(data = observed_df, ggplot2::aes(!!x, !!y, linetype = "observed"), color = "red") +
          ggplot2::scale_linetype_manual(name = "observed", values = "dashed", labels = "") +
          ggplot2::scale_color_continuous(name = "samples") +
          ggplot2::theme(legend.position = "bottom")
    } else {
      color_by <- as.symbol(if (length(s) > 1) "summary" else "stat")
      scales <- if (length(stat) > 1) "free" else NULL

      p <-
        ggplot2::ggplot(predicted_df[[s]], ggplot2::aes(value, color = !!color_by, fill = !!color_by)) +
          ggplot2::geom_density(alpha = 0.5) +
          ggplot2::geom_vline(data = observed_df, ggplot2::aes(xintercept = value, color = !!color_by, linetype = "observed")) +
          ggplot2::guides(color = ggplot2::guide_legend("simulated"),
                          fill = ggplot2::guide_legend("simulated"),
                          linetype = ggplot2::guide_legend("observed")) +
        ggplot2::scale_linetype_manual(name = "observed", values = "dashed", labels = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    }

    if (facets && !is_vector_stat) p <- p + ggplot2::facet_wrap(~ stat, scales = scales)

    model_name <- attr(data, "components")$model_name
    p <- p +
      ggplot2::ggtitle(paste0("Posterior predictive check for model '", model_name, "' and statistic '", s, "'"))

    p_stats[[length(p_stats) + 1]] <- p
  }
  names(p_stats) <- stat

  if (!is.null(file)) {
    grDevices::pdf(file, ...)
    for (p in p_stats) print(p)
    grDevices::dev.off()
  } else {
    if (length(p_stats) == 1)
      return(p_stats[[1]])
    else
      return(p_stats)
  }
}
