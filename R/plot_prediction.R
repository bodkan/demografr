#' Plot the results of a posterior predictive check for a given summary statistic
#'
#' @param data Data frame with predictions generated from ABC posteriors, as generated
#'   by the \code{predict} function
#' @param stat Which tree-sequence summary statistic to extract? If \code{NULL}, all summary
#'   statistics will be returned.
#' @param file Output file for a figure saved via \code{ggsave}
#' @param facets Should each summary statistic be plotted on its own facet (default is
#'   \code{TRUE})?
#' @param ... Optional argument which will be passed to \code{ggsave}
#'
#' @return If \code{file = NULL}, a ggplot2 object, otherwise nothing is returned and the
#'   plot is instead saved to a file.
#'
#' @export
plot_prediction <- function(data, stat = NULL, file = NULL, facets = TRUE, ...) {
  observed <- attr(data, "components")$observed

  all_stats <- names(observed)
  if (is.null(stat))
    stat <- all_stats
  if (!all(stat %in% all_stats))
    stop("Unknown statistic(s) '", paste(stat, collapse = ", "), "' specified", call. = FALSE)

  observed_df <-
    lapply(
      stat,
      function(s) {
        df <- observed[[s]]

        value_col <- vapply(df, is.numeric, FUN.VALUE = logical(1))

        stat_names <- colnames(df)[!value_col]
        value_name <- colnames(df)[value_col]

        dplyr::tibble(
          summary = s,
          stat = paste0(s, "_", apply(df[, stat_names, drop = FALSE], 1, paste, collapse = "_")),
          value = df[, value_col]
        )
      }
    ) %>% do.call(rbind, .)

  predicted_df <- extract_prediction(data, stat)

  color_by <- as.symbol(if (length(stat) > 1) "summary" else "stat")
  scales <- if (length(stat) > 1) "free_x" else NULL

  model_name <- attr(data, "components")$model_name

  p <-
    ggplot2::ggplot(predicted_df, ggplot2::aes(value, color = !!color_by, fill = !!color_by)) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::geom_vline(data = observed_df, ggplot2::aes(xintercept = value, color = !!color_by, linetype = "observed")) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(color = ggplot2::guide_legend("simulated"),
                      fill = ggplot2::guide_legend("simulated"),
                      linetype = ggplot2::guide_legend("observed")) +
      ggplot2::scale_linetype_manual(values = "dashed", labels = "") +
      ggplot2::ggtitle(paste0("Posterior predictive check for model '", model_name, "'"))

  if (facets) p <- p + ggplot2::facet_wrap(~ stat, scales = scales)

  if (!is.null(file))
    ggplot2::ggsave(file, plot = p, ...)
  else
    p
}
