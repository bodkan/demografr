#' Plot the results of a posterior predictive check for a given summary statistic
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
#' @export
plot_prediction <- function(data, stat, file = NULL, facets = TRUE, ...) {
  observed <- attr(data, "components")$observed

  all_stats <- names(observed)
  if (is.null(stat))
    stat <- all_stats
  if (!all(stat %in% all_stats))
    stop("Unknown statistic(s) '", paste(stat, collapse = ", "), "' specified", call. = FALSE)

  p_stats <- list()

  for (s in stat) {
    is_vector_stat <- all(vapply(observed[[s]], is.numeric, FUN.VALUE = logical(1)))

    if (is_vector_stat) {
      observed_df <- observed[[s]]
    } else {
      observed_df <-
        lapply(
          s,
          function(i) {
            df <- observed[[i]]

            value_col <- vapply(df, is.numeric, FUN.VALUE = logical(1))

            stat_names <- colnames(df)[!value_col]
            value_name <- colnames(df)[value_col]

            dplyr::tibble(
              summary = i,
              stat = paste0(i, "_", apply(df[, stat_names, drop = FALSE], 1, paste, collapse = "_")),
              value = df[, value_col]
            )
          }
        ) %>% do.call(rbind, .)
    }

    predicted_df <- extract_prediction(data, s)

    if (is_vector_stat) {
      x <- as.symbol(colnames(observed_df)[1])
      y <- as.symbol(colnames(observed_df)[2])
      p <-
        ggplot() +
          geom_line(data = predicted_df, aes(!!x, !!y, color = rep, group = rep), alpha = 0.5) +
          geom_line(data = observed_df, aes(!!x, !!y, linetype = "observed"), color = "red") +
          scale_linetype_manual(name = "observed", values = "dashed", labels = "") +
          scale_color_continuous(name = "samples")
    } else {
      color_by <- as.symbol(if (length(s) > 1) "summary" else "stat")
      scales <- if (length(stat) > 1) "free" else NULL

      p <-
        ggplot(predicted_df, aes(value, color = !!color_by, fill = !!color_by)) +
          geom_density(alpha = 0.5) +
          geom_vline(data = observed_df, aes(xintercept = value, color = !!color_by, linetype = "observed")) +
          guides(color = guide_legend("simulated"),
                 fill = guide_legend("simulated"),
                 linetype = guide_legend("observed")) +
        scale_linetype_manual(name = "observed", values = "dashed", labels = "")
    }

    if (facets && !is_vector_stat) p <- p + ggplot2::facet_wrap(~ stat, scales = scales)

    model_name <- attr(data, "components")$model_name
    p <- p +
      ggtitle(paste0("Posterior predictive check for model '", model_name, "' and statistic '", s, "'")) +
      theme(legend.position = "bottom")

    p_stats[[length(p_stats) + 1]] <- p
  }
  names(p_stats) <- stat

  if (!is.null(file)) {
    pdf(file, ...)
    for (p in p_stats) print(p)
    dev.off()
  } else {
    if (length(p_stats) == 1)
      return(p_stats[[1]])
    else
      return(p_stats)
  }
}
