#' Plot posterior distribution(s)
#' @import ggplot2
#' @export
plot_posterior <- function(abc, param = NULL, prefix = NULL, posterior = c("adj", "unadj"),
                           summary = c("mode", "mean", "median"),
                           geom = ggplot2::geom_density, facets = TRUE, xlim = NULL, ...) {
  df <- extract_posterior(abc, posterior)

  if (!is.null(prefix)) {
    prefix_match <- grepl(prefix, df$param)
    df <- df[prefix_match, ]
    if (!nrow(df))
      stop("No parameter matchin the given prefix", call. = FALSE)
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
    geom(aes(value), alpha = 0.5, ...) +
    # geom_vline(data = summary_df, aes(xintercept = value), linetype = 2) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
    # guides(color = legend, fill = legend) +
    scale_fill_discrete(drop = FALSE) +
    scale_color_discrete(drop = FALSE) +
    theme_minimal() +
    theme(strip.text.x = element_text(face = "bold", size = 13)) +
    coord_cartesian(xlim = xlim)

  if (facets) {
    # scales <- if (length(unique(as.character(df$type))) == 1) "fixed" else "free"
    p <- p + facet_wrap(~ param) #, scales = scales)
  }

  p
}

extract_posterior_summary <- function(abc, summary = c("mode", "mean", "median")) {
  summary <- match.arg(summary) %>% tools::toTitleCase()
  summary_wide <- quiet(summary(abc))[sprintf("Weighted %s:", summary), ]
  data.frame(
    param = names(summary_wide),
    value = as.vector(summary_wide),
    stringsAsFactors = FALSE
  )
}