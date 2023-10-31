bind_observed <- function(observed_list) {
  bound_stats <- lapply(names(observed_list), function(stat) {
    # convert observed statistics to a matrix, either from a normal data frame
    # result (with each statistic named), or from a simple vector
    x <- observed_list[[stat]]

    if (is.data.frame(x)) {
      # find the column with the value of a statistic `stat`
      # TODO: the last column will be numeric
      # value_col <- ncol(x)
      value_col <- sapply(names(x), function(i) is.numeric(x[[i]]))
      values <- matrix(x[, value_col, drop = TRUE], nrow = 1)
      names <- x[, !value_col, drop = FALSE] %>%
        apply(MARGIN = 1, FUN = function(row) paste(c(stat, row), collapse = "_"))
    } else {
      values <- matrix(x, nrow = 1)
      names <- paste0(stat, "_", seq_along(x))
    }
    colnames(values) <- names
    values
  }) %>% do.call(cbind, .)

  bound_stats
}
