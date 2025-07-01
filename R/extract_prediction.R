#' Unnest the predicted values of a given statistic from the simulated data
#'
#' @param data Data with posterior simulations performed via \code{predict.demografr_abc_sims}
#' @param stat Which tree-sequence summary statistic to extract? If \code{NULL}, all summary
#'   statistics will be returned.
#'
#' @return Data frame object containing columns with values of parameters sampled from the
#'   posterior, then a column "summary" containing a name of a summary statistic computed
#'   for each combination of parameters, column "stat" containing the exact summary
#'   statistic computed (useful in case multiple values of the summary statistic computed
#'   by a tree-sequence function), and a column "value" containing its value.
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
#' }
#'
#' @export
extract_prediction <- function(data, stat) {
  all_stats <- names(attr(data, "components")$observed)
  if (!all(stat %in% all_stats))
    stop("Unknown statistics '", stat, "' specified", call. = FALSE)

  result <- lapply(stat, function(s) unnest_stat(data, s))
  names(result) <- stat

  result
}

unnest_stat <- function(data, stat) {
  # get indices of list-columns (which contain results of tree-sequence summary statistics)
  list_cols <- vapply(seq_len(ncol(data)), function(col_i) is.list(data[, col_i, drop = TRUE]), FUN.VALUE = logical(1))

  # extract just the list-column data frame with the summary statistic of interest
  list_stat_df <- dplyr::select(data, dplyr::all_of(stat)) %>% tidyr::unnest(dplyr::all_of(stat))

  # in that data frame, identify the numeric value column
  value_col <- vapply(list_stat_df, is.numeric, FUN.VALUE = logical(1))

  if (all(value_col)) {
    stat_df <- list_stat_df
    stat_df$summary <- stat
    stat_df <- dplyr::select(stat_df, summary, dplyr::everything())
  } else {
    # all the remaining columns are user-defined identifiers of the statistic (population
    # names, SFS bins, etc.) -- collapse them all into a single name of the statistic,
    # prepending the statistic name
    stat_col <- paste0(stat, "_", apply(list_stat_df[!value_col], 1, paste, collapse = "_"))

    # create a new data frame with one column containing the collapsed single-name statistic,
    # the second column containing the numerical value
    stat_df <- dplyr::tibble(
      summary = stat,
      stat = stat_col,
      value = list_stat_df[, value_col, drop = TRUE]
    )
  }

  # extract just the parameter columns into their own data frame (must happen *after* unnesting
  # to have the same number of rows because nested list-column data frames are likely to have
  # more rows than just one)
  grid_df_colnames <- colnames(data)[!list_cols]
  grid_df <- tidyr::unnest(data, dplyr::all_of(stat))[, grid_df_colnames]

  # create a new data frame, with the parameter grid matrix and the statistic itself
  result <- dplyr::bind_cols(grid_df, stat_df)

  attr(result, "components") <- attr(data, "components")

  result
}
