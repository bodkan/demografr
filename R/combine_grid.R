#' Combine multiple individual grid simulation runs into one
#'
#' @param ... Either a list of objects of the class \code{demografr_sims} as produced
#'   by the function \code{simulate_abc}, or individual objects of this class given
#'   as standard function arguments, or paths to 'rds' files containing serializations of
#'   such \code{demografr_sims} objects.
#'
#' @return A combined object of the class \code{demografr_sims}
#'
#' @export
combine_grid <- function(...) {
  runs <- list(...)
  if (length(runs) == 1) runs <- runs[[1]]

  if (all(vapply(runs, is.character, FUN.VALUE = logical(1)))) {
    for (i in seq_along(runs)) {
      if (file.exists(runs[[i]]))
        runs[[i]] <- readRDS(runs[[i]])
      else
        stop("File ", runs[[i]], " does not exist", call. = FALSE)
    }
  }

  if (!all(vapply(runs, inherits, "data.frame", FUN.VALUE = logical(1))))
    stop("All provided runs must be data frames produced by the function `simulate_grid()`",
         call. = FALSE)

  if (length(unique(lapply(runs, colnames))) > 1)
    stop("Not all provided data frames have the same columns", call. = FALSE)

  # check that the first column is `run` and that the only list columns in each data frame
  # run are at the end (all being the format of the simulate_grid() function)
  rep_col_first <- vapply(runs, function(run_i) colnames(run_i)[1] == "rep", FUN.VALUE = logical(1))
  if (!all(rep_col_first))
    stop("The first column of each simulate_grid() data frame result must be `run`", call. = FALSE)

  list_cols <- lapply(runs, function(run)
    sapply(seq_len(ncol(run)), function(col_i) is.list(run[, col_i, drop = TRUE]))
  )
  list_cols_last <- list_cols %>%
    vapply(function(run) {
      list_pos <- rle(run)$values
      return(!any(list_pos[-length(list_pos)]) && list_pos[length(list_pos)])
    }, FUN.VALUE = logical(1))
  if (!all(list_cols_last))
    stop("Only the last columns of each simulate_grid() data frame can be list columns",
         call. = FALSE)

  # check that all individual demografr_sims objects are from the same model
  if (length(unique(lapply(runs, attr, "model"))) > 1)
    stop("Simulation runs must originate from the same grid setup but model functions differ", call. = FALSE)

  if (length(unique(lapply(runs, attr, "functions"))) > 1)
    stop("Simulation runs must originate from the same grid setup but summary functions differ", call. = FALSE)

  results <- do.call(rbind, runs)

  attr(results, "functions") <- attr(runs[[1]], "functions")
  attr(results, "model") <- attr(runs[[1]], "model")

  # readjust replicate numbers for each parameter grid combination
  # 1. first extract only those columns that belong to grid parameters
  grid_cols <- colnames(results)[c(FALSE, !list_cols[[1]][-1])]
  # 2. then replace replicate numbers of the original simulation runs
  results <- results %>% dplyr::group_by(dplyr::across(dplyr::all_of(grid_cols))) %>% dplyr::mutate(rep = 1:dplyr::n())

  results
}
