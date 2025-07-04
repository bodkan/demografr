#' Combine multiple individual ABC simulation runs into one
#'
#' @param ... Either a list of objects of the class \code{demografr_abc_sims} as produced
#'   by the function \code{simulate_abc}, or individual objects of this class given
#'   as standard function arguments, or paths to 'rds' files containing serializations of
#'   such \code{demografr_abc_sims} objects.
#'
#' @return A combined object of the class \code{demografr_abc_sims}
#'
#' @examples
#' \dontshow{slendr::check_dependencies(python = TRUE, quit = TRUE) # dependencies must be present
#' }
#' library(slendr)
#' init_env(quiet = TRUE)
#'
#' # simulate three runs of a slendr model across a parameter grid
#' model <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4) {
#'   p1 <- population("p1", time = 1, N = 1000)
#'   p2 <- population("p2", time = 2000, N = 3000, parent = p1)
#'
#'   model <- compile_model(
#'     populations = list(p1, p2),
#'     generation_time = 1,
#'     simulation_length = 10000, serialize = FALSE
#'   )
#'
#'   return(model)
#' }
#'
#' grid <- expand.grid(Ne_p1 = c(10, 100), Ne_p2 = c(10, 100))
#'
#' compute_diversity <- function(ts) {
#'   samples <- list(ts_samples(ts)$name)
#'   ts_diversity(ts, sample_sets = samples, mode = "branch")
#' }
#' functions <- list(diversity = compute_diversity)
#'
#' nreps <- 1
#' run1 <- simulate_grid(model, grid, functions, replicates = nreps,
#'                       sequence_length = 10000, recombination_rate = 0)
#' run2 <- simulate_grid(model, grid, functions, replicates = nreps,
#'                       sequence_length = 10000, recombination_rate = 0)
#'
#' # combine both simulation runs
#' combine_data(run1, run2)
#'
#' @export
combine_data <- function(...) {
    UseMethod("combine_data")
}

#' @export
#' @keywords internal
combine_data.demografr_abc_sims <- function(...) {
  runs <- list(...)

  if (!all(vapply(runs, inherits, "demografr_abc_sims", FUN.VALUE = logical(1))))
    stop("All provided runs must be a product of the function `simulate_abc()`", call. = FALSE)

  # check that all individual demografr_abc_sims objects are from the same model
  if (length(unique(lapply(runs, `[[`, "model"))) > 1)
    stop("Simulation runs must originate from the same ABC setup but model functions differ", call. = FALSE)

  if (length(unique(lapply(runs, function(run) priors_to_strings(run$priors)))) > 1)
    stop("Simulation runs must originate from the same ABC setup but priors differ", call. = FALSE)

  if (length(unique(lapply(runs, `[[`, "functions"))) > 1)
    stop("Simulation runs must originate from the same ABC setup but summary functions differ", call. = FALSE)

  if (length(unique(lapply(runs, `[[`, "observed"))) > 1)
    stop("Simulation runs must originate from the same ABC setup but observed statistics differ", call. = FALSE)

  # bind individual matrices of parameters and simulated summary statistics
  parameters <- do.call(rbind, lapply(runs, `[[`, "parameters"))
  simulated <- do.call(rbind, lapply(runs, `[[`, "simulated"))

  result <- list(
    parameters = parameters,
    simulated = simulated,
    observed = runs[[1]]$observed,
    functions = runs[[1]]$functions,
    priors = runs[[1]]$priors,
    model = runs[[1]]$model,
    model_name = runs[[1]]$model_name
  )

  attr(result, "options") <- attr(runs[[1]], "options")

  class(result) <- "demografr_abc_sims"

  result
}

#' @export
#' @keywords internal
combine_data.data.frame <- function(...) {
  runs <- list(...)

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

  # check that all individual demografr_abc_sims objects are from the same model
  if (length(unique(lapply(runs, function(run) attr(run, "components")$model))) > 1)
    stop("Simulation runs must originate from the same grid setup but model functions differ", call. = FALSE)

  if (length(unique(lapply(runs, function(run) attr(run, "components")$functions))) > 1)
    stop("Simulation runs must originate from the same grid setup but summary functions differ", call. = FALSE)

  results <- do.call(rbind, runs)

  # readjust replicate numbers for each parameter grid combination
  # 1. first extract only those columns that belong to grid parameters
  grid_cols <- colnames(results)[c(FALSE, !list_cols[[1]][-1])]
  # 2. then replace replicate numbers of the original simulation runs
  results <- results %>% dplyr::group_by(dplyr::across(dplyr::all_of(grid_cols))) %>% dplyr::mutate(rep = 1:dplyr::n()) %>% dplyr::ungroup()

  attr(results, "components") <- attr(runs[[1]], "components")
  attr(results, "options") <- attr(runs[[1]], "options")

  results
}

#' Combine multiple individual grid simulation runs into one
#'
#' @param ... Either a list of objects of the class \code{demografr_abc_sims} as produced
#'   by the function \code{simulate_abc}, or individual objects of this class given
#'   as standard function arguments, or paths to 'rds' files containing serializations of
#'   such \code{demografr_abc_sims} objects.
#'
#' @return A combined object of the class \code{demografr_abc_sims}
#'
#' @export
#' @keywords internal
combine_data.character <- function(...) {
  runs <- list(...)

  if (all(vapply(runs, is.character, FUN.VALUE = logical(1)))) {
    for (i in seq_along(runs)) {
      if (file.exists(runs[[i]]))
        runs[[i]] <- readRDS(runs[[i]])
      else
        stop("File ", runs[[i]], " does not exist", call. = FALSE)
    }
  }

  do.call(combine_data, runs)
}

#' @export
#' @keywords internal
combine_data.list <- function(...) {
  runs <- list(...)[[1]]

  if (inherits(runs[[1]], "demografr_abc_sims"))
    do.call(combine_data.demografr_abc_sims, runs)
  else if (inherits(runs[[1]], "data.frame"))
    do.call(combine_data.data.frame, runs)
  else if (is.character(runs[[1]]))
    do.call(combine_data.character, runs)
  else
    stop("Invalid data type of elements in the list of items to combine together", call. = FALSE)
}

# Formula objects we use for priors are stored alongside their environments, which
# effectively means that each run has a 'unique' prior, so the unique() calls above
# don't do what we need. This helper function converts each prior to their raw
# character representation purely for comparing the code of each prior formula.
priors_to_strings <- function(priors) {
  lapply(priors, function(p) paste(deparse(p[[2]]), "~", deparse(p[[3]]), collapse = " "))
}
