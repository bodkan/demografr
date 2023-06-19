#' Simulate values of summary statistics across a parameter grid
#'
#' For a given data frame (each column a parameter of a slendr model function) simulates
#' values of given population genetic statistics
#'
#' @param model A slendr model generating function
#' @param grid A data frame object containing parameter grid such as one produced by
#'   \code{tidyr::expand_grid} or \code{base::expand.grid}
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param replicates How many simulation replicates to run for each parameter combination?
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param engine Which simulation engine to use? Values "msprime" and "SLiM" will use the
#'   built-in slendr simulation back ends.
#' @param model_args Optional (non-prior) arguments for the scaffold model generating function
#' @param engine_args Optional arguments for the slendr simulation back ends
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when \code{future::plan("multisession", ...)} was initialized.
#'
#' @export
simulate_grid <- function(
  model, grid, functions, replicates,
  sequence_length, recombination_rate, mutation_rate = 0,
  split = FALSE,
  model_args = NULL, engine_args = NULL, packages = NULL,
  engine = c("msprime", "SLiM")
) {
  # make sure warnings are reported immediately before simulations are even started
  opts <- options(warn = 1)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!check_arg(model) || !check_arg(grid) || !check_arg(functions))
    stop("A model generating function, parameter grid, and summary functions must be\n",
         "must be provided (check that the variables that you provided really do\n",
         "contain what you think", call. = FALSE)

  if (!inherits(grid, "data.frame") && !nrow(grid))
    stop("The parameter `grid` must be a non-empty R data frame object", call. = FALSE)

  if (!all(vapply(grid, is.numeric, logical(1))))
    stop("The parameter `grid` must contain only numerical columns", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  engine <- match.arg(engine)

  # TODO: make sure the model is not serialized if it's to be run with msprime

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  globals <- c(
    names(model_args),
    names(engine_args)
  ) %>%
    unlist()
  if (is.null(globals)) globals <- TRUE

  results <- lapply(seq_len(replicates), function(rep_i) {
    iter_results <- future.apply::future_lapply(
      X = seq_len(nrow(grid)),
      FUN = function(grid_i) {
        run_iteration(
          grid_i,
          model = model,
          params = as.list(grid[grid_i, ]),
          functions = functions,
          sequence_length = sequence_length,
          recombination_rate = recombination_rate,
          mutation_rate = mutation_rate,
          engine = engine,
          model_args = model_args,
          engine_args = engine_args,
          model_name = as.character(substitute(model)),
          attempts = 1
        )
      },
      future.seed = TRUE,
      future.globals = globals,
      future.packages = c("slendr", "dplyr", "tidyr", packages)
    )
    # collect values of grid parameters from each simulation run back in a data frame format
    # (doing this instead of adding result columns to the original grid because simulations
    # could, in principle, be run in an arbitrary order due to parallelization)
    iter_results_df <- lapply(iter_results, `[[`, "parameters") %>% do.call(rbind, .) %>% dplyr::as_tibble()

    # similarly, convert computed simulation summary statistics into a matrix
    for (stat in names(functions)) {
      iter_results_df[[stat]] <- lapply(iter_results, `[[`, "simulated") %>% lapply(`[[`, stat)
    }

    iter_results_df$rep <- rep_i

    iter_results_df %>% dplyr::select(rep, dplyr::everything())
  }) %>%
    do.call(rbind, .)

  results
}
