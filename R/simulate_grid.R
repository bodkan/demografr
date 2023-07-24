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
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when parallelization is set up using \code{future::plan()} to make
#'   sure that the parallelized tree-sequence summary statistic functions have all of their
#'   packages available.
#' @param file If not \code{NULL}, a path where to save the data frame with simulated grid results
#' @param engine Which simulation engine to use? Values "msprime" and "slim" will use one of
#'   the built-in slendr simulation back ends. Which engine will be used is determined
#'   by the nature of the \code{model}. If \code{engine = NULL}, then spatial slendr models will
#'   by default use the "slim" back end, non-spatial models will use the "msprime" back end, and
#'   custom user-defined model scripts will use the "custom" engine. Setting this argument
#'   explicitly will change the back ends (where appropriate). Setting this argument for custom
#'   simulation script has no effect.
#' @param model_args Optional (non-prior) arguments for the slendr model generating function.
#'   Setting this argument for custom simulation script has no effect.
#' @param engine_args Optional arguments for the slendr simulation back end. Setting this
#'   argument for custom simulation script has no effect.
#'
#' @return If \code{file != NULL}, returns a data frame with simulated grid results. Otherwise
#'   does not return anything, saving an object to an .rds file instead.
#'
#' @return A data frame object with the results of parameter grid simulations, with values of
#'  each summary statistic stored in a list-column
#'
#' @export
simulate_grid <- function(
  model, grid, functions, replicates,
  sequence_length, recombination_rate, mutation_rate = 0,
  packages = NULL, file = NULL,
  engine = NULL, model_args = NULL, engine_args = NULL
) {
  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(colnames(grid)) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!check_arg(model) || !check_arg(grid) || !check_arg(functions))
    stop("A model generating function, parameter grid, and summary functions must be\n",
         "must be provided (check that the variables that you provided really do\n",
         "contain what you think)", call. = FALSE)

  if (!inherits(grid, "data.frame") && !nrow(grid))
    stop("The parameter `grid` must be a non-empty R data frame object", call. = FALSE)

  if (!all(vapply(grid, is.numeric, logical(1))))
    stop("The parameter `grid` must contain only numerical columns", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  globals <- c(
    names(model_args),
    names(engine_args)
  ) %>%
    unlist()
  if (is.null(globals)) globals <- TRUE

  # prepare the grid data frame for storing the results
  grid <- lapply(
    seq_len(replicates),
    function(rep_i) dplyr::mutate(grid, rep = rep_i)
  ) %>%
    do.call(rbind, .)

  results <- future.apply::future_lapply(
    X = seq_len(nrow(grid)),
    FUN = function(grid_i) {
      parameters <- as.list(dplyr::select(grid[grid_i, ], -rep))
      iter_result <- run_iteration(
        grid_i,
        model = model,
        params = parameters,
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
      iter_result$rep <- grid[grid_i, ]$rep
      iter_result
    },
    future.seed = TRUE,
    future.globals = globals,
    future.packages = c("slendr", packages)
  )

  # collect values of grid parameters from each simulation run back in a data frame format
  # (doing this instead of adding result columns to the original grid because simulations
  # could, in principle, be run in an arbitrary order due to parallelization)
  results_df <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% dplyr::as_tibble()
  # pull out the replicate number of each simulation run
  results_df$rep <- vapply(results, `[[`, "rep", FUN.VALUE = integer(1))

  # similarly, convert computed simulation summary statistics into a matrix
  for (stat in names(functions)) {
    results_df[[stat]] <- lapply(results, `[[`, "simulated") %>% lapply(`[[`, stat)
  }

  results_df <- dplyr::select(results_df, rep, dplyr::everything())

  attr(results_df, "components") <- list(
    functions = functions,
    model = model
  )

  if (is.null(file))
    return(results_df)
  else
    saveRDS(results_df, file)
}
