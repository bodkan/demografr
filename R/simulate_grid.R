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
#' @param sequence_length Amount of sequence to simulate using slendr (in base pairs).
#'   Ignored when custom simulations scripts are provided.
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param data A named list of data-generating functions. The names then represent all possible
#'   arguments of simulated summary statistic functions.
#' @param format In which format will the model generate results to be used for computing
#'   simulated summary statistics?
#' @param file If not \code{NULL}, a path where to save the data frame with simulated grid results.
#'   If this path is set, the results data frame is returned but invisibly.
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
#' @param strict Should parameter combinations leading to invalid slendr models be treated as
#'   an error? Default is \code{TRUE}. If set to \code{FALSE}, invalid simulations will be simply
#'   dropped, with an informative message.
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when parallelization is set up using \code{future::plan()} to make
#'   sure that the parallelized tree-sequence summary statistic functions have all of their
#'   packages available.
#' @param globals If a summary statistic function depends on object(s) in the R session which
#'   would not be available in separate parallel simulation processes, the names of such object(s)
#'   can be specified here, and they will be passed to each such separate process.
#'
#' @return If \code{file != NULL}, returns a data frame with simulated grid results. Otherwise
#'   does not return anything, saving an object to an .rds file instead.
#'
#' @return A data frame object with the results of parameter grid simulations, with values of
#'  each summary statistic stored in a list-column
#'
#' @example man/examples/grid_pipeline.R
#'
#' @export
simulate_grid <- function(
  model, grid, functions, replicates,
  sequence_length, recombination_rate, mutation_rate = 0,
  data = NULL, format = c("ts", "files"), file = NULL,
  engine = NULL, model_args = NULL, engine_args = NULL,
  strict = TRUE, packages = NULL, globals = NULL
) {
  format <- match.arg(format)

  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(colnames(grid)) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!arg_present(model) || !arg_present(grid) || !arg_present(functions))
    stop("A model to simulate from, parameter grid, and summary functions must be\n",
         "must be provided (check that the variables that you provided really do\n",
         "contain what you think)", call. = FALSE)

  if (!inherits(grid, "data.frame") && !nrow(grid))
    stop("The parameter `grid` must be a non-empty R data frame object", call. = FALSE)

  # if (!all(vapply(grid, is.numeric, logical(1))))
  #   stop("The parameter `grid` must contain only numerical columns", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  model_name <- as.character(substitute(model))
  summary_name <- as.character(substitute(functions))
  data_name <- as.character(substitute(data))

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  global_symbols <- c(
    names(model_args),
    names(engine_args)
  ) %>%
    unlist() %>%
    unique() %>%
    c(globals, model_name, summary_name, data_name, .) # TODO: Check if this is really needed in all instances
  if (is.null(globals)) global_symbols <- TRUE

  # prepare the grid data frame for storing the results
  grid <- lapply(
    seq_len(replicates),
    function(rep_i) dplyr::mutate(grid, rep = rep_i)
  ) %>%
    do.call(rbind, .)

  its <- seq_len(nrow(grid))
  p <- progressr::progressor(along = its)

  results <- future.apply::future_lapply(
    X = its,
    FUN = function(grid_i) {
      p()
      parameters <- as.list(grid[grid_i, -ncol(grid)])
      iter_result <- tryCatch(
        {
          wrap <- ifelse(strict, base::identity, utils::capture.output)
          wrap(res <- run_iteration(
            grid_i,
            model = model,
            params = parameters,
            functions = functions,
            sequence_length = sequence_length,
            recombination_rate = recombination_rate,
            mutation_rate = mutation_rate,
            data = data,
            format = format,
            engine = engine,
            model_args = model_args,
            engine_args = engine_args,
            model_name = model_name,
            attempts = NULL
          ))
          res$rep <- grid[grid_i, ]$rep
          res
        },
        error = function(cond) {
          if (strict)
            stop(conditionMessage(cond), call. = FALSE)
          else
            NA
            #list(error = TRUE, grid_i = grid_i)
        }
      )
      iter_result
    },
    future.seed = TRUE,
    future.globals = global_symbols,
    future.packages = c("slendr", packages)
  )

  invalid_runs <- vapply(results, function(run) all(is.na(run)), FUN.VALUE = logical(1))
  if (any(invalid_runs)) {
    msg <- sprintf(paste0(
      "Out of the total %i simulations, %d runs resulted in an error. The most\n",
      "likely explanation for this is that some parameter combinations lead to\n",
      "an invalid model (such as inconsistent order of split times).\n",
      "If you don't think this is right, run this function with `strict = TRUE`."),
      length(invalid_runs), sum(invalid_runs)
    )
    message(msg)
    results <- results[!invalid_runs]
  }

  if (all(invalid_runs))
    stop("All model simulation runs were invalid", call. = FALSE)

  # collect values of grid parameters from each simulation run back in a data frame format
  # (doing this instead of adding result columns to the original grid because simulations
  # could, in principle, be run in an arbitrary order due to parallelization)
  results_df <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% dplyr::as_tibble()
  # pull out the replicate number of each simulation run
  results_df$rep <- vapply(results, `[[`, "rep", FUN.VALUE = integer(1))

  # similarly, convert computed simulation summary statistics into a matrix
  summary_names <- if (is.call(functions)) names(as.list(functions)[-1]) else names(functions)
  for (stat in summary_names) {
    results_df[[stat]] <- lapply(results, `[[`, "simulated") %>% lapply(`[[`, stat)
  }

  results_df <- dplyr::select(results_df, rep, dplyr::everything())

  attr(results_df, "components") <- list(
    functions = functions,
    model = model
  )

  opts <- list(
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    mutation_rate = mutation_rate,
    engine = engine,
    model_args = model_args,
    engine_args = engine_args,
    packages = packages
  )
  attr(results_df, "options") <- opts

  if (is.null(file))
    return(results_df)
  else {
    saveRDS(results_df, file)
    return(invisible(results_df))
  }
}
