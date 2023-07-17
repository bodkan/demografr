#' Simulate data for ABC inference using specified priors
#'
#' This is a core function for ABC inference using demografr. It generates simulation
#' replicates and computes summary statistic for the next step of an inference procedure,
#' which is the ABC estimation itself.
#'
#' @param model Either a slendr model generating function (in which case \code{engine} must
#'   be either "msprime" or "slim", i.e. one of the two of slendr's simulation back ends),
#'   or a path to a custom user-defined SLiM or msprime script (in which case \code{engine}
#'   must be "custom").
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#' @param iterations How many simulation replicates to run?
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param file If not \code{NULL}, a path where to save the data frame with simulated grid results
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when parallelization is set up using \code{future::plan()} to make
#'   sure that the parallelized tree-sequence summary statistic functions have all of their
#'   packages available.
#' @param attempts Maximum number of attempts to generate prior values for a valid demographic
#'   model (default is 1000)
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
#' @return A list object of the class \code{demografr_abc_sims} containing the results
#'   of ABC simulations, sampled parameters, priors, and tree-sequence summary statistics
#'
#' @export
simulate_abc <- function(
  model, priors, functions, observed, iterations,
  sequence_length, recombination_rate, mutation_rate = 0,
  file = NULL, packages = NULL, attempts = 1000,
  engine = NULL, model_args = NULL, engine_args = NULL
) {
  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(priors) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!check_arg(model) || !check_arg(priors) || !check_arg(functions) || !check_arg(observed) ||
      !check_arg(iterations) || !length(priors))
    stop("A model generating function, priors, summary functions, observed\n",
         "statistics, and the number of iterations must be provided (check\n",
         "that the variables that you provided really do contain what you think)", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  # validate the ABC setup
  utils::capture.output(validate_abc(
    model, priors, functions, observed,
    sequence_length = sequence_length, recombination_rate = recombination_rate,
    mutation_rate = mutation_rate,
    engine = engine, model_args = model_args,
    engine_args = engine_args
  ))

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  globals <- c(
    lapply(priors, function(p) as.character(as.list(as.list(p)[[3]])[[1]])),
    names(model_args),
    names(engine_args)
  ) %>%
    unlist()

  results <- future.apply::future_lapply(
    X = seq_len(iterations),
    FUN = run_iteration,
    model = model,
    params = priors,
    functions = functions,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    mutation_rate = mutation_rate,
    engine = engine,
    model_args = model_args,
    engine_args = engine_args,
    model_name = as.character(substitute(model)),
    attempts = attempts,
    future.seed = TRUE,
    future.globals = globals,
    future.packages = c("slendr", packages)
  )

  # convert values of sampled priors (one element of a list for each replicate) into
  # a normal R matrix object
  parameters <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% as.matrix

  # similarly, convert computed simulation summary statistics into a matrix
  simulated <- lapply(names(functions), function(stat) do.call(
    rbind,
    {
      lapply(results, `[[`, "simulated") %>% lapply(function(it) {
        x <- it[[stat]]
        # convert simulated statistics to a matrix, either from a normal data frame
        # result (with each statistic named), or from a simple vector
        if (is.data.frame(x)) {
          # find the column with the value of a statistic `stat`
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
      })
    }
  )) %>% do.call(cbind, .)

  # and to the same for the observed statistics as well
  observed <- lapply(names(functions), function(stat) {
    # convert observed statistics to a matrix, either from a normal data frame
    # result (with each statistic named), or from a simple vector
    x <- observed[[stat]]

    if (is.data.frame(x)) {
      # find the column with the value of a statistic `stat`
      # TODO: the last column will be numeric
      # value_col <- sapply(names(x), function(i) is.numeric(x[[i]]))
      value_col <- ncol(x)
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

  result <- list(
    parameters = parameters,
    simulated = simulated,
    observed = observed,
    functions = functions,
    priors = priors,
    model = model
  )

  class(result) <- "demografr_abc_sims"

  if (is.null(file))
    return(result)
  else
    saveRDS(result)
}
