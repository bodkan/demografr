#' Simulate a single tree sequence from the given ABC setup
#'
#' Simulates a tree sequence object from a model with parameters sampled from priors
#'
#' This function is useful to generate a small tree sequence to be used when developing
#' summary statistic functions for ABC inference using demografr.
#'
#' @param model A compiled slendr model object
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param engine Which simulation engine to use? Values "msprime" and "SLiM" will use the
#'   built-in slendr simulation back ends.
#' @param model_args Optional (non-prior) arguments for the scaffold model generating function
#' @param engine_args Optional arguments for the slendr simulation back ends
#'
#' @export
simulate_ts <- function(
  model, priors,
  sequence_length = 1e6, recombination_rate = 0, mutation_rate = 0,
  samples = NULL, engine = NULL, model_args = NULL, engine_args = NULL
) {
  # check the presence of all arguments to avoid cryptic errors when running simulations
  # in parallel
  if (!check_arg(model) || !check_arg(priors) || !check_arg(sequence_length) || !check_arg(recombination_rate))
    stop(paste0("A scaffold model, priors and sequence information must be provided."), call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  init_env(quiet = TRUE)

  ts <- run_simulation(model, priors, sequence_length, recombination_rate, mutation_rate,
                       engine = "msprime", samples = NULL,
                       model_args = model_args, engine_args = engine_args, attempts = 1000,
                       model_name = substitute(model))$ts
  ts
}
