#' Simulate a single tree sequence from the given inference setup
#'
#' Simulates a tree sequence object from a model with parameters as specified either
#' by sampling from priors or by a given list of fixed parameter values
#'
#' This function is useful to generate a small tree sequence to be used when developing
#' summary statistic functions inference using demografr.
#'
#' @param model Either a slendr model generating function (in which case \code{engine} must
#'   be either "msprime" or "slim", i.e. one of the two of slendr's simulation back ends),
#'   or a path to a custom user-defined SLiM or msprime script (in which case \code{engine}
#'   must be "custom").
#' @param parameters A list of prior distributions to use for sampling of model parameters
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
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
#' @return A tree-sequence object of the class \code{slendr_ts}
#'
#' @export
simulate_ts <- function(
  model, parameters,
  sequence_length = 1e6, recombination_rate = 0, mutation_rate = 0,
  engine = NULL, model_args = NULL, engine_args = NULL
) {
  # check the presence of all arguments to avoid cryptic errors when running simulations
  # in parallel
  if (!check_arg(model) || !check_arg(parameters) || !check_arg(sequence_length) || !check_arg(recombination_rate))
    stop(paste0("A scaffold model, priors and sequence information must be provided."), call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  init_env(quiet = TRUE)

  ts <- run_simulation(model, parameters, sequence_length, recombination_rate, mutation_rate,
                       engine = engine, model_args = model_args,
                       engine_args = engine_args,
                       attempts = 1000, model_name = substitute(model))$ts

  ts
}
