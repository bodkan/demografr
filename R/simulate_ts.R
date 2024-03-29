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
#' @param random_seed Random seed to be used for simulation and (potentially) adding of mutations
#'   to a simulated tree sequence
#'
#' @return A tree-sequence object of the class \code{slendr_ts}
#'
#' @export
simulate_ts <- function(
  model, parameters,
  sequence_length = 1e6, recombination_rate = 0, mutation_rate = 0,
  attempts = 1000, engine = NULL, model_args = NULL, engine_args = NULL,
  random_seed = NULL
) {
  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(parameters) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running simulations
  # in parallel
  if (!check_arg(model) || !check_arg(parameters) || !check_arg(sequence_length) || !check_arg(recombination_rate) || !length(parameters))
    stop(paste0("A model generating function, parameters and sequence information must be provided"), call. = FALSE)

  if (inherits(parameters, "formula"))
    parameters <- list(parameters)

  if (!contains_priors(parameters) &&
      !(is.numeric(unlist(parameters)) &&
        !is.null(names(parameters)) &&
        all(names(parameters) != "")))
    stop("Parameters must be given as:\n  - a list of formula objects, or\n",
         "  - a named list of numerical values", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  init_env(quiet = TRUE)

  if (contains_priors(parameters)) {
    parameters <- expand_formulas(parameters, model, model_args) #%>% strip_prior_environments()

    # it's not possible to perform full validation here but at least try that sampling
    # from the prior definitions works as it should
    invisible(lapply(parameters, sample_prior))
  }

  ts <- run_simulation(model, parameters, sequence_length, recombination_rate, mutation_rate,
                       engine = engine, model_args = model_args,
                       engine_args = engine_args,
                       attempts = attempts, model_name = substitute(model),
                       random_seed = random_seed)$ts

  ts
}
