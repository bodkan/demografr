#' Run genetic algorithm (GA) inference of the parameters of the given model
#'
#' Runs a simulation-based GA routine of the given model, using the degree of
#' a match between observed and simulated data as a measure of fitness.
#'
#' This function internally uses the function\code{\link[ga]{ga}} from
#' the R package GA. All function arguments except to \code{data} are passed to the
#' \code{\link[abc]{abc}} function, appropriately unpacking the prior sample matrix,
#' and binding together matrices with observed statistics and simulated statistics
#' in the format required by the inference function.
#'
#' This function exists to avoid the need to manually track parameter matrices
#' and summary statistics as inputs to the \code{\link[abc]{abc}} function but acts
#' entirely transparently. A such, all implementation details can be found in the abc
#' vignette and the manpage which you can access by typing \code{?abc::abc}.
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
#' @param ... Additional arguments passed on the \code{ga} function from
#'   the GA package
#'
#' @export
run_ga <- function(
  model, functions, observed, bounds, iterations,
  sequence_length, recombination_rate, mutation_rate = 0,
  engine = NULL, model_args = NULL, engine_args = NULL,
  ...
) {
  # make sure warnings are reported immediately before simulations are even started
  warning_length <- (length(bounds) + length(model_args)) * 50
  if (warning_length < 1000) warning_length <- 1000
  opts <- options(warn = 1, warning.length = warning_length)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!arg_present(model) || !arg_present(bounds) || !arg_present(functions) || !arg_present(observed) ||
      !arg_present(iterations))
    stop("A model to simulate from, parameter bounds, summary functions, observed\n",
         "statistics, and the number of iterations must be provided (check\n",
         "that the variables that you provided really do contain what you think)", call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  # if bounds are given as "templated" formulas, expand them first (and order
  # them in the same way arguments of the model function are ordered)
  bounds <- expand_formulas(bounds, model, model_args)
  parsed_bounds <- parse_bounds(bounds, model, model_args)
  # extract (now properly ordered) boundary limits required by the ga() routine
  lower <- parsed_bounds$lower
  upper <- parsed_bounds$upper

  result <- ga(
    type = "real-valued",
    fitness = compute_fitness,
    model = model,
    functions = functions,
    observed = observed,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    mutation_rate = mutation_rate,
    engine = engine,
    model_args = model_args,
    engine_args = engine_args,
    maxiter = iterations,
    lower = lower,
    upper = upper,
    ...
  )

  # the result of the abc analysis is a standard object produced by the R package abc,
  # but additional annotation is added so that demografr's own functions can be used
  # for additional analyses and plotting...
  components <- list(
    model = model,
    functions = functions,
    observed = observed,
    bounds = bounds,
    iterations = iterations
  )

  attr(result, "components") <- components

  # ... which is why the result is annotated with another class
  # class(result) <- c("demografr_ga.", class(result))

  result
}
