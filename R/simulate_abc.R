#' Simulate data for ABC inference using specified priors
#'
#' This is a core function for ABC inference using demografr. It generates simulation
#' replicates and computes summary statistic for the next step of an inference procedure,
#' which is the ABC estimation itself.
#'
#' @param model A compiled slendr model object or a model generating function
#' @param priors A list of prior distributions to use for sampling of model parameters
#' @param functions A named list of summary statistic functions to apply on simulated
#'   tree sequences
#' @param observed A named list of observed summary statistics
#' @param sequence_length Amount of sequence to simulate using slendr (in numbers of basepairs)
#' @param recombination_rate Recombination rate to use for the simulation
#' @param mutation_rate Mutation rate to use for the simulation
#' @param engine Which simulation engine to use? Values "msprime" and "SLiM" will use the
#'   built-in slendr simulation back ends.
#' @param model_args Optional (non-prior) arguments for the scaffold model generating function
#' @param engine_args Optional arguments for the slendr simulation back ends
#' @param packages A character vector with package names used by user-defined summary statistic
#'   functions. Only relevant when \code{future::plan("multisession", ...)} was initialized.
#' @param debug Only perform a single ABC simulation run, skipping parallelization
#' @param ... Additional parameters used in the model generating function \code{model} (ignored
#'   if a standard slendr model is used as a scaffold model)
#'
#' @export
simulate_abc <- function(
  model, priors, functions, observed,
  iterations, sequence_length, recombination_rate, mutation_rate = 0,
  samples = NULL, model_args = NULL, engine_args = NULL, packages = NULL,
  engine = c("msprime", "SLiM", "custom"), debug = FALSE, ...
) {
  # make sure warnings are reported immediately before simulations are even started
  opts <- options(warn = 1)
  on.exit(options(opts))

  # check the presence of all arguments to avoid cryptic errors when running
  # simulations in parallel
  if (!check_arg(model) || !check_arg(priors) || !check_arg(functions) || !check_arg(observed) ||
      !check_arg(iterations))
    stop(paste0("A scaffold model, priors, summary functions, observed statistics,\n",
              "the number of iterations, and sequence information must be provided."), call. = FALSE)

  if (mutation_rate < 0)
    stop("Mutation rate must be a non-negative number", call. = FALSE)

  engine <- match.arg(engine)

  # validate the ABC setup
  capture.output(validate_abc(
    model, priors, functions, observed,
    sequence_length = sequence_length, recombination_rate = recombination_rate,
    mutation_rate = mutation_rate, model_args = model_args
  ))

  if (!is.function(model)) {
    if (engine == "msprime" && !is.null(model$path)) {
      warning("Model is serialized to disk which is unnecessary and inefficient\n",
              "for msprime ABC simulations. The engine will skip the serialized\n",
              "model files and use the in-memory representation instead.",
              call. = FALSE)
      model$path <- NULL
    }
    if (engine == "SLiM" && is.null(model$path))
      stop("Non-serialized slendr model cannot be used as a scaffold for SLiM ABC\n",
          "simulations. Make sure your model is not compiled with `serialized = FALSE`.",
          call. = FALSE)
  }

  # collect all required global objects, in case the ABC simulations will run in
  # multiple parallel sessions
  globals <- c(
    lapply(priors, function(p) as.character(as.list(as.list(p)[[3]])[[1]])),
    names(model_args),
    names(engine_args)
  ) %>%
    unlist()

  if (!debug) {
    results <- future.apply::future_lapply(
      X = seq_len(iterations),
      FUN = run_iteration,
      model = model,
      priors = priors,
      functions = functions,
      sequence_length = sequence_length,
      recombination_rate = recombination_rate,
      mutation_rate = mutation_rate,
      engine = engine,
      samples = samples,
      model_args = model_args,
      engine_args = engine_args,
      future.seed = TRUE,
      future.globals = globals,
      future.packages = c("slendr", packages)
    )
  } else {
    results <- list(
      run_iteration(
        it = 1, model = model, priors = priors, functions = functions,
        engine = engine, samples = samples, engine_args = engine_args, model_args = model_args,
        sequence_length = sequence_length, recombination_rate = recombination_rate, mutation_rate = mutation_rate
      )
    )
  }

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
  }) %>% do.call(cbind, .)

  result <- list(
    parameters = parameters,
    simulated = simulated,
    observed = observed,
    functions = functions,
    priors = priors,
    model = model
  )
  class(result) <- "demografr_sims"

  result
}
