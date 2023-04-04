# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it,
                          model, priors, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          engine, samples, model_args, engine_args,
                          model_name, attempts) {
  init_env(quiet = TRUE)

  sim_result <- run_simulation(
    model = model, priors = priors, sequence_length = sequence_length,
    recombination_rate = recombination_rate, mutation_rate = mutation_rate,
    model_name = model_name, engine = engine, samples = samples,
    model_args = model_args, engine_args = engine_args,
    attempts = attempts
  )
  ts <- sim_result$ts
  prior_values <- sim_result$prior_values

  # collect data for a downstream ABC inference:
  #   1. compute summary statistics using user-defined tree-sequence functions
  simulated_stats <- lapply(functions, function(f) f(ts))
  #   2. collect all sampled prior values into a single parameter matrix
  prior_values <- collect_prior_matrix(prior_values)

  list(
    parameters = prior_values,
    simulated = simulated_stats
  )
}

# Run a single simulation replicate from a model with parameters modified by the
# prior distribution
run_simulation <- function(model, priors, sequence_length, recombination_rate, mutation_rate,
                           engine, samples = NULL, model_args, engine_args,
                           model_name, attempts) {
  # only a well-defined slendr errors are allowed to be ignored during ABC simulations
  # (i.e. split time of a daughter population sampled from a prior at an older time than
  # its parent, etc.) -- such errors will simply lead to resampling, but all other errors
  # are considered real errors on the part of the user and will be reported as such
  errors <- c(
    # invalid split order implied by sampled split times
    "The model implies forward time direction but the specified split\ntime \\(\\d+\\) is lower than the parent's \\(\\d+\\)",
    # daughter population splitting *at the same time* as its parent (i.e. 1 vs 1.3 after rounding)
    "Population can be only created after its parent is already present in the simulation",
    # invalid gene-flow window
    "Specified times are not consistent with the assumed direction of\ntime \\(gene flow .* -> .* in the time window \\d+-\\d+\\)",
    # gene-flow participants not existing
    "Both .* and .* must be present within the gene-flow window \\d+-\\d+",
    # population is created right at the moment the simulation is about to be finished
    "msprime._msprime.InputError: Input error in initialise: Attempt to sample a lineage from an inactive population"
  )

  n_tries <- 0
  repeat {
    if (n_tries == attempts)
      stop("\n\nGenerating a valid slendr model using the provided generation function\n",
           "and priors failed even after ", attempts, " repetitions. Please make sure\n",
           "that your model function can produce a valid model assuming the specified\n",
           "prior distributions.", call. = FALSE)

    n_tries <- n_tries + 1

    ts <- tryCatch(
      {
        # collect prior model function arguments
        prior_args <- generate_prior_args(priors)

        # generate a compiled slendr model from a provided function
        fun_args <- c(prior_args, model_args)
        slendr_model <- do.call(model, fun_args)

        # compose a list of required and optional arguments for msprime / SLiM engine
        engine_args <- list(
          model = slendr_model,
          sequence_length = sequence_length,
          recombination_rate = recombination_rate,
          samples = samples
        ) %>% c(., engine_args)

        # simulate a tree sequence
        do.call(engine, engine_args)
      },
      error = function(cond) {
        msg <- conditionMessage(cond)

        # check that the received error is one of the valid, potentially expected slendr errors
        if (any(vapply(errors, grepl, msg, FUN.VALUE = logical(1)))) {
          return(NULL)
        } else { # if an unexpected error ocurred, report it in full
          cat(" \u274C\n\n")
          # compose parameters for the complete model function call
          # (i.e. priors and non-prior arguments to the model generating function)
          fun_params <- paste(
            vapply(names(fun_args),
                  function(x) sprintf("%s = %s", x, ifelse(is.numeric(fun_args[[x]]),
                                                          fun_args[[x]],
                                                          sprintf("\"%s\"", fun_args[[x]]))),
                  FUN.VALUE = character(1)),
            collapse = ", "
          )
          stop("An unexpected error was raised while generating a slendr model\n",
              "using the provided slendr function.\n\nThe error message received was:\n",
              msg,
              "\n\nPerhaps re-running the model function with the sampled parameters will\n",
              "identify the problem. You can do so by calling:\n\n",
              paste0(model_name, "(", fun_params, ")"),
              call. = FALSE)
        }
      }
    )

    if (inherits(ts, "slendr_ts")) break
  }

  if (mutation_rate != 0)
    ts <- slendr::ts_mutate(ts, mutation_rate = mutation_rate)

  list(ts = ts, prior_values = unlist(prior_args))
}

collect_prior_matrix <- function(prior_values) {
  m <- matrix(prior_values, nrow = 1)
  colnames(m) <- names(prior_values)
  m
}

# This seems like a horrible hack but unless it turns out this is completely
# broken, it seems better to keep the ability to compare summary functions
# between ABC simulation runs rather than not. Either way, all functions *will*
# be identical between runs unless the user messes up in some way, so being
# potentially overly conservative here seems appropriate.
identical_functions <- function(run1_functions, run2_functions) {
  # get sources of both functions, stripping the address line
  run1_sources <- lapply(run1_functions, function(x) capture.output(print(x)) %>% .[-length(.)])
  run2_sources <- lapply(run2_functions, function(x) capture.output(print(x)) %>% .[-length(.)])
  identical(run1_sources, run2_sources)
}

# Generate a named list of prior samples to be used in model generating functions
generate_prior_args <- function(priors) {
  prior_samples <- lapply(seq_along(priors), function(i) sample_prior(priors[[i]]))

  prior_args <- lapply(prior_samples, `[[`, "value")
  names(prior_args) <- lapply(prior_samples, `[[`, "variable")

  prior_args
}
