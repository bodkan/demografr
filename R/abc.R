#######################################################
# internal functions used for batch simulations in
# functions simulate_abc() and simulate_grid()
#######################################################

# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it,
                          model, params, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          engine, samples, model_args, engine_args,
                          model_name, attempts) {
  init_env(quiet = TRUE)

  sim_result <- run_simulation(
    model = model, params = params, sequence_length = sequence_length,
    recombination_rate = recombination_rate, mutation_rate = mutation_rate,
    model_name = model_name, engine = engine, samples = samples,
    model_args = model_args, engine_args = engine_args,
    attempts = attempts
  )
  ts <- sim_result$ts
  param_values <- sim_result$param_values

  # collect data for a downstream ABC inference:
  #   1. compute summary statistics using user-defined tree-sequence functions
  simulated_stats <- lapply(functions, function(f) f(ts))
  #   2. collect all parameter values (sampled from priors or given) into a single parameter matrix
  if (contains_priors(params))
    param_values <- collect_param_matrix(param_values)

  list(
    parameters = param_values,
    simulated = simulated_stats
  )
}

# Run a single simulation replicate from a model with parameters modified by the
# prior distribution
run_simulation <- function(model, params, sequence_length, recombination_rate, mutation_rate,
                           engine, samples, model_args, engine_args, model_name, attempts) {
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
    "Both .* and .* must be already present within the gene-flow window \\d+-\\d+",
    # population is created right at the moment the simulation is about to be finished
    "msprime._msprime.InputError: Input error in initialise: Attempt to sample a lineage from an inactive population",
    # sampling schedule itself provides times which are hard requirements for valid priors
    "A sampling event was scheduled outside of the simulation time window",
    "Cannot schedule sampling for '.*' at time \\d+"
  )

  model_is_sampled <- contains_priors(params)

  n_tries <- 0
  repeat {
    if (n_tries == attempts && model_is_sampled)
      stop("\n\nGenerating a valid slendr model using the provided generation function\n",
           "and priors failed even after ", attempts, " repetitions. Please make sure\n",
           "that your model function can produce a valid model assuming the specified\n",
           "prior distributions.", call. = FALSE)

    n_tries <- n_tries + 1

    ts <- tryCatch(
      {
        # sample model parameters from the prior or use the parameters as given
        if (model_is_sampled)
          param_args <- generate_prior_args(params)
        else
          param_args <- params

        # generate a compiled slendr model from a provided function
        model_fun_args <- c(param_args, model_args)

        model_result <- do.call(model, model_fun_args)

        if (inherits(model_result, "slendr_model")) {
          slendr_model <- model_result
          sample_schedule <- NULL
        } else if (length(model_result) == 2) {
          slendr_model <- model_result[[1]]
          sample_schedule <- model_result[[2]]
        } else
          stop("Incorrect format of the returned result of the model function", call. = FALSE)

        # compose a list of required and optional arguments for msprime / SLiM engine
        engine_fun_args <- list(
          model = slendr_model,
          sequence_length = sequence_length,
          recombination_rate = recombination_rate,
          samples = sample_schedule
        ) %>% c(., engine_args)

        # simulate a tree sequence
        do.call(engine, engine_fun_args)
      },
      error = function(cond) {
        msg <- conditionMessage(cond)

        # check that the received error is one of the valid, potentially expected slendr errors,
        # but only in situations where sampling from priors is used -- all parameters given as
        # a parameter grid must lead to a valid simulation, so any error is reported
        if (model_is_sampled && any(vapply(errors, grepl, msg, FUN.VALUE = logical(1)))) {
          return(NULL)
        } else { # if an unexpected error ocurred, report it in full
          cat(" \u274C\n\n")
          # compose parameters for the complete model function call
          # (i.e. priors and non-prior arguments to the model generating function)
          model_fun_params <- paste(
            vapply(names(model_fun_args),
                  function(x) sprintf("%s = %s", x, ifelse(is.numeric(model_fun_args[[x]]),
                                                          model_fun_args[[x]],
                                                          sprintf("\"%s\"", model_fun_args[[x]]))),
                  FUN.VALUE = character(1)),
            collapse = ", "
          )
          stop("An unexpected error was raised when generating data from a slendr model\n",
              "using the provided slendr function.\n\nThe error message received was:\n",
              msg,
              "\n\nPerhaps re-running the model function with the sampled parameters will\n",
              "identify the problem. You can do so by calling:\n\n",
              paste0(model_name, "(", model_fun_params, ")"),
              call. = FALSE)
        }
      }
    )

    if (inherits(ts, "slendr_ts")) break
  }

  if (mutation_rate != 0)
    ts <- slendr::ts_mutate(ts, mutation_rate = mutation_rate)

  list(ts = ts, param_values = unlist(param_args))
}

collect_param_matrix <- function(prior_values) {
  m <- matrix(prior_values, nrow = 1)
  colnames(m) <- names(prior_values)
  m
}

# Generate a named list of prior samples to be used in model generating functions
generate_prior_args <- function(priors) {
  prior_samples <- lapply(seq_along(priors), function(i) sample_prior(priors[[i]]))

  prior_args <- lapply(prior_samples, `[[`, "value")
  names(prior_args) <- lapply(prior_samples, `[[`, "variable")

  prior_args
}

# Does the given list contain prior sampling formulas?
contains_priors <- function(l) {
  all(vapply(l, function(f) inherits(f, "formula"), logical(1)))
}