# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it,
                          model, params, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          data, format, engine, model_args, engine_args,
                          model_name, attempts) {
  if (format == "files" && missing(data))
    stop("Models which generate custom files require a list of data function(s)\n",
         "which will process them for computation of summary statistics.", call. = FALSE)

  data_expr <- base::substitute(data)
  if (is.symbol(data_expr))
    data_expr <- data

  if (format == "ts")
    validate_user_functions(data_expr, valid_args = c("ts", "model"))
  else
    validate_user_functions(data_expr, valid_args = c("path", "model"))

  init_env(quiet = TRUE)
  result <- run_simulation(
    model = model, params = params, sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    engine = engine, model_args = model_args, engine_args = engine_args,
    format = format, model_name = model_name, attempts = attempts
  )
  result_data <- result$data
  result_params <- result$param_values

  if (format == "ts") {
    if (mutation_rate != 0)
      result_data <- slendr::ts_mutate(result_data, mutation_rate = mutation_rate)

    result_data <- list(ts = result_data)
  }

  # if user-defined generators were provided, apply each generator to the result
  if (!is.null(data_expr)) {
    env <- populate_data_env(result)
    result_data <- evaluate(data_expr, env)
  }

  # clean up if needed
  if (format == "ts")
    result_path <- attr(result_data, "path")
  else
    result_path <- result$data

  # collect data for a downstream ABC inference:
  #   1. compute summary statistics using user-defined tree-sequence functions
  simulated_stats <- summarise_data(result_data, functions)
  #   2. collect all parameter values (sampled from priors or given) into a single parameter matrix
  if (contains_priors(params)) {
    param_names <- names(result_params)
    result_params <- matrix(result_params, nrow = 1)
    colnames(result_params) <- param_names
  }

  if (!is.null(result_path))
    unlink(result_path, recursive = TRUE)

  list(
    parameters = result_params,
    simulated = simulated_stats
  )
}

# Run a single simulation replicate from a model with parameters modified by the
# prior distribution
run_simulation <- function(model, params, sequence_length, recombination_rate,
                           engine, model_args, engine_args, format,
                           model_name, attempts) {
  # only a well-defined slendr errors are allowed to be ignored during ABC simulations
  # (i.e. split time of a daughter population sampled from a prior at an older time than
  # its parent, etc.) -- such errors will simply lead to resampling, but all other errors
  # are considered real errors on the part of the user and will be reported as such
  errors <- c(
    # invalid split order implied by sampled split times
    "The model implies (backward|forward) time direction but the specified split\ntime .* is (higher|lower) than the parent's \\(\\d+\\)",
    # daughter population splitting *at the same time* as its parent (i.e. 1 vs 1.3 after rounding)
    "Population can be only created after its parent is already present in the simulation",
    # invalid gene-flow window
    "Specified times are not consistent with the assumed direction of\ntime",
    # gene-flow participants not existing
    "Both .* and .* must be already present within the gene-flow window",
    # population is created right at the moment the simulation is about to be finished
    "msprime._msprime.InputError: Input error in initialise: Attempt to sample a lineage from an inactive population",
    # sampling schedule itself provides times which are hard requirements for valid priors
    "A sampling event was scheduled outside of the simulation time window",
    "Cannot schedule sampling for '.*' at time \\d+",
    # population splits are specified in the right order, but the order is opposite to
    # what is forced by the user in `compile_model()`
    "The direction that was explicitly specified contradicts the direction implied by the model"
  )

  model_is_sampled <- contains_priors(params)
  model_is_parametrized <- is.data.frame(params)

  if (format == "ts")
    path <- NULL
  else {
    path <- paste0(tempdir(), "demografr_", sample.int(n = .Machine$integer.max, size = 1)) %>%
      normalizePath(winslash = "/", mustWork = FALSE)
    dir.create(path, showWarnings = FALSE)
  }

  n_tries <- 0
  repeat {
    if ((!is.null(attempts) && n_tries == attempts) && model_is_sampled)
      stop("\n\nGenerating a valid slendr model using the provided generation function\n",
           "and priors failed even after ", attempts, " repetitions. Please make sure\n",
           "that your model function can produce a valid model assuming the specified\n",
           "prior distributions.", call. = FALSE)

    n_tries <- n_tries + 1

    result <- tryCatch(
      {
        # sample model parameters from the prior or use the parameters as given
        if (model_is_sampled) {
          param_args <- generate_prior_args(params)
        } else if (model_is_parametrized) {
          param_args <- params[sample(seq_len(nrow(params)), 1), , drop = FALSE]
        } else
          param_args <- params

        # if a slendr model generating function is given as a model, generate a compiled model
        # from given parameters and simulate a tree sequence with a specified engine
        if (is.function(model)) {
          model_fun_args <- c(param_args, model_args)
          model_result <- do.call(model, model_fun_args)

          if (inherits(model_result, "slendr_model")) {
            compiled_model <- model_result
            sample_schedule <- NULL
          } else if (length(model_result) == 2) {
            compiled_model <- model_result[[1]]
            sample_schedule <- model_result[[2]]
          } else
            stop("Incorrect format of the returned result of the model function", call. = FALSE)

          engine <- get_engine(compiled_model, engine)

          # force no serialization for msprime runs
          if (engine == "msprime") compiled_model$path <- NULL

          # compose a list of required and optional arguments for msprime / SLiM engine
          engine_fun_args <- list(
            model = compiled_model,
            samples = sample_schedule
          ) %>% c(., engine_args, path = path)

          if (!missing(sequence_length))
            engine_fun_args <- c(engine_fun_args, sequence_length = sequence_length)
          if (!missing(recombination_rate))
            engine_fun_args <- c(engine_fun_args, recombination_rate = recombination_rate)

          engine_fun <- get(engine, envir = asNamespace("slendr"))

          # run a slendr simulation
          result <- do.call(engine_fun, engine_fun_args)

          # TODO: take care of this upstream now that the model path stores customized
          # user output files
          # # clean up if needed
          # if (!is.null(compiled_model$path))
          #   unlink(compiled_model$path, recursive = TRUE)
        } else { # a user-defined script was provided as a model
          compiled_model <- NULL

          # run a cutsom simulation
          model_engine_args <- c(model, c(engine_args, param_args, path = path))
          result <- do.call(run_script, model_engine_args)
        }
        result
      },
      error = function(cond) {
        msg <- conditionMessage(cond)

        # check that the received error is one of the valid, potentially expected slendr errors,
        # but only in situations where sampling from priors is used -- all parameters given as
        # a parameter grid must lead to a valid simulation, so any error is reported
        if ((model_is_sampled || model_is_parametrized) && any(vapply(errors, grepl, msg, FUN.VALUE = logical(1)))) {
          return(NULL)
        } else { # if an unexpected error occurred, report it in full
          cross <- " \u274C\n\n"
          if (is.function(model)) {
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
            stop(cross, "An unexpected error was raised when generating data from a slendr model\n",
                 "using the provided slendr function.\n\nThe error message received was:\n",
                 msg,
                 "\n\nPerhaps re-running the model function with the sampled parameters\n",
                 "(ideally also running the model by a respective simulation engine) will\n",
                 "help to identify the problem. You can do so by calling:\n\n",
                 paste0(model_name, "(", model_fun_params, ")"),
                 call. = FALSE)
          } else
            stop("Simulation via the provided custom script ended with the following error:\n\n",
                 msg, call. = FALSE)
        }
      }
    )

    if (!is.null(result)) break
  }

  list(data = result, param_values = unlist(param_args), model = compiled_model)
}

# Generate a named list of prior samples to be used in model generating functions
generate_prior_args <- function(priors) {
  prior_samples <- lapply(seq_along(priors), function(i) sample_prior(priors[[i]]))

  prior_args <- lapply(prior_samples, `[[`, "value")
  names(prior_args) <- lapply(prior_samples, `[[`, "variable")

  prior_args
}

# Run a custom tailored SLiM or Python (msprime) simulation script
#
# This function will take as input a path to a SLiM or Python script (detecting
# which one of the two it is based on the script's contents) and run it on the
# command line with model arguments formatted on the command line automatically.
run_script <- function(script, path, ...) {
  if (!file.exists(script))
    stop("No '", engine, "' script found at '", script, "'", call. = FALSE)
  else
    script <- normalizePath(script)

  # every single valid demografr SLiM script engine must contain the following
  # string -- this is how we can tell whether the input script is a SLiM script
  # or an msprime Python script
  script_contents <- readLines(script)
  if (any(grepl("initializeMutationRate", script_contents)))
    engine <- "slim"
  else
    engine <- reticulate::py_exe()

  args <- list(...)

  var_names <- names(args)
  var_values <- unname(args)
  # compose a vector of CLI arguments, either for a SLiM binary or for a Python script
  prefix <- if (engine == "slim") "-d " else "--"
  cli_args <- vapply(
    seq_along(var_names),
    function(i) paste0(prefix, var_names[i], "=", var_values[i]),
    character(1)
  )

  if (engine == "slim") { # run the SLiM script on the command line
    # add the generated output path to the list of CLI arguments for SLiM
    cli_args <- c(cli_args, paste0("-d \"path='", path, "'\""))

    # compose the whole CLI command
    cli_command <- paste(engine, paste(cli_args, collapse = " "), script, collapse = " ")

    # execute the command on the shell command line
    system(cli_command, intern = TRUE)
  } else { # run the msprime Python script in the reticulate'd Python interpreter
    # add the path to the output tree sequence and collapse the whole CLI command
    cli_args <- c(cli_args, paste0("--path=\"", path, "\""))

    # compose the whole CLI command
    cli_command <- paste(c(engine, script, cli_args), collapse = " ")

    # execute the command on the Python command line
    reticulate::py_run_string(sprintf("import os; os.system(r'%s')", cli_command))
  }

  if (dir(path) == 0)
    stop("The provided simulation script did not generate any files. Inspect the log\n",
         "information above for errors and make sure you can run your custom script \n",
         "on the command-line manually without any issues.\n\n",
         "The exact command that failed was:\n\n",
         cli_command, call. = FALSE)

  path
}
