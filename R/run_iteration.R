# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it,
                          model, params, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          data, format, engine, model_args, engine_args,
                          model_name, attempts) {
  if (format == "files" && missing(data_funs))
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
    model_name = model_name, engine = engine, samples = samples,
    model_args = model_args, engine_args = engine_args, format = format,
    attempts = attempts
  )
  result_data <- result$data
  result_params <- result$param_values

  if (format == "ts") {
    if (mutation_rate != 0)
      result_data <- ts = slendr::ts_mutate(result_data, mutation_rate = mutation_rate)

    # clean up if needed
    if (!is.null(attr(result_data, "path")))
      unlink(attr(result_data, "path"))

    result_data <- list(ts = result_data)
  }

  # if user-defined generators were provided, apply each generator to the result
  if (!is.null(data_expr)) {
    env <- populate_data_env(result)
    result_data <- evaluate_functions(data_expr, env)
  }

  # collect data for a downstream ABC inference:
  #   1. compute summary statistics using user-defined tree-sequence functions
  simulated_stats <- summarise_data(result_data, functions)
  #   2. collect all parameter values (sampled from priors or given) into a single parameter matrix
  if (contains_priors(params))
    result_params <- collect_param_matrix(result_params)

  list(
    parameters = result_params,
    simulated = simulated_stats
  )
}
