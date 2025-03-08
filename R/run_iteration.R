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

warning(paste(c(reticulate::conda_list()$name,
	     "###",
	     normalizePath(reticulate::py_exe(), winslash = "/"),
	     "###"), collapse="\n"), call. = FALSE)
  init_env(quiet = TRUE)
warning("@@@",  normalizePath(reticulate::py_exe(), winslash = "/"), "@@@", call. = FALSE)
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
  if (contains_priors(params))
    result_params <- collect_param_matrix(result_params)

  if (!is.null(result_path))
    unlink(result_path, recursive = TRUE)

  list(
    parameters = result_params,
    simulated = simulated_stats
  )
}
