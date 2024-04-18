# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it,
                          model, params, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          engine, model_args, engine_args,
                          model_name, attempts) {
  init_env(quiet = TRUE)

  sim_result <- run_simulation(
    model = model, params = params, sequence_length = sequence_length,
    recombination_rate = recombination_rate, mutation_rate = mutation_rate,
    model_name = model_name, engine = engine, samples = samples,
    model_args = model_args, engine_args = engine_args,
    attempts = attempts
  )
  output <- sim_result$output
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
