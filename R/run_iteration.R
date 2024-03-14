# Get parameters from the priors, simulate a tree sequence, compute summary statistics
run_iteration <- function(it,
                          model, params, functions,
                          sequence_length, recombination_rate, mutation_rate,
                          samples, engine, model_args, engine_args,
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
  # i <- sample(x = 10000, size = 1)
  # write(ts$metadata$slendr$arguments$SEED, file = paste0(i, ".seed"))
  # write(param_values, file = paste0(i, ".params"))
  # then use this to investigate reasons for crashing statistics:
  # runs <- list.files(pattern = ".seed") %>% sapply(function(i) gsub(".seed", "", i))
  # res <- list()
  # for (i in runs) {
  #   random_seed <- scan(paste0(i, ".seed"), what = integer())
  #   param_values <- scan(paste0(i, ".params"), what = numeric()) %>% as.list() %>% setNames(c("admixture_time", "admixture_rate"))
  #
  #   ts <- simulate_ts(model, params, random_seed = random_seed)
  #
  #   simulated_stats <- lapply(functions, function(f) f(ts))
  #
  #   x <- list(
  #     parameters = param_values,
  #     simulated = simulated_stats
  #   )
  #   res[[length(res) + 1]] <- x
  # }

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
