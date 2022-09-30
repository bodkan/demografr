run_simulation <- function(model, Ne_samples, sequence_length, recombination_rate) {
  # replace Ne values in the model object
  for (Ne in Ne_samples) {
    model$splits[model$splits$pop == Ne$variable, "N"] <- as.integer(Ne$value)
  }

  ts <- slendr::msprime(model, sequence_length = sequence_length, recombination_rate = recombination_rate)
  ts
}

run_iteration <- function(it, priors, functions, mutation_rate, sequence_length, recombination_rate) {
  setup_env(quiet = TRUE)

  Ne_samples <- lapply(priors, sample_prior)

  ts <- run_simulation(model, Ne_samples, sequence_length, recombination_rate)
  if (mutation_rate != 0)
    ts <- ts_mutate(ts, mutation_rate = mutation_rate)

  simulated_stats <- lapply(functions, function(f) f(ts))

  Ne_values <- matrix(sapply(Ne_samples, `[[`, "value"), nrow = 1)
  colnames(Ne_values) <- paste0("N_", as.character(lapply(Ne_samples, `[[`, "variable")))

  list(
    parameters = Ne_values,
    simulated_stats = simulated_stats
  )
}

simulate_abc <- function(
  model, priors, summary_funs, observed_stats,
  iterations = 1, epochs = 1,
  mutation_rate = 0, sequence_length = 10e6, recombination_rate = 1e-8
) {
  if (length(setdiff(names(summary_funs), names(observed_stats))))
    stop("List of summary functions and observed statistics must have the same names",
         call. = FALSE)

  # results <- future.apply::future_lapply(
  results <- parallel::mclapply(
  # results <- lapply(
    X = seq_len(iterations),
    FUN = run_iteration,
    priors = priors,
    functions = functions,
    mutation_rate = mutation_rate,
    sequence_length = sequence_length,
    recombination_rate = recombination_rate,
    mc.cores = 10
    # future.seed = TRUE
  )

  parameters <- lapply(results, `[[`, "parameters") %>% do.call(rbind, .) %>% as.matrix
  simulated_stats <- lapply(results, `[[`, "simulated_stats")

  list(
    parameters = parameters,
    simulated_stats = simulated_stats,
    observed_stats = observed_stats,
    stats_names = names(summary_funs)
  )
}

perform_abc <- function(data, tolerance, method, ...) {
  parameters <- data$parameters

  observed_stats <- lapply(data$stats_names, function(stat) {
    df <- data$observed_stats[[stat]]
    values <- matrix(df[, 2, drop = TRUE], nrow = 1)
    colnames(values) <- df[, 1, drop = TRUE]
    values
  }) %>% do.call(cbind, .)

  simulated_stats <- lapply(data$stats_names, function(stat) do.call(
    rbind, lapply(data$simulated_stats, function(it) {
      df <- it[[stat]]
      values <- matrix(df[, 2, drop = TRUE], nrow = 1)
      colnames(values) <- df[, 1, drop = TRUE]
      values
    }))
  ) %>% do.call(cbind, .)

  abc(
    param = parameters,
    target = observed_stats,
    sumstat = simulated_stats,
    tol = tolerance,
    method = method,
    ...
  )
}

extract_model <- function(abc, model) {

}
