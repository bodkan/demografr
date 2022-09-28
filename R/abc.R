run_simulation <- function(model, Ne_samples, sequence_length, recombination_rate) {
  # replace Ne values in the model object
  for (Ne in Ne_samples) {
    model$splits[model$splits$pop == Ne$variable, "N"] <- as.integer(Ne$value)
  }

  ts <- slendr::msprime(model, sequence_length = sequence_length, recombination_rate = recombination_rate)
  ts
}

run_iteration <- function(it, priors, functions, sequence_length, recombination_rate) {
  setup_env(quiet = TRUE)

  Ne_samples <- lapply(priors, sample_prior)

  ts <- run_simulation(model, Ne_samples, sequence_length, recombination_rate)
  sum_stats <- lapply(functions, function(f) f(ts))[[1]]

  list(
    parameters = sapply(Ne_samples, `[[`, "value"),
    summary_stats = sum_stats
  )
}

run_abc <- function(model, priors, functions, iterations = 1, epochs = 1, sequence_length = 1e6, recombination_rate = 1e-8) {
  # make sure that every population has an assigned prior on Ne
  # (this can be a proper prior backed by a sampling function or a fixed Ne value)
  model_populations <- model$splits$pop %>% sort
  prior_populations <- sort(prior_variables(priors))
  if (!all(model_populations == prior_populations))
    stop("Every population in the model needs a prior on Ne", call. = FALSE)

  results <- future.apply::future_lapply(
    seq_len(iterations),
    run_iteration,
    priors, functions,
    sequence_length, recombination_rate,
    future.seed = TRUE
  )

  Ne_parameters <- do.call(rbind, lapply(results, `[[`, "parameters"))
  colnames(Ne_parameters) <- prior_populations

  summary_stats <- do.call(rbind, lapply(results, `[[`, "summary_stats"))
  colnames(summary_stats) <- paste(names(functions), seq_len(ncol(summary_stats)), sep = "_")

  list(
    Ne_parameters = Ne_parameters,
    summary_stats = summary_stats
  )
}
