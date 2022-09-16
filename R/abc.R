run_simulation <- function(model, Ne_samples, sequence_length, recombination_rate) {
  # replace Ne values in the model object
  for (Ne in Ne_samples) {
    model$splits[model$splits$pop == Ne$variable, "N"] <- as.integer(Ne$value)
  }

  ts <- slendr::msprime(model, sequence_length = sequence_length, recombination_rate = recombination_rate)
  ts
}

run_iteration <- function(it, priors, functions) {
  Ne_samples <- lapply(priors, sample_prior)

  ts <- run_simulation(model, Ne_samples, sequence_length = 1e6, recombination_rate = 1e-8)
  sum_stats <- lapply(functions, function(f) f(ts))[[1]]

  list(
    parameters = sapply(Ne_samples, `[[`, "value"),
    summary_stats = sum_stats
  )
}

run_abc <- function(model, priors, functions, iterations = 1, epochs = 1) {
  # make sure that every population has an assigned prior on Ne
  model_populations <- model$splits$pop %>% sort
  prior_populations <- sapply(priors, function(p) as.character(as.list(p)[[2]])) %>% sort
  if (!all(model_populations == prior_populations))
    stop("Every population in the model needs a prior on Ne", call. = FALSE)

  abc_results <- future.apply::future_lapply(
    seq_len(iterations),
    run_iteration,
    priors, functions
  )

  list(
    parameters = do.call(rbind, lapply(abc_results, `[[`, "parameters")),
    summary_stats = do.call(rbind, lapply(abc_results, `[[`, "summary_stats"))
  )
}
