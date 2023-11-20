optim_fn <- function(par, model, functions, observed) {
  # get all arguments of the model function...
  all_args <- names(formals(model))
  # ... from those, extract names of arguments which don't have an implicit value ...
  nonimpl_args <- all_args[vapply(all_args, function(x) is.name(formals(model)[[x]]), logical(1))]
  par <- as.list(par)
  names(par) <- nonimpl_args
  par <- as.data.frame(par) # TODO: this is no longer necessary as run_iteration accepts a list
  # print(par)

  result <- tryCatch({
    # TODO: figure out a way to get the model_name just like simulate_grid does it
    quiet(run_iteration(it = 1, model, params = par, functions = functions,
                  sequence_length = 25e6, recombination_rate = 1e-8, mutation_rate = 1e-8,
                  attempts = 1, NULL, engine = NULL, model_args = NULL, engine_args = NULL,
                  model_name = "asdf"))
    },
    error = function(e) {
      # cat(e$message, "\n")
      NULL
    }
  )

  if (is.null(result)) {
    # rmse <- Inf
    error <- Inf
  } else {
    simulated <- result$simulated

    value_cols <- vapply(
      observed,
      function(df) which(vapply(df, is.numeric, FUN.VALUE = logical(1))),
      FUN.VALUE = integer(1)
    )

    merged <- lapply(names(observed), function(stat) {
      shared_cols <- colnames(observed[[stat]][, -value_cols[stat], drop = FALSE])
      merged_stats <- dplyr::inner_join(
        simulated[[stat]], observed[[stat]],
        by = shared_cols
      )
      merged_stats[shared_cols] <- NULL
      merged_stats$stat <- stat
      names(merged_stats) <- c("simulated", "observed", "stat")
      merged_stats
      # merged_stats %>%
      #   dplyr::mutate(
      #     norm_sim = (simulated - mean(simulated)) / sd(simulated),
      #     norm_obs = (observed - mean(observed)) / sd(observed)
      #   )
    }) %>%
      do.call(rbind, .)

      # browser()
    # rmse <- sqrt(mean((merged$norm_sim - merged$norm_obs)^2))
    errors <- 2 * abs(merged$simulated - merged$observed) / (abs(merged$simulated) + abs(merged$observed))
    error <- mean(errors)
  }

  # error = Inf (worse fit possible) => fitness = 0
  # error = 0   (best fit possible)  => fitness = 100
  fitness <- 1 / (1 + error)

  fitness
}
