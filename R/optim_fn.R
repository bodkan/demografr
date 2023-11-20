# GA fitness function
optim_fn <- function(par, model, functions, observed,
                     sequence_length, recombination_rate, mutation_rate,
                     engine, model_args, engine_args,
                     model_name, param_names) {
  par <- as.list(par)
  names(par) <- param_names

  result <- tryCatch({
    quiet(run_iteration(it = 1, model, params = par, functions = functions,
                  sequence_length = sequence_length,
                  recombination_rate = recombination_rate,
                  mutation_rate = mutation_rate,
                  engine = NULL, model_args = NULL, engine_args = NULL,
                  attempts = 1, samples = NULL, model_name = model_name))
    },
    error = function(e) {
      cat(e$message, "\n")
      NULL
    }
  )

  if (is.null(result)) { # consider simulation issues as Infinity errors
    error <- Inf
  } else {
    simulated <- result$simulated

    # examine which column of each statistic's data frame is numerical
    # (there can be only one)
    value_cols <- vapply(
      observed,
      function(df) which(vapply(df, is.numeric, FUN.VALUE = logical(1))),
      FUN.VALUE = integer(1)
    )

    # loop across all observed and simulated statistics' data frames, and join
    # them into a single table to compute observed-vs-simulated errors
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
    }) %>%
      do.call(rbind, .)

    merged$errors <- 2 * abs(merged$simulated - merged$observed) /
      (abs(merged$simulated) + abs(merged$observed))

    error <- mean(merged$errors)
  }

  # error = Inf (worse fit possible) => fitness = 0
  # error = 0   (best fit possible)  => fitness = 100
  fitness <- 1 / (1 + error)

  fitness
}
