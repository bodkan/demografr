optim_fn <- function(par, model, observed) {
  # get all arguments of the model function...
  all_args <- names(formals(model))
  # ... from those, extract names of arguments which don't have an implicit value ...
  nonimpl_args <- all_args[vapply(all_args, function(x) is.name(formals(model)[[x]]), logical(1))]
  par <- as.list(par)
  names(par) <- nonimpl_args
  par <- as.data.frame(par)
  print(par)

  result <- tryCatch({
    simulate_grid(model, par, functions, replicates = 1,
                  sequence_length = 1e6, recombination_rate = 1e-8, mutation_rate = 1e-8)},
    error = function(e) NULL
  )

  if (is.null(result)) {
    # rmse <- Inf
    error <- Inf
  } else {
    # name the list of data frames with simulated statistics
    stat_names <- names(observed)
    simulated <- lapply(stat_names, function(x) result[[x]][[1]])
    names(simulated) <- stat_names

    simulated_col <- vapply(stat_names,
                            function(x) which(vapply(simulated[[x]], is.numeric, FUN.VALUE = logical(1))),
                            FUN.VALUE = integer(1))
    observed_col <- vapply(stat_names,
                           function(x) which(vapply(observed[[x]], is.numeric, FUN.VALUE = logical(1))),
                           FUN.VALUE = integer(1))

    merged <- lapply(stat_names, function(stat) {
      shared_cols <- colnames(observed[[stat]][, -observed_col[stat], drop = FALSE])
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
    errors <- 2 * abs(merged$simulated - merged$observed) / abs(merged$observed)
    error <- mean(errors)
  }

  # error = Inf (worse fit possible) => fitness = 0
  # error = 0   (best fit possible)  => fitness = 100
  fitness <- 1 / (1 + error)

  fitness
}
