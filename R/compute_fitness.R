#' Compute the fitness of the given parameter set against observed data
#'
compute_fitness <- function(
    params, model, functions, observed,
    sequence_length, recombination_rate, mutation_rate = 0,
    engine = NULL, model_args = NULL, engine_args = NULL,
    statistics = FALSE
) {
  # if the parameter list is not named (such as is the case when this function
  # is run by the ga() routine), fill in the parameter names based on the names
  # of the model function's arguments
  if (is.null(names(params))) {
    param_names <- setdiff(get_nonimpl_params(model), names(model_args))
    params <- as.list(params)
    names(params) <- param_names
  }

  # detect if compute_fitness was called directly or by the inference loop
  # of the run_ga() routine to determine if the model output should be silenced,
  # to set the name of the model function
  non_interactive <- vapply(
    sys.calls(),
    function(call) length(call[[1]]) == 1 && all(deparse(call[[1]]) == "run_ga"),
    FUN.VALUE = logical(1)
  ) %>% any()

  if (non_interactive) {
    capture_message <- quiet
    model_name <- "user_model_function"
  } else {
    capture_message <- identity
    model_name <- as.character(substitute(model))
  }

  result <- tryCatch({
    capture_message(
      run_iteration(
        it = 1, model, params = params, functions = functions,
        sequence_length = sequence_length,
        recombination_rate = recombination_rate,
        mutation_rate = mutation_rate,
        engine = NULL, model_args = NULL, engine_args = NULL,
        attempts = 1, samples = NULL, model_name = model_name
      )
    )
    },
    error = function(e) {
      capture_message(cat(e$message, "\n"))
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
      # concatenate identifier columns of the statistics into a single name
      merged_stats$id <- apply(merged_stats[, shared_cols], 1, paste, collapse = "_")
      merged_stats$id <- paste0(stat, "_", merged_stats$id)
      merged_stats$stat <- stat
      # then remove the individual identifier columns (pop names, etc.)
      merged_stats[shared_cols] <- NULL
      names(merged_stats) <- c("simulated", "observed", "id", "stat")
      merged_stats <- merged_stats[c("stat", "id", "simulated", "observed")]
      merged_stats
    }) %>%
      do.call(rbind, .)

    merged$error <- 2 * abs(merged$simulated - merged$observed) / (abs(merged$simulated) + abs(merged$observed))

    error <- mean(merged$error)
  }

  # error = Inf (worse fit possible) => fitness = 0
  # error = 0   (best fit possible)  => fitness = 100
  fitness <- 1 / (1 + error)

  if (statistics)
    return(list(merged, fitness))
  else
    return(fitness)
}
